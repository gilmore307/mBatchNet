# ===============================
# File: _4_upload.py
# ===============================
from typing import List, Tuple, Dict, Optional
from pathlib import Path
import csv
import math
import time
from datetime import datetime
from io import BytesIO
import shutil
import zipfile
import json
from urllib.parse import parse_qs
from dash import dcc, html
from dash.dependencies import Input, Output, State
import dash
import dash_bootstrap_components as dbc

from _1_components import build_navbar
from _2_utils import (
    decode_contents,
    get_session_dir,
    save_uploaded_file,
    human_size,
    run_preprocess,
    run_r_scripts,
    log_file_meta,
    BASE_DIR,
    _make_ag_grid,
    _encode_image_source,
    clear_session_derived_outputs,
    CODE_TO_DISPLAY,
    COUNT_REQUIRED_METHOD_CODES,
    METHOD_INPUT_REQUIREMENT_BY_CODE,
    SUPPORTED_METHODS,
)


EXAMPLE_DIR = BASE_DIR / "assets" / "example"
PREVIEW_MAX_ROWS = 5
PREVIEW_MAX_COLS = 50
MAX_UPLOAD_BYTES = 10 * 1024 * 1024
MAX_REPRO_BUNDLE_BYTES = 25 * 1024 * 1024
WARN_UPLOAD_BYTES = 3 * 1024 * 1024
MAX_SAMPLES = 500
MAX_FEATURES = 1000
MAX_MATRIX_CELLS = MAX_SAMPLES * MAX_FEATURES
MAX_METADATA_COLUMNS = 5
WARN_MATRIX_CELLS = 500 * 500
HIGH_SPARSITY_FRACTION = 0.80
STRONG_CONFOUNDING_V = 0.60
OUTLIER_MAD_MULTIPLIER = 5.0
COUNT_INTEGER_TOLERANCE = 1e-6

# Mapping presets for example datasets (case-insensitive keys)
EXAMPLE_COLUMN_MAP: Dict[str, Dict[str, object]] = {
    "ad": {
        "batch": "Batch",
        "target": "Initial Phenol Concentration",
        "covariates": ["Treatment Duration"],
    },
}


def _blank(value: object) -> bool:
    return value is None or str(value).strip() == ""


def _metadata_missing(value: object) -> bool:
    if _blank(value):
        return True
    token = str(value).strip().lower()
    return token in {"na", "n/a", "nan", "null", "none", "inf", "+inf", "-inf"}


def _as_float(value: object) -> Optional[float]:
    if _blank(value):
        return None
    try:
        val = float(str(value).strip())
    except (TypeError, ValueError):
        return None
    if val != val or val in (float("inf"), float("-inf")):
        return None
    return val


def _read_csv_records(path: Path) -> Tuple[List[str], List[Dict[str, str]]]:
    with path.open("r", encoding="utf-8", newline="") as fh:
        reader = csv.DictReader(fh)
        header = [str(col).strip() for col in (reader.fieldnames or []) if str(col).strip()]
        rows = [dict(row) for row in reader]
    return header, rows


def _read_numeric_matrix(path: Path) -> Tuple[List[List[float]], Dict[str, object]]:
    rows: List[List[str]] = []
    with path.open("r", encoding="utf-8", newline="") as fh:
        reader = csv.reader(fh)
        rows = [[cell.strip() for cell in row] for row in reader if any(not _blank(cell) for cell in row)]
    if not rows:
        return [], {"header_detected": False, "sample_id_column": False}

    header_detected = False
    sample_id_column = False

    def numeric_fraction(cells: List[str]) -> float:
        if not cells:
            return 0.0
        return sum(_as_float(cell) is not None for cell in cells) / len(cells)

    if numeric_fraction(rows[0]) < 0.75 and len(rows) > 1 and numeric_fraction(rows[1]) >= 0.75:
        header_detected = True
        rows = rows[1:]

    if rows and all(_as_float(row[0]) is None and numeric_fraction(row[1:]) >= 0.75 for row in rows if row):
        sample_id_column = True
        rows = [row[1:] for row in rows]

    width = max((len(row) for row in rows), default=0)
    matrix: List[List[float]] = []
    for row in rows:
        padded = row + [""] * (width - len(row))
        parsed: List[float] = []
        for cell in padded:
            value = _as_float(cell)
            if value is None:
                raise ValueError("Matrix contains blank, non-numeric, NA, NaN, or Inf values.")
            parsed.append(value)
        matrix.append(parsed)
    return matrix, {"header_detected": header_detected, "sample_id_column": sample_id_column}


def _level_counts(rows: List[Dict[str, str]], column: str) -> Dict[str, int]:
    counts: Dict[str, int] = {}
    for row in rows:
        value = str(row.get(column, "")).strip()
        if value:
            counts[value] = counts.get(value, 0) + 1
    return counts


def _first_non_empty_level(rows: List[Dict[str, str]], column: Optional[str]) -> Optional[str]:
    if not column:
        return None
    for row in rows:
        value = str(row.get(column, "")).strip()
        if value:
            return value
    return None


def _cramers_v(rows: List[Dict[str, str]], col_a: str, col_b: str) -> Optional[float]:
    levels_a = sorted(_level_counts(rows, col_a))
    levels_b = sorted(_level_counts(rows, col_b))
    if len(levels_a) < 2 or len(levels_b) < 2:
        return None
    table = [[0 for _ in levels_b] for _ in levels_a]
    idx_a = {value: idx for idx, value in enumerate(levels_a)}
    idx_b = {value: idx for idx, value in enumerate(levels_b)}
    n = 0
    for row in rows:
        a = str(row.get(col_a, "")).strip()
        b = str(row.get(col_b, "")).strip()
        if not a or not b:
            continue
        table[idx_a[a]][idx_b[b]] += 1
        n += 1
    if n == 0:
        return None
    row_totals = [sum(row) for row in table]
    col_totals = [sum(table[i][j] for i in range(len(levels_a))) for j in range(len(levels_b))]
    chi2 = 0.0
    for i, row_total in enumerate(row_totals):
        for j, col_total in enumerate(col_totals):
            expected = row_total * col_total / n if n else 0
            if expected > 0:
                chi2 += ((table[i][j] - expected) ** 2) / expected
    denom = n * max(1, min(len(levels_a) - 1, len(levels_b) - 1))
    return (chi2 / denom) ** 0.5 if denom else None


def _fabatch_retained_feature_count(matrix: List[List[float]]) -> Optional[int]:
    if not matrix or not matrix[0]:
        return None
    n_rows = len(matrix)
    n_cols = len(matrix[0])
    if n_rows < 2 or n_cols < 1:
        return None

    has_negative = any(value < 0 for row in matrix for value in row)
    transformed: List[List[float]] = []
    if has_negative:
        transformed = [list(row) for row in matrix]
    else:
        tss_rows: List[List[float]] = []
        positive_values: List[float] = []
        for row in matrix:
            clean = [value if value > 0 else 0.0 for value in row]
            row_sum = sum(clean) or 1.0
            tss = [value / row_sum for value in clean]
            tss_rows.append(tss)
            positive_values.extend(value for value in tss if value > 0)
        if not positive_values:
            return 0
        eps = max(min(positive_values) * 0.65, 1e-6)
        transformed = [[math.log(value if value > 0 else eps) for value in row] for row in tss_rows]

    retained = 0
    for col in range(n_cols):
        values = [row[col] for row in transformed]
        mean_val = sum(values) / n_rows
        variance = sum((value - mean_val) ** 2 for value in values) / max(1, n_rows - 1)
        if variance > 1e-12:
            retained += 1
    return retained


def _is_count_like_matrix(matrix: List[List[float]]) -> bool:
    values = [value for row in matrix for value in row]
    if not values:
        return False
    return all(
        math.isfinite(value)
        and value >= 0
        and abs(value - round(value)) <= COUNT_INTEGER_TOLERANCE
        for value in values
    )


def _build_method_availability(matrix: List[List[float]]) -> Dict[str, Dict[str, object]]:
    count_like = _is_count_like_matrix(matrix)
    availability: Dict[str, Dict[str, object]] = {}
    count_reason = (
        "Requires nonnegative integer count input; this upload contains continuous, transformed, "
        "negative, or otherwise non-count matrix values."
    )
    for code, display in SUPPORTED_METHODS:
        requirement = METHOD_INPUT_REQUIREMENT_BY_CODE.get(code, {})
        requires_counts = code in COUNT_REQUIRED_METHOD_CODES
        entry: Dict[str, object] = {
            "display_name": display,
            "available": bool(count_like or not requires_counts),
            "requirement": requirement.get("requirement", "continuous_or_discrete"),
            "requirement_label": requirement.get("label", "Accepts numeric input"),
        }
        if requires_counts and not count_like:
            entry["reason"] = count_reason
        availability[code] = entry
    return availability


def _set_method_unavailable(
    availability: Dict[str, Dict[str, object]],
    code: str,
    reason: str,
) -> None:
    requirement = METHOD_INPUT_REQUIREMENT_BY_CODE.get(code, {})
    entry = availability.setdefault(
        code,
        {
            "display_name": CODE_TO_DISPLAY.get(code, code),
            "requirement": requirement.get("requirement", "method_specific"),
            "requirement_label": requirement.get("label", "Method-specific requirement"),
        },
    )
    entry["available"] = False
    entry["reason"] = reason


def _unavailable_method_names(availability: Dict[str, Dict[str, object]], codes: List[str]) -> List[str]:
    names = []
    for code in codes:
        entry = availability.get(code, {})
        if entry.get("available") is False:
            names.append(str(entry.get("display_name") or CODE_TO_DISPLAY.get(code, code)))
    return names


def _mad_outlier_count(values: List[float]) -> int:
    if len(values) < 4:
        return 0

    import numpy as np

    arr = np.asarray(values, dtype=float)
    arr = arr[np.isfinite(arr)]
    if arr.size < 4:
        return 0

    median = float(np.median(arr))
    mad = float(np.median(np.abs(arr - median)))
    if mad <= 0:
        if median <= 0:
            return 0
        upper = median * 10
        return int(np.sum(arr > upper))

    lower = median - OUTLIER_MAD_MULTIPLIER * mad
    upper = median + OUTLIER_MAD_MULTIPLIER * mad
    return int(np.sum((arr < lower) | (arr > upper)))


def _scanpy_outlier_counts(matrix: List[List[float]]) -> Tuple[int, int]:
    import numpy as np
    import scanpy as sc
    from anndata import AnnData

    adata = AnnData(X=np.asarray(matrix, dtype=float))
    obs_metrics, _ = sc.pp.calculate_qc_metrics(
        adata,
        expr_type="abundance",
        var_type="features",
        qc_vars=(),
        percent_top=None,
        inplace=False,
        log1p=False,
    )
    sample_outliers = _mad_outlier_count(obs_metrics["total_abundance"].tolist())
    value_outliers = _mad_outlier_count([value for row in matrix for value in row])
    return sample_outliers, value_outliers


def _write_validation_report(session_dir: Path, report: Dict[str, object]) -> None:
    try:
        (session_dir / "validation_report.json").write_text(
            json.dumps(report, indent=2, ensure_ascii=False),
            encoding="utf-8",
        )
    except OSError:
        pass


def _build_validation_report(
    errors: List[str],
    warnings: List[str],
    dimensions: Dict[str, object],
    started_at: float,
    method_availability: Optional[Dict[str, Dict[str, object]]] = None,
) -> Dict[str, object]:
    report = {
        "valid": not errors,
        "validated_at": datetime.now().isoformat(timespec="seconds"),
        "validation_elapsed_sec": round(time.perf_counter() - started_at, 6),
        "errors": errors,
        "warnings": warnings,
        "dimensions": dimensions,
        "limits": {
            "max_upload_bytes": MAX_UPLOAD_BYTES,
            "max_samples": MAX_SAMPLES,
            "max_features": MAX_FEATURES,
            "max_matrix_cells": MAX_MATRIX_CELLS,
            "max_metadata_columns": MAX_METADATA_COLUMNS,
        },
        "input_contract": "sample-feature numeric matrix: rows are samples and columns are profiled features",
    }
    if method_availability is not None:
        report["method_availability"] = method_availability
    return report


def validate_session_inputs(
    session_dir: Path,
    *,
    batch_col: Optional[str] = None,
    target_col: Optional[str] = None,
) -> Dict[str, object]:
    """Validate the server-side upload contract before running R preprocessing."""

    started_at = time.perf_counter()
    raw_path = session_dir / "raw.csv"
    meta_path = session_dir / "metadata_origin.csv"
    errors: List[str] = []
    warnings: List[str] = []
    dimensions: Dict[str, object] = {}
    method_availability: Optional[Dict[str, Dict[str, object]]] = None

    if not raw_path.exists():
        errors.append("Missing raw.csv.")
    if not meta_path.exists():
        errors.append("Missing metadata_origin.csv.")
    if errors:
        report = _build_validation_report(errors, warnings, dimensions, started_at)
        _write_validation_report(session_dir, report)
        return report

    raw_size = raw_path.stat().st_size
    meta_size = meta_path.stat().st_size
    dimensions["raw_file_bytes"] = raw_size
    dimensions["metadata_file_bytes"] = meta_size
    if raw_size > MAX_UPLOAD_BYTES or meta_size > MAX_UPLOAD_BYTES:
        errors.append(f"CSV files must be {human_size(MAX_UPLOAD_BYTES)} or smaller for the public server.")
    elif raw_size > WARN_UPLOAD_BYTES or meta_size > WARN_UPLOAD_BYTES:
        warnings.append("Large CSV detected; correction methods may take several minutes or fail on the public server.")

    matrix: List[List[float]] = []
    matrix_meta: Dict[str, object] = {}
    try:
        matrix, matrix_meta = _read_numeric_matrix(raw_path)
    except Exception as exc:
        errors.append(str(exc))
    if matrix:
        sample_count = len(matrix)
        feature_count = len(matrix[0]) if matrix else 0
        method_availability = _build_method_availability(matrix)
        dimensions["count_like_matrix"] = _is_count_like_matrix(matrix)
        unavailable_count_methods = _unavailable_method_names(method_availability, list(COUNT_REQUIRED_METHOD_CODES))
        if unavailable_count_methods:
            warnings.append(
                "Method availability warning: "
                f"{', '.join(unavailable_count_methods)} disabled for this session because they require "
                "nonnegative integer count input; continuous, transformed, negative, or non-count matrices "
                "can still use methods that accept continuous numeric input."
            )
        dimensions.update(
            {
                "samples": sample_count,
                "features": feature_count,
                "matrix_cells": sample_count * feature_count,
                **matrix_meta,
            }
        )
        if sample_count < 2 or feature_count < 2:
            errors.append("Matrix must contain at least 2 samples and 2 features.")
        if sample_count > MAX_SAMPLES or feature_count > MAX_FEATURES or sample_count * feature_count > MAX_MATRIX_CELLS:
            errors.append(
                f"Matrix exceeds public-server limits: {MAX_SAMPLES} samples, "
                f"{MAX_FEATURES} features, or {MAX_MATRIX_CELLS:,} cells."
            )
        elif sample_count * feature_count > WARN_MATRIX_CELLS:
            warnings.append(
                f"Large matrix detected (> {WARN_MATRIX_CELLS:,} cells, about 500 x 500); "
                "correction methods may run slowly. Consider running a smaller method set first "
                "and downloading results frequently."
            )
        zero_rows = sum(1 for row in matrix if all(value == 0 for value in row))
        zero_cols = 0
        if feature_count:
            zero_cols = sum(1 for col in range(feature_count) if all(row[col] == 0 for row in matrix))
        if zero_rows:
            errors.append(f"Matrix contains {zero_rows} all-zero sample row(s).")
        if zero_cols:
            warnings.append(f"Matrix contains {zero_cols} all-zero feature column(s); these features add runtime only.")
        total_values = sample_count * feature_count
        zero_values = sum(1 for row in matrix for value in row if value == 0)
        if total_values and zero_values / total_values >= HIGH_SPARSITY_FRACTION:
            warnings.append("Matrix is highly sparse; review count assumptions and transformed-data assumptions before selecting correction methods.")
        negative_values = sum(1 for row in matrix for value in row if value < 0)
        if negative_values:
            warnings.append("Negative values detected; this looks transformed. Count-specific methods may be inappropriate.")
        sample_outliers, value_outliers = _scanpy_outlier_counts(matrix)
        if sample_outliers or value_outliers:
            dimensions["outlier_sample_total_count"] = sample_outliers
            dimensions["outlier_matrix_value_count"] = value_outliers
            warnings.append(
                "Outlier detection warning: "
                f"{sample_outliers} sample total(s) and {value_outliers} matrix value(s) exceed the "
                f"Scanpy QC {OUTLIER_MAD_MULTIPLIER:.1f}x MAD screening rule. Review input scaling and extreme samples "
                "before running correction methods."
            )

    metadata_header: List[str] = []
    metadata_rows: List[Dict[str, str]] = []
    try:
        metadata_header, metadata_rows = _read_csv_records(meta_path)
    except Exception as exc:
        errors.append(f"Could not read metadata_origin.csv: {exc}")
    dimensions["metadata_rows"] = len(metadata_rows)
    dimensions["metadata_columns"] = len(metadata_header)
    if len(metadata_header) > MAX_METADATA_COLUMNS:
        errors.append(f"Metadata must contain {MAX_METADATA_COLUMNS} columns or fewer for the public server.")
    if matrix and metadata_rows and len(metadata_rows) != len(matrix):
        errors.append(
            "Metadata row count must match matrix sample rows "
            f"({len(metadata_rows)} metadata rows vs {len(matrix)} matrix rows)."
        )
    if not metadata_rows or not metadata_header:
        errors.append("Metadata must include a header and at least one data row.")
    if metadata_header and metadata_rows:
        for column in metadata_header:
            missing_rows = [
                row_idx
                for row_idx, row in enumerate(metadata_rows, start=2)
                if _metadata_missing(row.get(column))
            ]
            if missing_rows:
                preview = ", ".join(str(row_idx) for row_idx in missing_rows[:5])
                suffix = "..." if len(missing_rows) > 5 else ""
                errors.append(
                    f"Metadata column '{column}' contains blank or NA-like value(s) "
                    f"at CSV row(s) {preview}{suffix}."
                )
    if batch_col:
        if batch_col not in metadata_header:
            errors.append(f"Selected batch column '{batch_col}' is missing from metadata.")
        else:
            batch_counts = _level_counts(metadata_rows, batch_col)
            dimensions["batch_levels"] = len(batch_counts)
            if batch_counts:
                dimensions["max_batch_size"] = max(batch_counts.values())
            if len(batch_counts) < 2:
                errors.append("Batch column must contain at least two non-empty levels.")
    if target_col:
        if target_col not in metadata_header:
            errors.append(f"Selected target column '{target_col}' is missing from metadata.")
        else:
            target_counts = _level_counts(metadata_rows, target_col)
            dimensions["target_levels"] = len(target_counts)
            if len(target_counts) != 2:
                errors.append("Target column must contain exactly two non-empty levels for current binary assessments.")
    if batch_col and target_col and batch_col == target_col:
        errors.append("Batch and target columns must be different.")
    if batch_col and target_col and batch_col in metadata_header and target_col in metadata_header:
        v = _cramers_v(metadata_rows, batch_col, target_col)
        if v is not None:
            dimensions["batch_target_cramers_v"] = round(v, 4)
            if v >= STRONG_CONFOUNDING_V:
                warnings.append(
                    f"Batch and target are strongly associated (Cramer's V = {v:.2f}; "
                    f"advisory warning threshold = {STRONG_CONFOUNDING_V:.2f}). In the current "
                    "implementation, Cramer's V >= 0.60 triggers this batch-target association "
                    "warning. This is an "
                    "effect-size warning for study-design imbalance, not a formal hypothesis test. "
                    "Interpret correction results carefully because removing batch-associated structure "
                    "may also affect target-associated signal. Use the mosaic plot after study-setting "
                    "confirmation to inspect the batch-target composition visually."
                )
    if matrix and batch_col and batch_col in metadata_header:
        batch_counts = _level_counts(metadata_rows, batch_col)
        max_batch_size = max(batch_counts.values()) if batch_counts else None
        retained = _fabatch_retained_feature_count(matrix)
        if retained is not None and max_batch_size is not None:
            dimensions["fabatch_retained_features"] = retained
            dimensions["fabatch_max_batch_size"] = max_batch_size
            dimensions["fabatch_available"] = retained > max_batch_size
            if retained <= max_batch_size:
                reason = (
                    "FAbatch is unavailable for this input because retained feature count after low-variance filtering "
                    f"({retained}) is not greater than the largest batch size ({max_batch_size})."
                )
                warnings.append(reason)
                if method_availability is not None:
                    _set_method_unavailable(method_availability, "FAbatch", reason)

    report = _build_validation_report(errors, warnings, dimensions, started_at, method_availability)
    _write_validation_report(session_dir, report)
    return report


def _render_validation_report(report: Dict[str, object]) -> object:
    errors = list(report.get("errors") or [])
    warnings = list(report.get("warnings") or [])
    dimensions = report.get("dimensions") if isinstance(report.get("dimensions"), dict) else {}
    dim_text = []
    if dimensions.get("samples") and dimensions.get("features"):
        dim_text.append(f"{dimensions.get('samples')} samples x {dimensions.get('features')} features")
    if dimensions.get("metadata_rows"):
        dim_text.append(f"{dimensions.get('metadata_rows')} metadata rows")
    body: List[object] = []
    if dim_text:
        body.append(html.Div("Detected: " + "; ".join(dim_text), className="mb-2"))
    if errors:
        body.append(html.Ul([html.Li(msg) for msg in errors], className="mb-0"))
        return dbc.Alert(body, color="danger", className="mt-3")
    if warnings:
        body.append(html.Ul([html.Li(msg) for msg in warnings], className="mb-0"))
        return dbc.Alert(body, color="warning", className="mt-3")
    body.append(html.Div("Input validation passed.", className="mb-0"))
    return dbc.Alert(body, color="success", className="mt-3")


REPRO_BUNDLE_FILES = {
    "raw.csv",
    "metadata_origin.csv",
    "metadata.csv",
    "raw_tss.csv",
    "raw_clr.csv",
    "session_config.json",
    "validation_report.json",
    "parameter_manifest.json",
    "runtime_summary.json",
    "reproducibility_manifest.json",
    "session_summary.json",
    "execution_commands.sh",
    "run.log",
}


def _restore_repro_bundle(contents: str, session_dir: Path) -> Tuple[bool, object, bool]:
    """Restore a reproducibility bundle zip into the current session directory."""

    try:
        data = decode_contents(contents)
    except Exception:
        return False, dbc.Alert("Could not decode uploaded bundle.", color="danger"), False
    if len(data) > MAX_REPRO_BUNDLE_BYTES:
        return (
            False,
            dbc.Alert(
                f"Bundle is too large for this server. Maximum bundle size is {human_size(MAX_REPRO_BUNDLE_BYTES)}.",
                color="danger",
            ),
            False,
        )

    restored: List[str] = []
    skipped: List[str] = []
    try:
        with zipfile.ZipFile(BytesIO(data)) as zf:
            allowed_infos = []
            for info in zf.infolist():
                if info.is_dir():
                    continue
                source_path = Path(info.filename)
                if source_path.is_absolute() or ".." in source_path.parts:
                    skipped.append(info.filename)
                    continue
                name = source_path.name
                if name not in REPRO_BUNDLE_FILES:
                    skipped.append(info.filename)
                    continue
                if info.file_size > MAX_UPLOAD_BYTES and name.endswith(".csv"):
                    return (
                        False,
                        dbc.Alert(f"{name} exceeds the public CSV size limit.", color="danger"),
                        False,
                    )
                allowed_infos.append(info)

            incoming_names = {Path(info.filename).name for info in allowed_infos}
            required = {"raw.csv", "metadata_origin.csv"}
            missing = sorted(required - incoming_names)
            if missing:
                return (
                    False,
                    dbc.Alert(
                        "Bundle is missing required input file(s): " + ", ".join(missing),
                        color="danger",
                    ),
                    False,
                )

            session_dir.mkdir(parents=True, exist_ok=True)
            clear_session_derived_outputs(session_dir, preserve_inputs=False)
            for info in allowed_infos:
                name = Path(info.filename).name
                target = session_dir / name
                with zf.open(info) as src, target.open("wb") as dst:
                    shutil.copyfileobj(src, dst)
                restored.append(name)
    except zipfile.BadZipFile:
        return False, dbc.Alert("Uploaded file is not a valid zip bundle.", color="danger"), False
    except Exception as exc:
        return False, dbc.Alert(f"Failed to restore bundle: {exc}", color="danger"), False

    preprocess_ready = all((session_dir / name).exists() for name in ("metadata.csv", "raw_tss.csv", "raw_clr.csv"))
    cfg = _read_session_config(session_dir)
    target_col = cfg.get("label_column") if isinstance(cfg, dict) else None
    batch_col = None
    try:
        header, _ = _read_csv_records(session_dir / "metadata_origin.csv")
        for col in header:
            if col.lower() == "batch":
                batch_col = col
                break
        if target_col not in header:
            target_col = None
        if target_col is None:
            for col in header:
                if col.lower() in {"phenotype", "target", "group", "target_binary"}:
                    target_col = col
                    break
    except Exception:
        batch_col = None
        target_col = None
    report_payload = validate_session_inputs(
        session_dir,
        batch_col=batch_col,
        target_col=str(target_col) if target_col else None,
    )

    body: List[object] = [
        html.Div("Reproducibility bundle restored.", className="fw-semibold mb-2"),
        html.Div("Restored files: " + ", ".join(sorted(set(restored))), className="mb-2"),
    ]
    if skipped:
        body.append(html.Div("Ignored non-bundle files: " + ", ".join(sorted(set(skipped))[:5]), className="text-muted mb-2"))
    if preprocess_ready:
        body.append(html.Div("Preprocessed inputs found. You can proceed directly to correction or assessment reruns."))
    else:
        body.append(html.Div("Raw inputs restored. Switch to Manual Upload if you need to regenerate preprocessing outputs."))
    if report_payload and not report_payload.get("valid"):
        body.append(_render_validation_report(report_payload))
        return False, dbc.Alert(body, color="danger"), False
    return True, dbc.Alert(body, color="success"), preprocess_ready


def _scan_example_sets() -> List[Tuple[str, Path, Path]]:
    """Find example pairs in assets/example as (key, raw_path, meta_path).

    A pair matches when files share the same suffix after 'raw_' / 'metadata_'.
    Case-insensitive; supports any file extension (commonly .csv).
    """
    if not EXAMPLE_DIR.exists():
        return []
    raw_map: Dict[str, Path] = {}
    meta_map: Dict[str, Path] = {}
    for p in EXAMPLE_DIR.iterdir():
        if not p.is_file():
            continue
        name = p.name
        low = name.lower()
        if low.startswith("raw_"):
            suf = name.split("_", 1)[1].rsplit(".", 1)[0]
            raw_map[suf] = p
        elif low.startswith("metadata_"):
            suf = name.split("_", 1)[1].rsplit(".", 1)[0]
            meta_map[suf] = p
    pairs: List[Tuple[str, Path, Path]] = []
    for key in sorted(set(raw_map) & set(meta_map)):
        pairs.append((key, raw_map[key], meta_map[key]))
    return pairs


def _example_pair_for(key: str) -> Optional[Tuple[Path, Path]]:
    for k, raw_p, meta_p in _scan_example_sets():
        if k == key:
            return raw_p, meta_p
    return None


def _read_session_config(session_dir: Path) -> Dict[str, object]:
    cfg_path = session_dir / "session_config.json"
    if not cfg_path.exists():
        return {}
    try:
        return json.loads(cfg_path.read_text(encoding="utf-8"))
    except Exception:
        return {}


def _example_mapping_for(key: Optional[str]) -> Optional[Dict[str, object]]:
    if not key:
        return None
    return EXAMPLE_COLUMN_MAP.get(key.lower())


def _render_mosaic_card(session_dir: Path) -> html.Div:
    """Return a card displaying the mosaic plot if it exists."""
    img_path = session_dir / "mosaic_plot.tif"
    if not img_path.exists():
        return html.Div("Mosaic plot not generated yet.", className="text-muted")
    try:
        encoded = _encode_image_source(img_path)
    except Exception:
        return html.Div("Failed to read mosaic plot image.", className="text-danger")
    img = html.Img(
        src=encoded,
        alt="Mosaic plot",
        style={"maxWidth": "100%", "height": "auto"},
    )
    return dbc.Card(
        [
            dbc.CardHeader(html.Strong("Mosaic plot")),
            dbc.CardBody(
                html.Div(img, className="d-flex justify-content-center"),
                className="text-center",
            ),
        ],
        className="mt-3",
    )


def _update_session_config(session_dir: Path, **entries: object) -> Tuple[bool, Optional[str]]:
    """Merge provided entries into session_config.json."""

    cfg_path = session_dir / "session_config.json"
    config: Dict[str, object] = {}
    if cfg_path.exists():
        try:
            config = json.loads(cfg_path.read_text(encoding="utf-8"))
        except Exception:
            config = {}

    for key, value in entries.items():
        config[key] = value

    try:
        cfg_path.write_text(
            json.dumps(config, indent=2, ensure_ascii=False),
            encoding="utf-8",
        )
        return True, None
    except Exception as exc:
        return False, str(exc)


def _persist_study_settings(session_dir: Path, control_label: str, reference_batch: str) -> Tuple[bool, Optional[str]]:
    """Write the selected study settings into session_config.json."""

    return _update_session_config(
        session_dir,
        control_label=None if control_label is None else str(control_label),
        reference_batch=None if reference_batch is None else str(reference_batch),
    )


def _resolve_label_column(session_dir: Path, header: Optional[List[str]] = None) -> Optional[str]:
    """Determine which metadata column carries the user-provided labels (text)."""

    cfg_path = session_dir / "session_config.json"
    label_col: Optional[str] = None
    if cfg_path.exists():
        try:
            cfg = json.loads(cfg_path.read_text(encoding="utf-8"))
            label_col = cfg.get("label_column")
        except Exception:
            label_col = None

    if header:
        if label_col in header:
            return label_col
        for col in header:
            if col.lower() == "phenotype":
                return col
    return label_col


def _generate_mosaic(session_dir: Path) -> Tuple[bool, Optional[str]]:
    """Run the Mosaic.R script for the current session."""
    log_path = session_dir / "run.log"
    ok, log = run_r_scripts(("Mosaic.R",), session_dir, log_path=log_path)
    if not ok:
        return False, log
    return True, None


def upload_layout(active_path: str):
    # Prepare example set options
    example_pairs = _scan_example_sets()
    default_example = example_pairs[0][0] if example_pairs else None
    return html.Div(
        [
            build_navbar(active_path),
            # Stores moved to global layout in _0_main.py
            dbc.Container(
                [
                    html.H2("Upload Data"),
                    html.P("Choose manual upload or example dataset."),
                    dcc.Tabs(
                        id="upload-tabs",
                        value="manual",
                        children=[
                            dcc.Tab(
                                label="Manual Upload",
                                value="manual",
                                children=html.Div(
                                    [
                                        dbc.Row(
                                            [
                                                dbc.Col(
                                                    dbc.Card(
                                                        [
                                                            dbc.CardHeader(html.Strong("Microbiome feature table (CSV)")),
                                                            dbc.CardBody(
                                                                [
                                                                    html.Ul(
                                                                        [
                                                                            html.Li(
                                                                                "Processed sample-by-feature numeric table, including "
                                                                                "16S-derived OTU/ASV tables or shotgun-derived "
                                                                                "taxonomic/functional profiles after upstream profiling."
                                                                            ),
                                                                            html.Li("Raw sequencing files such as FASTQ are not accepted."),
                                                                            html.Li("Samples in rows and profiled features in columns."),
                                                                            html.Li(f"Samples: {MAX_SAMPLES} or fewer."),
                                                                            html.Li(f"Features: {MAX_FEATURES} or fewer."),
                                                                            html.Li(f"CSV size: {human_size(MAX_UPLOAD_BYTES)} or smaller."),
                                                                            html.Li("No blank, NA, NaN, Inf, or non-numeric matrix values."),
                                                                            html.Li("All-zero sample rows are blocked; all-zero feature columns trigger a warning."),
                                                                            html.Li(
                                                                                "Count-based correction methods require nonnegative integer counts; "
                                                                                "continuous or transformed matrices disable those methods with a warning."
                                                                            ),
                                                                            html.Li(
                                                                                "FAbatch requires retained features after low-variance filtering "
                                                                                "to be greater than the largest batch size; otherwise FAbatch is "
                                                                                "marked unavailable for the session."
                                                                            ),
                                                                        ],
                                                                        className="mb-3",
                                                                    ),
                                                                    dcc.Upload(
                                                                        id="upload-matrix",
                                                                        children=html.Div(["Drag & drop or click to upload feature table"]),
                                                                        multiple=False,
                                                                        className="border border-secondary rounded p-4 text-center bg-light",
                                                                        accept=".csv,text/csv",
                                                                    ),
                                                                ]
                                                            ),
                                                            dbc.CardFooter(
                                                                html.Div(id="matrix-file-info", className="text-muted", children="No file uploaded yet.")
                                                            ),
                                                        ],
                                                        className="h-100",
                                                    ),
                                                    md=6,
                                                    className="mb-3",
                                                ),
                                                dbc.Col(
                                                    dbc.Card(
                                                        [
                                                            dbc.CardHeader(html.Strong("Metadata (CSV)")),
                                                            dbc.CardBody(
                                                                [
                                                                    html.Ul(
                                                                        [
                                                                            html.Li("One row per sample, in the same sample order as the matrix."),
                                                                            html.Li("Required: one batch column and one binary target/phenotype column."),
                                                                            html.Li(
                                                                                f"Columns: {MAX_METADATA_COLUMNS} or fewer, including batch, target, and optional covariates."
                                                                            ),
                                                                            html.Li(f"CSV size: {human_size(MAX_UPLOAD_BYTES)} or smaller."),
                                                                            html.Li("No blank, NA, NaN, Inf, or NA-like metadata values."),
                                                                            html.Li(
                                                                                "mBatchNet reports a warning when batch and target are strongly associated."
                                                                            ),
                                                                        ],
                                                                        className="mb-3",
                                                                    ),
                                                                    dcc.Upload(
                                                                        id="upload-metadata",
                                                                        children=html.Div(["Drag & drop or click to upload metadata"]),
                                                                        multiple=False,
                                                                        className="border border-secondary rounded p-4 text-center bg-light",
                                                                        accept=".csv,text/csv",
                                                                    ),
                                                                ]
                                                            ),
                                                            dbc.CardFooter(
                                                                html.Div(id="metadata-file-info", className="text-muted", children="No file uploaded yet.")
                                                            ),
                                                        ],
                                                        className="h-100",
                                                    ),
                                                    md=6,
                                                    className="mb-3",
                                                ),
                                            ]
                                        ),
                                    ],
                                    className="mt-3",
                                ),
                            ),
                            dcc.Tab(
                                label="Example Dataset",
                                value="example",
                                children=html.Div(
                                    [
                                        dbc.Card(
                                            [
                                                dbc.CardHeader(html.Strong("Quick Start: Example Data")),
                                                dbc.CardBody(
                                                    [
                                                        html.Div(
                                                            [
                                                                html.Div(
                                                                    "Example dataset: Anaerobic Digestion",
                                                                    className="fw-semibold",
                                                                ),
                                                                html.Div(
                                                                    "Columns mapped as Batch (batch), Initial Phenol Concentration (target), Treatment Duration (covariate).",
                                                                    className="text-muted mb-2",
                                                                ),
                                                                dcc.Input(
                                                                    id="example-select",
                                                                    type="hidden",
                                                                    value=default_example,
                                                                    readOnly=True,
                                                                ),
                                                            ]
                                                        ),
                                                        dbc.Row(
                                                            [
                                                                dbc.Col(
                                                                    [
                                                                        dbc.Label(
                                                                            "Preview rows",
                                                                            html_for="example-preview-rows",
                                                                            className="fw-semibold",
                                                                        ),
                                                                        dbc.Input(
                                                                            id="example-preview-rows",
                                                                            type="number",
                                                                            value=PREVIEW_MAX_ROWS,
                                                                            min=1,
                                                                            step=1,
                                                                            style={"maxWidth": "200px"},
                                                                        ),
                                                                    ],
                                                                    md=6,
                                                                    className="mt-2",
                                                                ),
                                                                dbc.Col(
                                                                    [
                                                                        dbc.Label(
                                                                            "Preview columns",
                                                                            html_for="example-preview-cols",
                                                                            className="fw-semibold",
                                                                        ),
                                                                        dbc.Input(
                                                                            id="example-preview-cols",
                                                                            type="number",
                                                                            value=PREVIEW_MAX_COLS,
                                                                            min=1,
                                                                            step=1,
                                                                            style={"maxWidth": "200px"},
                                                                        ),
                                                                    ],
                                                                    md=6,
                                                                    className="mt-2",
                                                                ),
                                                            ],
                                                            className="g-3 mt-3",
                                                        ),
                                                        html.Div(id="example-preview", className="mt-3"),
                                                        dbc.Button(
                                                            "Load Selected Example",
                                                            id="load-example",
                                                            color="success",
                                                            outline=False,
                                                            className="mt-2",
                                                            style={"width": "250px"},
                                                        ),
                                                        html.Div(id="example-load-status", className="text-muted mt-2"),
                                                    ]
                                                ),
                                            ]
                                        ),
                                    ],
                                    className="mt-3",
                                ),
                            ),
                            dcc.Tab(
                                label="Reproducibility Bundle",
                                value="reproducibility",
                                children=html.Div(
                                    [
                                        dbc.Card(
                                            [
                                                dbc.CardHeader(html.Strong("Restore from Repro bundle")),
                                                dbc.CardBody(
                                                    [
                                                        html.P(
                                                            "Upload a reproducibility_bundle.zip exported by mBatchNet to restore the saved session inputs and rerun from the same state.",
                                                            className="text-muted",
                                                        ),
                                                        html.Ul(
                                                            [
                                                                html.Li("Run or restore an mBatchNet session until the navbar download buttons are enabled."),
                                                                html.Li("Click Repro bundle in the navbar."),
                                                                html.Li("Upload that downloaded zip here to reproduce the saved session state."),
                                                            ],
                                                            className="mb-3",
                                                        ),
                                                        dcc.Upload(
                                                            id="upload-repro-bundle",
                                                            children=html.Div("Upload Repro bundle"),
                                                            multiple=False,
                                                            className="border border-secondary rounded p-4 text-center bg-light",
                                                            accept=".zip,application/zip",
                                                        ),
                                                        html.Div(
                                                            id="repro-bundle-status",
                                                            className="mt-3 text-muted",
                                                            children="No bundle uploaded yet.",
                                                        ),
                                                    ]
                                                ),
                                            ],
                                            className="mb-3",
                                        ),
                                    ],
                                    className="mt-3",
                                ),
                            ),
                        ],
                    ),
                    # Shared process area below tabs (wrapped in Loading to prevent duplicate clicks)
                    html.Div([
                        html.Hr(),
                        html.H4("Process"),
                        html.P("Review metadata columns and run preprocessing."),
                        dcc.Loading([
                            dbc.Button(
                                "Process",
                                id="process-uploads",
                                color="secondary",  # gray until enabled
                                className="mb-3",
                                disabled=True,
                                style={"width": "250px"},
                                size="sm",
                            ),
                            html.Div(id="metadata-columns-display", className="mt-3 mb-2"),
                            html.Div(id="column-mapping-container", className="mb-3"),
                            html.Div(id="process-result", className="mb-2"),
                        ], type="default"),
                    ], id="process-area"),
                    html.Div(id="study-settings-container", className="mt-4"),
                    dcc.Loading(
                        html.Div(
                            "Mosaic plot not generated yet.",
                            id="mosaic-preview",
                            className="mt-3 text-muted",
                        ),
                        type="default",
                    ),
                ],
                fluid=True,
            ),
        ]
    )


def register_upload_callbacks(app):
    # Switch to Example tab when explicitly requested via URL query (?tab=example)
    @app.callback(
        Output("upload-tabs", "value"),
        Input("page-url", "search"),
        State("page-url", "pathname"),
        State("upload-tabs", "value"),
        prevent_initial_call=False,
    )
    def select_tab_from_query(search: str, pathname: str, current_tab: str):
        # Only react on the Upload page to avoid interfering with other routes
        if (pathname or "/").split("?", 1)[0] != "/upload":
            raise dash.exceptions.PreventUpdate

        query = (search or "").lstrip("?")
        params = parse_qs(query)
        desired_tab = params.get("tab", [None])[0]

        if desired_tab in {"manual", "example", "reproducibility"} and desired_tab != current_tab:
            return desired_tab

        raise dash.exceptions.PreventUpdate

    # Reset example-loaded when visiting Upload under a new session
    @app.callback(
        Output("example-loaded", "data", allow_duplicate=True),
        Output("upload-last-session", "data"),
        Input("page-url", "pathname"),
        State("session-id", "data"),
        State("upload-last-session", "data"),
        prevent_initial_call=True,
    )
    def reset_example_loaded_on_new_session(pathname, session_id, last_seen_session):
        if pathname != "/upload" or not session_id:
            raise dash.exceptions.PreventUpdate
        if last_seen_session == session_id:
            # Already recorded this session; keep example flag as-is, but ensure last session is stored
            return dash.no_update, last_seen_session
        # New session detected on Upload page: clear example flag and record session id
        return False, session_id

    @app.callback(
        Output("upload-complete", "data"),
        Output("matrix-file-info", "children"),
        Output("metadata-file-info", "children"),
        Output("example-load-status", "children"),
        Output("example-loaded", "data"),
        Output("repro-bundle-status", "children"),
        Output("preprocess-complete", "data", allow_duplicate=True),
        Output("pre-started", "data", allow_duplicate=True),
        Output("pre-complete", "data", allow_duplicate=True),
        Output("correction-complete", "data", allow_duplicate=True),
        Output("post-complete", "data", allow_duplicate=True),
        Output("method-operation-trigger", "data", allow_duplicate=True),
        Output("runlog-path", "data", allow_duplicate=True),
        Output("runlog-file-meta", "data", allow_duplicate=True),
        Input("upload-matrix", "contents"),
        Input("upload-metadata", "contents"),
        Input("load-example", "n_clicks"),
        Input("upload-repro-bundle", "contents"),
        State("upload-matrix", "filename"),
        State("upload-metadata", "filename"),
        State("upload-repro-bundle", "filename"),
        State("session-id", "data"),
        State("example-select", "value"),
        State("method-operation-trigger", "data"),
        prevent_initial_call=True,
    )
    def handle_upload(
        matrix_contents,
        metadata_contents,
        load_example_clicks,
        repro_contents,
        matrix_name,
        metadata_name,
        repro_name,
        session_id,
        example_key,
        method_operation_trigger,
    ):
        method_refresh = int(method_operation_trigger or 0)
        if not session_id:
            return (
                False,
                "No file uploaded yet.",
                "No file uploaded yet.",
                dash.no_update,
                dash.no_update,
                dash.no_update,
                False,
                False,
                False,
                False,
                False,
                dash.no_update,
                "",
                None,
            )

        session_dir = get_session_dir(session_id)
        saved_items: List[str] = []
        matrix_info = dash.no_update
        metadata_info = dash.no_update
        example_status = dash.no_update
        example_loaded_out = dash.no_update
        repro_status = dash.no_update
        preprocess_complete_out = False
        pre_started_out = False
        pre_complete_out = False
        correction_complete_out = False
        post_complete_out = False
        method_operation_trigger_out = dash.no_update
        runlog_path_out = ""
        runlog_meta_out = None

        # Determine trigger
        ctx = dash.callback_context
        trig = ctx.triggered[0]["prop_id"].split(".")[0] if ctx.triggered else None

        if trig == "load-example":
            # Copy selected example files into the session directory
            try:
                clear_session_derived_outputs(session_dir, preserve_inputs=True)
                method_operation_trigger_out = method_refresh + 1
                pair = None
                selected_key = example_key
                if example_key:
                    res = _example_pair_for(example_key)
                    if res:
                        pair = res
                if pair is None:
                    pairs = _scan_example_sets()
                    if pairs:
                        selected_key, raw_src, meta_src = pairs[0]
                        pair = (raw_src, meta_src)
                if pair is None:
                    example_status = html.Span("No example datasets found in assets/example.", className="text-danger")
                else:
                    raw_src, meta_src = pair
                    if raw_src.exists():
                        shutil.copy2(raw_src, session_dir / "raw.csv")
                    if meta_src.exists():
                        shutil.copy2(meta_src, session_dir / "metadata_origin.csv")
                    if selected_key:
                        _update_session_config(session_dir, example_key=selected_key)
                    # Build file info
                    if (session_dir / "raw.csv").exists():
                        size = (session_dir / "raw.csv").stat().st_size
                        matrix_info = (
                            dbc.Badge("Loaded", color="info", className="me-2"),
                            html.Span(f"Source: {raw_src.relative_to(BASE_DIR)} | Saved as: raw.csv | {human_size(size)}"),
                        )
                    if (session_dir / "metadata_origin.csv").exists():
                        size = (session_dir / "metadata_origin.csv").stat().st_size
                        metadata_info = (
                            dbc.Badge("Loaded", color="info", className="me-2"),
                            html.Span(
                                f"Source: {meta_src.relative_to(BASE_DIR)} | Saved as: metadata_origin.csv | {human_size(size)}"
                            ),
                        )
                    example_status = html.Span("Example files loaded.")
                    example_loaded_out = True
                    repro_status = "No bundle uploaded yet."
            except Exception:
                example_status = html.Span("Failed to load example files.", className="text-danger")

        if matrix_contents and trig == "upload-matrix":
            clear_session_derived_outputs(session_dir, preserve_inputs=True)
            method_operation_trigger_out = method_refresh + 1
            save_uploaded_file(matrix_contents, session_dir, "raw.csv")
            size = (session_dir / "raw.csv").stat().st_size
            matrix_info = (
                dbc.Badge("Uploaded", color="success", className="me-2"),
                html.Span(f"Source: {matrix_name} | Saved as: raw.csv | {human_size(size)}"),
            )
            saved_items.append(f"Matrix saved as raw.csv (source: {matrix_name})")
            example_loaded_out = False
            repro_status = "No bundle uploaded yet."

        if metadata_contents and trig == "upload-metadata":
            clear_session_derived_outputs(session_dir, preserve_inputs=True)
            method_operation_trigger_out = method_refresh + 1
            save_uploaded_file(metadata_contents, session_dir, "metadata_origin.csv")
            size = (session_dir / "metadata_origin.csv").stat().st_size
            metadata_info = (
                dbc.Badge("Uploaded", color="success", className="me-2"),
                html.Span(f"Source: {metadata_name} | Saved as: metadata_origin.csv | {human_size(size)}"),
            )
            saved_items.append(f"Metadata saved as metadata_origin.csv (source: {metadata_name})")
            example_loaded_out = False
            repro_status = "No bundle uploaded yet."

        if repro_contents and trig == "upload-repro-bundle":
            ok, repro_status, preprocess_ready = _restore_repro_bundle(repro_contents, session_dir)
            example_loaded_out = False
            method_operation_trigger_out = method_refresh + 1
            upload_complete = bool(ok and (session_dir / "raw.csv").exists() and (session_dir / "metadata_origin.csv").exists())
            if upload_complete:
                raw_size = (session_dir / "raw.csv").stat().st_size
                meta_size = (session_dir / "metadata_origin.csv").stat().st_size
                source = repro_name or "uploaded bundle"
                matrix_info = (
                    dbc.Badge("Restored", color="success", className="me-2"),
                    html.Span(f"Source: {source} | Saved as: raw.csv | {human_size(raw_size)}"),
                )
                metadata_info = (
                    dbc.Badge("Restored", color="success", className="me-2"),
                    html.Span(f"Source: {source} | Saved as: metadata_origin.csv | {human_size(meta_size)}"),
                )
                preprocess_complete_out = bool(preprocess_ready)
                log_path = session_dir / "run.log"
                if log_path.exists():
                    runlog_path_out = str(log_path)
                    runlog_meta_out = log_file_meta(log_path)
            return (
                upload_complete,
                matrix_info,
                metadata_info,
                dash.no_update,
                example_loaded_out,
                repro_status,
                preprocess_complete_out,
                pre_started_out,
                pre_complete_out,
                correction_complete_out,
                post_complete_out,
                method_operation_trigger_out,
                runlog_path_out,
                runlog_meta_out,
            )

        upload_complete = (session_dir / "raw.csv").exists() and (session_dir / "metadata_origin.csv").exists()
        return (
            upload_complete,
            matrix_info,
            metadata_info,
            example_status,
            example_loaded_out,
            repro_status,
            preprocess_complete_out,
            pre_started_out,
            pre_complete_out,
            correction_complete_out,
            post_complete_out,
            method_operation_trigger_out,
            runlog_path_out,
            runlog_meta_out,
        )

    @app.callback(
        Output("process-uploads", "disabled"),
        Output("process-uploads", "color"),
        Input("upload-complete", "data"),
    )
    def toggle_process_button(upload_complete: bool):
        enabled = bool(upload_complete)
        return (not enabled), ("success" if enabled else "secondary")

    @app.callback(
        Output("study-settings-container", "children"),
        Input("preprocess-complete", "data"),
        Input("example-loaded", "data"),
        State("session-id", "data"),
        State("page-url", "pathname"),
        prevent_initial_call=True,
    )
    def show_study_settings_card(preprocess_complete, example_loaded, session_id, pathname):
        if (pathname or "/").split("?", 1)[0] != "/upload":
            raise dash.exceptions.PreventUpdate
        if not session_id:
            return html.Div()
        if not preprocess_complete:
            return html.Div()
        if example_loaded:
            # Example datasets apply study settings automatically; hide card.
            return html.Div()

        session_dir = get_session_dir(session_id)
        meta_path = session_dir / "metadata_origin.csv"
        if not meta_path.exists():
            return html.Div("Metadata file not found; preprocess the data first.", className="text-danger")

        import csv  # local import to avoid top-level dependency when unused

        try:
            with meta_path.open("r", encoding="utf-8", newline="") as fh:
                reader = csv.DictReader(fh)
                rows = list(reader)
        except Exception:
            return html.Div("Failed to read metadata_origin.csv for study settings.", className="text-danger")
        if not rows:
            return html.Div("Metadata file is empty; unable to configure study settings.", className="text-danger")

        label_col = _resolve_label_column(session_dir, reader.fieldnames or [])

        def _collect(column: str) -> List[str]:
            vals = {
                str(row.get(column)).strip()
                for row in rows
                if row.get(column) not in (None, "")
            }
            return sorted(v for v in vals if v)

        phenotypes = _collect(label_col) if label_col else []
        batches = _collect("batch")
        if not phenotypes or not batches:
            return html.Div("Metadata must contain non-empty target labels and batch columns.", className="text-danger")

        saved_control = None
        saved_reference = None
        cfg_path = session_dir / "session_config.json"
        if cfg_path.exists():
            try:
                cfg = json.loads(cfg_path.read_text(encoding="utf-8"))
                saved_control = cfg.get("control_label")
                saved_reference = cfg.get("reference_batch")
            except Exception:
                saved_control = None
                saved_reference = None

        if saved_control not in phenotypes:
            saved_control = None
        if saved_reference not in batches:
            saved_reference = None

        control_dropdown = dbc.Col(
            [
                dbc.Label("Negative / Control label", html_for="study-control-label", className="fw-semibold"),
                dcc.Dropdown(
                    id="study-control-label",
                    options=[{"label": val, "value": val} for val in phenotypes],
                    value=saved_control,
                    placeholder="Select control label",
                    clearable=False,
                ),
            ],
            md=6,
        )
        reference_dropdown = dbc.Col(
            [
                dbc.Label("Reference batch", html_for="study-reference-batch", className="fw-semibold"),
                dcc.Dropdown(
                    id="study-reference-batch",
                    options=[{"label": val, "value": val} for val in batches],
                    value=saved_reference,
                    placeholder="Select reference batch",
                    clearable=False,
                ),
            ],
            md=6,
        )

        return dbc.Card(
            [
                dbc.CardHeader(html.Strong("Study settings")),
                dbc.CardBody(
                    [
                        html.P(
                            "Select the control/negative target label and reference batch before generating the mosaic plot.",
                            className="text-muted",
                        ),
                        dbc.Row([control_dropdown, reference_dropdown], className="gy-2"),
                        dbc.Button(
                            "Apply Study Settings",
                            id="apply-study-settings",
                            color="secondary",
                            className="mt-3",
                            disabled=True,
                            style={"width": "250px"},
                            size="sm",
                        ),
                        html.Div(id="study-settings-status", className="mt-3 text-muted"),
                    ]
                ),
            ],
            className="mt-3",
        )

    @app.callback(
        Output("apply-study-settings", "disabled"),
        Output("apply-study-settings", "color"),
        Input("study-control-label", "value"),
        Input("study-reference-batch", "value"),
        prevent_initial_call=True,
    )
    def toggle_study_settings_button(control_label, reference_batch):
        ready = bool(control_label) and bool(reference_batch)
        return (not ready), ("success" if ready else "secondary")

    @app.callback(
        Output("study-settings-status", "children"),
        Output("preprocess-complete", "data", allow_duplicate=True),
        Output("mosaic-preview", "children", allow_duplicate=True),
        Input("apply-study-settings", "n_clicks"),
        State("study-control-label", "value"),
        State("study-reference-batch", "value"),
        State("session-id", "data"),
        State("page-url", "pathname"),
        prevent_initial_call=True,
    )
    def apply_study_settings(
        n_clicks: int,
        control_label: str,
        reference_batch: str,
        session_id: str,
        pathname: str,
    ):
        if not n_clicks:
            raise dash.exceptions.PreventUpdate
        if (pathname or "/").split("?", 1)[0] != "/upload":
            raise dash.exceptions.PreventUpdate
        if not session_id:
            return html.Span("Session not initialised.", className="text-danger"), dash.no_update, dash.no_update
        if not control_label or not reference_batch:
            return html.Span("Select both control label and reference batch.", className="text-danger"), dash.no_update, dash.no_update

        session_dir = get_session_dir(session_id)
        ok_cfg, err = _persist_study_settings(session_dir, control_label, reference_batch)
        if not ok_cfg:
            return html.Span(f"Failed to save study settings: {err}", className="text-danger"), dash.no_update, dash.no_update

        ok_mosaic, mosaic_err = _generate_mosaic(session_dir)
        if not ok_mosaic:
            return html.Span("Failed to generate mosaic plot. Check run log for details.", className="text-danger"), dash.no_update, dash.no_update

        return (
            html.Span("Study settings applied. Mosaic generated.", className="text-success"),
            True,
            _render_mosaic_card(session_dir),
        )

    @app.callback(
        Output("metadata-columns-display", "children"),
        Output("column-mapping-container", "children"),
        Input("process-uploads", "n_clicks"),
        Input("example-loaded", "data"),
        State("session-id", "data"),
        State("page-url", "pathname"),
        prevent_initial_call=True,
    )
    def show_metadata_columns(n_clicks: int, example_loaded: bool, session_id: str, pathname: str):
        # Guard: only update when on Upload page
        if (pathname or "/").split("?", 1)[0] != "/upload":
            raise dash.exceptions.PreventUpdate
        if not n_clicks and not example_loaded:
            raise dash.exceptions.PreventUpdate
        if not session_id:
            return dash.no_update, dash.no_update
        session_dir = get_session_dir(session_id)
        meta_path = session_dir / "metadata_origin.csv"
        if not meta_path.exists():
            return dash.no_update, dash.no_update

        # Read header from metadata
        try:
            import csv
            with meta_path.open("r", encoding="utf-8") as fh:
                reader = csv.reader(fh)
                header = next(reader)
        except Exception:
            return dash.no_update, dash.no_update

        col_names = [name.strip() for name in header if name and name.strip()]
        if not col_names:
            return dash.no_update, dash.no_update

        chips = html.Div([
            html.Span(name, className="badge bg-secondary me-1 mb-1") for name in col_names
        ])

        # Detect whether this was triggered by example load or manual process
        ctx = dash.callback_context
        triggered_id = ctx.triggered[0]["prop_id"].split(".")[0] if ctx.triggered else ""
        example_mode = (triggered_id == "example-loaded" and bool(example_loaded))

        if example_mode:
            # Show read-only mapping info; no dropdowns, no apply button
            cfg = _read_session_config(session_dir)
            example_key = cfg.get("example_key") if isinstance(cfg, dict) else None
            preset = _example_mapping_for(example_key)
            batch_label = preset.get("batch") if preset else "batch"
            target_label = preset.get("target") if preset else "phenotype"
            covar_labels = preset.get("covariates") if preset else []
            report = validate_session_inputs(
                session_dir,
                batch_col=str(batch_label),
                target_col=str(target_label),
            )
            mapping_display = dbc.Card(
                [
                    dbc.CardHeader(html.Strong("Metadata mapping")),
                    dbc.CardBody(
                        [
                            html.Div("Using the following mapping for example data:"),
                            html.Ul(
                                [
                                    html.Li([html.Code(batch_label), " column -> batch"]),
                                    html.Li(
                                        [
                                            html.Code(target_label),
                                            " column -> target_binary (binary copy written for correction)",
                                        ]
                                    ),
                                    html.Li(
                                        [
                                            html.Code(", ".join(map(str, covar_labels)) or "(none)"),
                                            " column(s) -> covariates",
                                        ]
                                    ),
                                ],
                                className="mb-0",
                            ),
                        ]
                    ),
                ],
                className="mt-2",
            )

            info = html.Div(
                [
                    html.H6("Columns in metadata_origin.csv:"),
                    chips,
                    _render_validation_report(report),
                ]
            )
            return info, mapping_display

        # Manual flow: require explicit user selection (no defaults)
        report = validate_session_inputs(session_dir)
        opts = [{"label": name, "value": name} for name in col_names]
        mapping_ui = dbc.Card(
            [
                dbc.CardHeader(html.Strong("Map Metadata Columns")),
                dbc.CardBody(
                    [
                        dbc.Row(
                            [
                                dbc.Col(
                                    [
                                        dbc.Label("Batch ID column"),
                                        dcc.Dropdown(
                                            id="map-batch-id",
                                            options=opts,
                                            value=None,
                                            placeholder="Select batch column",
                                            clearable=True,
                                        ),
                                    ],
                                    md=6,
                                ),
                                dbc.Col(
                                    [
                                        dbc.Label("Target (binary) column"),
                                        dcc.Dropdown(
                                            id="map-target-binary",
                                            options=opts,
                                            value=None,
                                            placeholder="Select column to convert to target_binary (e.g., positive/negative)",
                                            clearable=True,
                                        ),
                                    ],
                                    md=6,
                                ),
                            ],
                            className="gy-2",
                        ),
                        dbc.Button(
                            "Apply mapping and preprocess",
                            id="apply-mapping",
                            color="secondary",
                            className="mt-3",
                            disabled=True,
                            style={"width": "250px"},
                            size="sm",
                        ),
                    ]
                ),
            ],
            className="mt-2",
        )

        info = html.Div(
            [
                html.H6("Columns in metadata_origin.csv:"),
                chips,
                _render_validation_report(report),
            ]
        )
        return info, mapping_ui

    @app.callback(
        Output("preprocess-complete", "data", allow_duplicate=True),
        Output("runlog-path", "data", allow_duplicate=True),
        Output("runlog-file-meta", "data", allow_duplicate=True),
        Output("runlog-modal", "is_open", allow_duplicate=True),
        Output("process-result", "children", allow_duplicate=True),
        Input("apply-mapping", "n_clicks"),
        State("map-batch-id", "value"),
        State("map-target-binary", "value"),
        State("session-id", "data"),
        State("page-url", "pathname"),
        prevent_initial_call=True,
    )
    def apply_mapping_and_preprocess(n_clicks: int, batch_col: str, pheno_col: str, session_id: str, pathname: str):
        # Guard: only run this when Upload page is active
        if (pathname or "/").split("?", 1)[0] != "/upload":
            raise dash.exceptions.PreventUpdate
        if not n_clicks:
            raise dash.exceptions.PreventUpdate
        if not session_id:
            return False, dash.no_update, dash.no_update, dash.no_update, dash.no_update
        if not batch_col or not pheno_col:
            return False, dash.no_update, dash.no_update, dash.no_update, dash.no_update

        session_dir = get_session_dir(session_id)
        meta_path = session_dir / "metadata_origin.csv"
        if not meta_path.exists():
            return False, dash.no_update, dash.no_update, dash.no_update, dash.no_update

        pre_report = validate_session_inputs(session_dir, batch_col=batch_col, target_col=pheno_col)
        if not pre_report.get("valid"):
            return False, dash.no_update, dash.no_update, dash.no_update, _render_validation_report(pre_report)

        # Rename columns by rewriting CSV
        try:
            import csv
            with meta_path.open("r", encoding="utf-8", newline="") as fh:
                reader = csv.DictReader(fh)
                orig_fieldnames = reader.fieldnames or []
                rows = list(reader)
            # Build new header mapping (no sample_id mapping required)
            new_fieldnames = []
            for name in orig_fieldnames:
                if name == batch_col:
                    new_fieldnames.append("batch")
                else:
                    new_fieldnames.append(name)
            # Write back with new headers
            with meta_path.open("w", encoding="utf-8", newline="") as fh:
                writer = csv.DictWriter(fh, fieldnames=new_fieldnames)
                writer.writeheader()
                for row in rows:
                    out_row = {}
                    for old, new in zip(orig_fieldnames, new_fieldnames):
                        out_row[new] = row.get(old)
                    writer.writerow(out_row)
            _update_session_config(
                session_dir,
                label_column=pheno_col,
                target_binary_column="target_binary",
            )
        except Exception:
            return False, dash.no_update, dash.no_update, dash.no_update, dash.no_update

        report = validate_session_inputs(session_dir, batch_col="batch", target_col=pheno_col)
        if not report.get("valid"):
            return False, dash.no_update, dash.no_update, dash.no_update, _render_validation_report(report)

        # Run preprocess.R in the session directory
        # Use a single session-wide log file and append to it
        log_path = session_dir / "run.log"
        ok, log = run_preprocess(session_dir, log_path=log_path)
        # Do not auto-open logs modal; user can open manually
        result = _render_validation_report(report)
        if not ok:
            result = dbc.Alert("Preprocessing failed. Check the run log for details.", color="danger")
        return bool(ok), str(log_path), log_file_meta(log_path), dash.no_update, result

    # Auto-preprocess when example data is loaded (no button press needed)
    @app.callback(
        Output("preprocess-complete", "data", allow_duplicate=True),
        Output("runlog-path", "data", allow_duplicate=True),
        Output("runlog-file-meta", "data", allow_duplicate=True),
        Output("runlog-modal", "is_open", allow_duplicate=True),
        Output("mosaic-preview", "children", allow_duplicate=True),
        Output("process-result", "children", allow_duplicate=True),
        Input("example-loaded", "data"),
        State("session-id", "data"),
        State("page-url", "pathname"),
        prevent_initial_call=True,
    )
    def auto_preprocess_on_example(example_loaded: bool, session_id: str, pathname: str):
        # Guard: only run this when Upload page is active
        if (pathname or "/").split("?", 1)[0] != "/upload":
            raise dash.exceptions.PreventUpdate
        if not example_loaded:
            raise dash.exceptions.PreventUpdate
        if not session_id:
            return False, dash.no_update, dash.no_update, dash.no_update, dash.no_update, dash.no_update
        session_dir = get_session_dir(session_id)
        meta_path = session_dir / "metadata_origin.csv"
        if not meta_path.exists():
            return False, dash.no_update, dash.no_update, dash.no_update, dash.no_update, dash.no_update
        label_col: Optional[str] = None

        # Ensure standard header names if needed (batch and chosen label column)
        try:
            import csv
            with meta_path.open("r", encoding="utf-8", newline="") as fh:
                reader = csv.DictReader(fh)
                orig_fieldnames = reader.fieldnames or []
                rows = list(reader)
            # Use mapping preset (if any) to locate batch/target columns
            cfg = _read_session_config(session_dir)
            example_key = cfg.get("example_key") if isinstance(cfg, dict) else None
            preset = _example_mapping_for(example_key) or {}

            def find_col(name: str) -> Optional[str]:
                for col in orig_fieldnames:
                    if col.lower() == name.lower():
                        return col
                return None

            batch_src = preset.get("batch", "batch")
            target_src = preset.get("target")

            batch_col = find_col(batch_src) or find_col("batch")
            label_col = find_col(target_src) if target_src else None
            if label_col is None:
                label_col = find_col("phenotype")

            replacements: Dict[str, str] = {}
            if batch_col and batch_col != "batch":
                replacements[batch_col] = "batch"
            if label_col:
                desired_label = target_src or label_col
                if label_col.lower() == desired_label.lower() and label_col != desired_label:
                    replacements[label_col] = desired_label
                elif target_src and label_col.lower() != target_src.lower():
                    replacements[label_col] = target_src

            new_fieldnames = orig_fieldnames
            if replacements:
                new_fieldnames = [replacements.get(name, name) for name in orig_fieldnames]
                with meta_path.open("w", encoding="utf-8", newline="") as fh:
                    writer = csv.DictWriter(fh, fieldnames=new_fieldnames)
                    writer.writeheader()
                    for row in rows:
                        out_row = {}
                        for old, new in zip(orig_fieldnames, new_fieldnames):
                            out_row[new] = row.get(old)
                        writer.writerow(out_row)
                if label_col:
                    label_col = replacements.get(label_col, label_col)
                if batch_col:
                    batch_col = "batch"

            if label_col is None and target_src and target_src in new_fieldnames:
                label_col = target_src
            if label_col is None and "phenotype" in new_fieldnames:
                label_col = "phenotype"
            if label_col:
                _update_session_config(
                    session_dir,
                    label_column=label_col,
                    target_binary_column="target_binary",
                )
        except Exception:
            pass

        report = validate_session_inputs(
            session_dir,
            batch_col="batch",
            target_col=label_col or "__missing_target__",
        )
        if not report.get("valid"):
            return (
                False,
                dash.no_update,
                dash.no_update,
                dash.no_update,
                dash.no_update,
                _render_validation_report(report),
            )

        # Kick off preprocess and append to the session-wide log
        log_path = session_dir / "run.log"
        ok, _ = run_preprocess(session_dir, log_path=log_path)
        # Do not auto-open logs modal; user can open manually
        mosaic_children = dash.no_update
        if ok:
            control_label = None
            reference_batch = None
            try:
                with meta_path.open("r", encoding="utf-8", newline="") as fh:
                    reader = csv.DictReader(fh)
                    current_rows = list(reader)
                control_label = _first_non_empty_level(current_rows, label_col)
                reference_batch = _first_non_empty_level(current_rows, "batch")
            except Exception:
                control_label = None
                reference_batch = None
            cfg_ok = False
            if control_label and reference_batch:
                cfg_ok, _ = _persist_study_settings(session_dir, control_label, reference_batch)
            if cfg_ok:
                mosaic_ok, _ = _generate_mosaic(session_dir)
                if mosaic_ok:
                    mosaic_children = _render_mosaic_card(session_dir)
        result = _render_validation_report(report)
        if not ok:
            result = dbc.Alert("Preprocessing failed. Check the run log for details.", color="danger")
        return bool(ok), str(log_path), log_file_meta(log_path), dash.no_update, mosaic_children, result

    # Hide the manual Process button when example data is used
    @app.callback(
        Output("process-uploads", "style"),
        Input("example-loaded", "data"),
        State("page-url", "pathname"),
    )
    def toggle_process_visibility(example_loaded: bool, pathname: str):
        # Guard to avoid updating when Upload page isn't active
        if (pathname or "/").split("?", 1)[0] != "/upload":
            raise dash.exceptions.PreventUpdate
        return {"display": "none", "width": "250px"} if example_loaded else {"width": "250px"}

    # Hide the manual Process section when a non-manual tab is active
    @app.callback(
        Output("process-area", "style"),
        Input("upload-tabs", "value"),
    )
    def toggle_process_area(active_tab: str):
        if active_tab != "manual":
            return {"display": "none"}
        return {}

    # Toggle the Apply Mapping button enabled state and color based on selections
    @app.callback(
        Output("apply-mapping", "disabled"),
        Output("apply-mapping", "color"),
        Input("map-batch-id", "value"),
        Input("map-target-binary", "value"),
        prevent_initial_call=True,
    )
    def toggle_apply_mapping(batch_val, pheno_val):
        ready = bool(batch_val) and bool(pheno_val)
        return (not ready), ("success" if ready else "secondary")

    # Disable and gray out Load Example button after loading
    @app.callback(
        Output("load-example", "disabled"),
        Output("load-example", "color"),
        Input("example-loaded", "data"),
        Input("session-id", "data"),
        Input("example-select", "value"),
        State("upload-last-session", "data"),
        State("page-url", "pathname"),
    )
    def toggle_load_example_button(example_loaded: bool, session_id: str, selected_example: str, last_seen_session: str, pathname: str):
        # Only update this Upload-page control when Upload is active
        if (pathname or "/").split("?", 1)[0] != "/upload":
            raise dash.exceptions.PreventUpdate
        # Enable the button for a new session even if example_loaded persisted True
        is_same_session = bool(session_id) and (session_id == (last_seen_session or ""))
        if not selected_example:
            return True, "secondary"
        if bool(example_loaded) and is_same_session:
            return True, "secondary"
        return False, "success"

    # Preview the selected example dataset (few rows from metadata/raw)
    @app.callback(
        Output("example-preview", "children"),
        Input("example-select", "value"),
        Input("example-preview-rows", "value"),
        Input("example-preview-cols", "value"),
        State("page-url", "pathname"),
        prevent_initial_call=False,
    )
    def preview_example(selected_key: str, preview_rows: Optional[int], preview_cols: Optional[int], pathname: str):
        # Only render on Upload page
        if (pathname or "/").split("?", 1)[0] != "/upload":
            raise dash.exceptions.PreventUpdate
        if not selected_key:
            return html.Div("No example datasets detected.", className="text-muted")
        pair = _example_pair_for(selected_key)
        if not pair:
            return html.Div("Selected example not found.", className="text-danger")
        raw_p, meta_p = pair
        import csv as _csv

        def _clamp_positive(val: Optional[int], default: int) -> int:
            try:
                iv = int(val)
                if iv > 0:
                    return iv
            except Exception:
                pass
            return default

        row_limit = _clamp_positive(preview_rows, PREVIEW_MAX_ROWS)
        col_limit = _clamp_positive(preview_cols, PREVIEW_MAX_COLS)

        def _table_for(path: Path, title: str):
            try:
                rows_preview: List[List[str]] = []
                row_count = 0
                total_col_count = 0
                header: Optional[List[str]] = None
                with path.open("r", encoding="utf-8", newline="") as fh:
                    reader = _csv.reader(fh)
                    for i, row in enumerate(reader):
                        if title == "Metadata" and i == 0:
                            header = row
                            total_col_count = len(header)
                            header = header[:col_limit]
                            continue
                        row_count += 1
                        if total_col_count == 0:
                            total_col_count = len(row)
                        if len(rows_preview) < row_limit:
                            rows_preview.append(row[:col_limit])
                if title == "Metadata" and header is None:
                    return html.Div(f"{title}: empty file", className="text-muted")
                col_count = total_col_count if total_col_count else (
                    len(rows_preview[0]) if rows_preview else 0
                )
                if title == "Metadata":
                    column_names = header or []
                else:
                    display_cols = min(col_count, col_limit)
                    column_names = [f"Column {i+1}" for i in range(display_cols)] if rows_preview else []
                if not column_names:
                    return html.Div(f"{title}: no preview rows", className="text-muted")

                row_records: List[Dict[str, object]] = []
                for row_vals in rows_preview:
                    record: Dict[str, object] = {}
                    for idx, col_name in enumerate(column_names):
                        cell = row_vals[idx] if idx < len(row_vals) else ""
                        if title == "Raw Matrix":
                            try:
                                record[col_name] = round(float(cell), 3)
                            except Exception:
                                record[col_name] = cell
                        else:
                            record[col_name] = cell
                    row_records.append(record)

                numeric_columns = {
                    col for col in column_names
                    if any(isinstance(row.get(col), (int, float)) for row in row_records)
                }
                column_defs: List[Dict[str, object]] = []
                for col in column_names:
                    col_def: Dict[str, object] = {"headerName": col, "field": col}
                    if title == "Metadata" and column_names and col == column_names[0]:
                        col_def["minWidth"] = 220
                        col_def["flex"] = 1
                    else:
                        col_def["minWidth"] = 140 if title == "Raw Matrix" else 180
                    if col in numeric_columns:
                        col_def["type"] = "numericColumn"
                    column_defs.append(col_def)

                slug_base = f"{title}-{path.stem}"
                slug = ''.join(ch.lower() if ch.isalnum() else '-' for ch in slug_base)
                while '--' in slug:
                    slug = slug.replace('--', '-')
                slug = slug.strip('-') or 'preview'
                grid = _make_ag_grid(
                    grid_id=f"example-preview-{slug}",
                    column_defs=column_defs,
                    row_data=row_records,
                    default_col_def={"minWidth": 150},
                    grid_options={"suppressPaginationPanel": True},
                )
                size = path.stat().st_size if path.exists() else 0
                meta = f"{row_count} rows × {col_count} cols"
                preview_scope = f"showing first {min(row_count, row_limit)} rows and first {min(col_count, col_limit)} cols"
                return dbc.Card([
                    dbc.CardHeader(html.Strong(f"{title} — {meta} ({human_size(size)}) — {preview_scope}")),
                    dbc.CardBody(
                        html.Div(
                            grid,
                            style={"overflowX": "auto"}
                        )
                    ),
                ], className="mb-2")
            except Exception:
                return html.Div(f"{title}: failed to preview", className="text-danger")

        cards = []
        if meta_p.exists():
            cards.append(_table_for(meta_p, "Metadata"))
        if raw_p.exists():
            cards.append(_table_for(raw_p, "Raw Matrix"))
        if not cards:
            return html.Div("No preview available.", className="text-muted")
        return html.Div(cards)
