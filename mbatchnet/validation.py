from __future__ import annotations

import csv
import json
import math
from dataclasses import dataclass, asdict
from pathlib import Path
from typing import Any, Iterable, Optional

import pandas as pd


@dataclass
class ValidationMessage:
    level: str
    code: str
    message: str


@dataclass
class ValidationReport:
    status: str
    n_samples: int
    n_features: int
    metadata_columns: list[str]
    messages: list[ValidationMessage]

    def to_dict(self) -> dict[str, Any]:
        payload = asdict(self)
        payload["messages"] = [asdict(item) for item in self.messages]
        return payload


def _msg(level: str, code: str, message: str) -> ValidationMessage:
    return ValidationMessage(level=level, code=code, message=message)


def _read_matrix(path: Path) -> pd.DataFrame:
    # Current application contract: one sample per row and features in columns.
    return pd.read_csv(path, header=None)


def _read_metadata(path: Path) -> pd.DataFrame:
    return pd.read_csv(path)


def _unique_nonempty(series: pd.Series) -> list[str]:
    values = []
    for value in series.dropna().astype(str).map(str.strip):
        if value and value not in values:
            values.append(value)
    return values


def validate_uploaded_inputs(
    raw_path: Path,
    metadata_path: Path,
    *,
    batch_column: Optional[str] = None,
    target_column: Optional[str] = None,
) -> ValidationReport:
    messages: list[ValidationMessage] = []
    n_samples = 0
    n_features = 0
    metadata_columns: list[str] = []

    try:
        matrix = _read_matrix(raw_path)
    except Exception as exc:
        return ValidationReport(
            status="error",
            n_samples=0,
            n_features=0,
            metadata_columns=[],
            messages=[_msg("error", "matrix_read_failed", f"Count matrix could not be read as CSV: {exc}")],
        )

    try:
        metadata = _read_metadata(metadata_path)
    except Exception as exc:
        return ValidationReport(
            status="error",
            n_samples=int(matrix.shape[0]),
            n_features=int(matrix.shape[1]),
            metadata_columns=[],
            messages=[_msg("error", "metadata_read_failed", f"Metadata could not be read as CSV: {exc}")],
        )

    n_samples = int(matrix.shape[0])
    n_features = int(matrix.shape[1])
    metadata_columns = [str(col) for col in metadata.columns]

    if n_samples < 2 or n_features < 2:
        messages.append(_msg("error", "matrix_too_small", "Count matrix must contain at least 2 samples and 2 features."))

    matrix_numeric = matrix.apply(pd.to_numeric, errors="coerce")
    if matrix_numeric.isna().any().any():
        messages.append(_msg("error", "matrix_non_numeric_or_missing", "Count matrix must be fully numeric and contain no missing values."))
    else:
        values = matrix_numeric.to_numpy(dtype=float)
        if not math.isfinite(float(values.sum())):
            messages.append(_msg("error", "matrix_non_finite", "Count matrix contains Inf or NaN values."))
        if (values < 0).any():
            messages.append(_msg("warning", "matrix_negative_values", "Negative values were detected; only transformed abundance inputs should contain negatives."))
        zero_sample_count = int((matrix_numeric.sum(axis=1) == 0).sum())
        if zero_sample_count:
            messages.append(_msg("error", "all_zero_samples", f"{zero_sample_count} sample row(s) contain only zeros."))
        zero_feature_count = int((matrix_numeric.sum(axis=0) == 0).sum())
        if zero_feature_count:
            messages.append(_msg("warning", "all_zero_features", f"{zero_feature_count} all-zero feature column(s) will not contribute to correction."))
        sparsity = float((matrix_numeric == 0).sum().sum()) / float(max(1, matrix_numeric.size))
        if sparsity >= 0.8:
            messages.append(_msg("warning", "high_sparsity", f"Input sparsity is high ({sparsity:.1%}); inspect correction results carefully."))

    if n_samples != int(metadata.shape[0]):
        messages.append(_msg("error", "sample_count_mismatch", "Matrix row count must match metadata row count."))

    if "sample_id" in metadata.columns:
        dup_count = int(metadata["sample_id"].duplicated().sum())
        if dup_count:
            messages.append(_msg("error", "duplicate_sample_id", f"Metadata contains {dup_count} duplicated sample_id value(s)."))

    for label, column in (("batch", batch_column), ("target", target_column)):
        if column:
            if column not in metadata.columns:
                messages.append(_msg("error", f"{label}_column_missing", f"Selected {label} column is not present in metadata."))
            elif metadata[column].isna().any() or (metadata[column].astype(str).map(str.strip) == "").any():
                messages.append(_msg("error", f"{label}_missing_values", f"Selected {label} column contains missing values."))
            else:
                levels = _unique_nonempty(metadata[column])
                if len(levels) < 2:
                    messages.append(_msg("error", f"{label}_needs_two_levels", f"Selected {label} column must contain at least two levels."))

    if batch_column and target_column and batch_column == target_column:
        messages.append(_msg("error", "batch_target_same_column", "Batch and target must be mapped to different metadata columns."))

    if batch_column and target_column and batch_column in metadata.columns and target_column in metadata.columns:
        contingency = pd.crosstab(metadata[batch_column].astype(str), metadata[target_column].astype(str))
        if contingency.shape[0] >= 2 and contingency.shape[1] >= 2:
            pure_rows = int((contingency > 0).sum(axis=1).eq(1).sum())
            pure_cols = int((contingency > 0).sum(axis=0).eq(1).sum())
            if pure_rows or pure_cols:
                messages.append(
                    _msg(
                        "warning",
                        "batch_target_confounding",
                        "Batch and target labels show possible confounding; some methods may fail or over-correct biological signal.",
                    )
                )

    status = "error" if any(item.level == "error" for item in messages) else "warning" if messages else "ok"
    return ValidationReport(status=status, n_samples=n_samples, n_features=n_features, metadata_columns=metadata_columns, messages=messages)


def write_validation_report(session_dir: Path, report: ValidationReport) -> Path:
    path = session_dir / "validation_report.json"
    path.write_text(json.dumps(report.to_dict(), indent=2), encoding="utf-8")
    return path


def standardize_metadata(session_dir: Path, *, batch_column: str, target_column: str, covariates: Optional[Iterable[str]] = None) -> None:
    meta_path = session_dir / "metadata_origin.csv"
    with meta_path.open("r", encoding="utf-8", newline="") as handle:
        reader = csv.DictReader(handle)
        original = reader.fieldnames or []
        rows = list(reader)

    renamed = ["batch" if col == batch_column else col for col in original]
    with meta_path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=renamed)
        writer.writeheader()
        for row in rows:
            writer.writerow({new: row.get(old) for old, new in zip(original, renamed)})

    config = {
        "label_column": target_column,
        "target_binary_column": "target_binary",
        "batch_column": "batch",
        "original_batch_column": batch_column,
        "original_target_column": target_column,
        "covariates": list(covariates or []),
    }
    (session_dir / "session_config.json").write_text(json.dumps(config, indent=2), encoding="utf-8")
