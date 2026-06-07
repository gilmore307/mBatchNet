from __future__ import annotations

import csv
import os
import shutil
from pathlib import Path
from typing import Any
from urllib.parse import unquote

from flask import Flask, abort, redirect, render_template, request, send_file, send_from_directory, url_for

from mbatchnet.artifacts import build_output_bundle, build_reproducibility_bundle, write_output_summary
from mbatchnet.content import ASSESSMENT_OVERVIEW, HELP_SECTIONS, METHOD_GUIDANCE, METRIC_GUIDES, UPLOAD_GUIDANCE
from mbatchnet.jobs import runner
from mbatchnet.methods import PARAMETER_CONFIG, load_methods
from mbatchnet.paths import ASSETS_DIR, EXAMPLE_DIR
from mbatchnet.runtime import cleanup_old_sessions, get_session_dir, new_session_id, run_method, run_preprocess
from mbatchnet.validation import standardize_metadata, validate_uploaded_inputs, write_validation_report
from mbatchnet.workflow import (
    assessment_by_key,
    assessments_for,
    display_name_for,
    list_files,
    method_status,
    run_all_assessments,
    run_assessment,
    run_correction_method,
    stage_files,
)


app = Flask(__name__, static_folder=None)
app.config["MAX_CONTENT_LENGTH"] = 128 * 1024 * 1024

EXAMPLE_COLUMN_MAP = {
    "batch": "Batch",
    "target": "Initial Phenol Concentration",
    "covariates": ["Treatment Duration"],
}
DEFAULT_PREVIEW_ROWS = 5
DEFAULT_PREVIEW_COLUMNS = 50
MAX_PREVIEW_ROWS = 50
MAX_PREVIEW_COLUMNS = 100


def _example_pair() -> tuple[Path, Path]:
    raw = EXAMPLE_DIR / "raw_ad.csv"
    meta = EXAMPLE_DIR / "metadata_ad.csv"
    if not raw.exists() or not meta.exists():
        abort(404)
    return raw, meta


def _session(session_id: str) -> Path:
    return get_session_dir(session_id)


def _bounded_int(value: str | None, default: int, maximum: int) -> int:
    try:
        parsed = int(value or default)
    except (TypeError, ValueError):
        parsed = default
    return max(1, min(parsed, maximum))


def _preview_csv(path: Path, *, rows: int, columns: int, header: bool) -> dict[str, Any] | None:
    if not path.exists():
        return None
    with path.open("r", encoding="utf-8-sig", newline="") as handle:
        reader = csv.reader(handle)
        if header:
            try:
                source_columns = [str(item) for item in next(reader)]
            except StopIteration:
                source_columns = []
            preview_columns = source_columns[:columns]
            preview_rows = [[str(item) for item in row[:columns]] for _, row in zip(range(rows), reader)]
            total_columns = len(source_columns)
        else:
            preview_rows = [[str(item) for item in row[:columns]] for _, row in zip(range(rows), reader)]
            total_columns = max((len(row) for row in preview_rows), default=0)
            preview_columns = [f"Feature {index + 1}" for index in range(min(columns, total_columns))]
    return {
        "columns": preview_columns,
        "rows": preview_rows,
        "total_columns": total_columns,
        "truncated_columns": total_columns > len(preview_columns),
    }


def _selected_columns(report) -> dict[str, Any]:
    columns = set(report.metadata_columns)
    return {
        "batch": "Batch" if "Batch" in columns else "batch" if "batch" in columns else "",
        "target": "Initial Phenol Concentration" if "Initial Phenol Concentration" in columns else "phenotype" if "phenotype" in columns else "",
        "covariates": ["Treatment Duration"] if "Treatment Duration" in columns else [],
    }


def _session_upload_context(session_id: str | None, *, rows: int, columns: int) -> dict[str, Any]:
    if not session_id:
        return {}
    session_dir = _session(session_id)
    raw_path = session_dir / "raw.csv"
    metadata_path = session_dir / "metadata_origin.csv"
    if not raw_path.exists() or not metadata_path.exists():
        return {"session_id": session_id, "session_dir": session_dir}

    report = validate_uploaded_inputs(raw_path, metadata_path)
    write_validation_report(session_dir, report)
    return {
        "session_id": session_id,
        "session_dir": session_dir,
        "report": report,
        "selected": _selected_columns(report),
        "raw_preview": _preview_csv(raw_path, rows=rows, columns=columns, header=False),
        "metadata_preview": _preview_csv(metadata_path, rows=rows, columns=columns, header=True),
    }


def _job_next_url(job) -> str:
    kind = job.kind or ""
    if kind == "preprocess":
        return url_for("pre_session", session_id=job.session_id)
    if kind.startswith("assessment:"):
        parts = kind.split(":")
        stage = parts[1] if len(parts) > 1 else "pre"
        return url_for("post_session" if stage == "post" else "pre_session", session_id=job.session_id)
    if kind.startswith("method:"):
        return url_for("correction_session", session_id=job.session_id)
    return url_for("session_results", session_id=job.session_id)


def _coerce_params(form) -> dict[str, object]:
    params: dict[str, object] = {}
    for key, value in form.items():
        if not key.startswith("param:"):
            continue
        name = key.split(":", 1)[1]
        if value in ("", None):
            continue
        if value in {"true", "false"}:
            params[name] = value == "true"
            continue
        try:
            params[name] = float(value) if "." in value else int(value)
        except ValueError:
            params[name] = value
    return params


@app.route("/assets/<path:filename>")
def assets(filename: str):
    return send_from_directory(ASSETS_DIR, filename)


@app.route("/")
def home():
    cleanup_old_sessions()
    return render_template("home.html", active_path="/")


@app.get("/upload")
def upload():
    active_tab = request.args.get("tab", "manual")
    preview_rows = _bounded_int(request.args.get("preview_rows"), DEFAULT_PREVIEW_ROWS, MAX_PREVIEW_ROWS)
    preview_columns = _bounded_int(request.args.get("preview_columns"), DEFAULT_PREVIEW_COLUMNS, MAX_PREVIEW_COLUMNS)
    raw, meta = _example_pair()
    session_context = _session_upload_context(
        request.args.get("session_id"),
        rows=preview_rows,
        columns=preview_columns,
    )
    return render_template(
        "upload.html",
        active_path="/upload",
        active_tab=active_tab,
        upload_guidance=UPLOAD_GUIDANCE,
        preview_rows=preview_rows,
        preview_columns=preview_columns,
        example_mapping=EXAMPLE_COLUMN_MAP,
        example_raw_preview=_preview_csv(raw, rows=preview_rows, columns=preview_columns, header=False),
        example_metadata_preview=_preview_csv(meta, rows=preview_rows, columns=preview_columns, header=True),
        **session_context,
    )


@app.get("/help")
def help_page():
    return render_template(
        "help.html",
        active_path="/",
        help_sections=HELP_SECTIONS,
        metric_guides=METRIC_GUIDES,
        method_guidance=METHOD_GUIDANCE,
    )


@app.get("/pre")
def pre_placeholder():
    return render_template(
        "workflow_placeholder.html",
        active_path="/pre",
        title="Pre-correction Assessment",
        description="Inspect QC summaries, ordination structure, and batch-associated variation before correction.",
    )


@app.get("/correction")
def correction_placeholder():
    return render_template(
        "workflow_placeholder.html",
        active_path="/correction",
        title="Batch Effect Correction",
        description="Run supported batch-effect correction methods with curated defaults or custom parameters.",
    )


@app.get("/post")
def post_placeholder():
    return render_template(
        "workflow_placeholder.html",
        active_path="/post",
        title="Post-correction Assessment",
        description="Compare diagnostics before and after correction, then export processed matrices and reports.",
    )


@app.post("/sessions/example")
def create_example_session():
    session_id = new_session_id()
    session_dir = get_session_dir(session_id)
    raw, meta = _example_pair()
    shutil.copy2(raw, session_dir / "raw.csv")
    shutil.copy2(meta, session_dir / "metadata_origin.csv")
    return redirect(url_for("upload", tab="example", session_id=session_id))


@app.post("/sessions/upload")
def create_uploaded_session():
    matrix = request.files.get("matrix")
    metadata = request.files.get("metadata")
    if not matrix or not metadata:
        abort(400)
    session_id = new_session_id()
    session_dir = get_session_dir(session_id)
    matrix.save(session_dir / "raw.csv")
    metadata.save(session_dir / "metadata_origin.csv")
    return redirect(url_for("upload", session_id=session_id))


@app.get("/sessions/<session_id>/configure")
def configure_session(session_id: str):
    session_dir = _session(session_id)
    report = validate_uploaded_inputs(session_dir / "raw.csv", session_dir / "metadata_origin.csv")
    write_validation_report(session_dir, report)
    selected = {
        "batch": "Batch" if "Batch" in report.metadata_columns else "batch" if "batch" in report.metadata_columns else "",
        "target": "Initial Phenol Concentration" if "Initial Phenol Concentration" in report.metadata_columns else "",
        "covariates": ["Treatment Duration"] if "Treatment Duration" in report.metadata_columns else [],
    }
    return render_template(
        "configure.html",
        active_path="/upload",
        session_id=session_id,
        report=report,
        selected=selected,
    )


@app.post("/sessions/<session_id>/preprocess")
def preprocess_session(session_id: str):
    session_dir = _session(session_id)
    batch_column = request.form.get("batch_column", "")
    target_column = request.form.get("target_column", "")
    covariates = [value for value in request.form.getlist("covariates") if value and value not in {batch_column, target_column}]
    report = validate_uploaded_inputs(
        session_dir / "raw.csv",
        session_dir / "metadata_origin.csv",
        batch_column=batch_column,
        target_column=target_column,
    )
    write_validation_report(session_dir, report)
    if report.status == "error":
        return (
            render_template(
                "configure.html",
                active_path="/upload",
                session_id=session_id,
                report=report,
                selected={"batch": batch_column, "target": target_column, "covariates": covariates},
            ),
            400,
        )
    standardize_metadata(session_dir, batch_column=batch_column, target_column=target_column, covariates=covariates)
    job = runner.submit("preprocess", session_id, lambda: run_preprocess(session_dir))
    return redirect(url_for("job_status_page", job_id=job.id))


@app.get("/jobs/<job_id>")
def job_status_page(job_id: str):
    job = runner.get(job_id)
    if job is None:
        abort(404)
    return render_template("job.html", active_path="/upload", job=job, next_url=_job_next_url(job))


@app.get("/jobs/<job_id>.json")
def job_status_json(job_id: str):
    job = runner.get(job_id)
    if job is None:
        abort(404)
    return job.to_dict()


@app.get("/sessions/<session_id>")
def session_results(session_id: str):
    session_dir = _session(session_id)
    write_output_summary(session_dir)
    methods = load_methods()
    files = list_files(session_dir)
    return render_template(
        "results.html",
        active_path="/correction",
        session_id=session_id,
        methods=methods,
        files=files,
    )


@app.get("/sessions/<session_id>/pre")
def pre_session(session_id: str):
    session_dir = _session(session_id)
    return render_template(
        "assessment.html",
        active_path="/pre",
        session_id=session_id,
        stage="pre",
        title="Pre-correction Assessment",
        description="Run diagnostics before correction to understand batch-associated structure and phenotype separation.",
        assessments=assessments_for("pre"),
        files=stage_files(session_dir, "pre"),
        assessment_overview=ASSESSMENT_OVERVIEW,
        metric_guides=METRIC_GUIDES,
    )


@app.get("/sessions/<session_id>/correction")
def correction_session(session_id: str):
    session_dir = _session(session_id)
    return render_template(
        "correction.html",
        active_path="/correction",
        session_id=session_id,
        rows=method_status(session_dir),
        parameter_config=PARAMETER_CONFIG,
        method_guidance=METHOD_GUIDANCE,
    )


@app.get("/sessions/<session_id>/post")
def post_session(session_id: str):
    session_dir = _session(session_id)
    return render_template(
        "assessment.html",
        active_path="/post",
        session_id=session_id,
        stage="post",
        title="Post-correction Assessment",
        description="Run post-correction diagnostics after at least one correction method has generated outputs.",
        assessments=assessments_for("post"),
        files=stage_files(session_dir, "post"),
        assessment_overview=ASSESSMENT_OVERVIEW,
        metric_guides=METRIC_GUIDES,
    )


@app.post("/sessions/<session_id>/assessment/<stage>/<key>")
def run_assessment_route(session_id: str, stage: str, key: str):
    if assessment_by_key(stage, key) is None:
        abort(404)
    session_dir = _session(session_id)
    job = runner.submit(f"assessment:{stage}:{key}", session_id, lambda: run_assessment(session_dir, stage, key))
    return redirect(url_for("job_status_page", job_id=job.id))


@app.post("/sessions/<session_id>/assessment/<stage>/all")
def run_all_assessments_route(session_id: str, stage: str):
    if stage not in {"pre", "post"}:
        abort(404)
    session_dir = _session(session_id)
    job = runner.submit(f"assessment:{stage}:all", session_id, lambda: run_all_assessments(session_dir, stage))
    return redirect(url_for("job_status_page", job_id=job.id))


@app.post("/sessions/<session_id>/methods/<method_code>")
def run_method_route(session_id: str, method_code: str):
    session_dir = _session(session_id)
    params = _coerce_params(request.form)
    job = runner.submit(
        f"method:{method_code}",
        session_id,
        lambda: run_correction_method(session_dir, method_code, params=params),
    )
    return redirect(url_for("job_status_page", job_id=job.id))


@app.post("/sessions/<session_id>/methods/<method_code>/delete")
def delete_method_route(session_id: str, method_code: str):
    session_dir = _session(session_id)
    aliases = {
        "BMC": "normalized_bmc",
        "limma": "normalized_limma",
        "ConQuR": "normalized_conqur",
        "PLSDA": "normalized_plsda",
        "ComBat": "normalized_combat",
        "FSQN": "normalized_fsqn",
        "MMUPHin": "normalized_mmuphin",
        "RUV": "normalized_ruv",
        "MetaDICT": "normalized_metadict",
        "FAbatch": "normalized_fabatch",
        "ComBatSeq": "normalized_combatseq",
        "DEBIAS": "normalized_debias",
    }
    basename = aliases.get(method_code, f"normalized_{method_code.lower()}")
    for suffix in (".csv", "_tss.csv", "_clr.csv"):
        path = session_dir / f"{basename}{suffix}"
        path.unlink(missing_ok=True)
    return redirect(url_for("correction_session", session_id=session_id))


@app.get("/sessions/<session_id>/logs")
def download_log(session_id: str):
    log_path = _session(session_id) / "run.log"
    if not log_path.exists():
        abort(404)
    return send_file(log_path, as_attachment=False)


@app.get("/sessions/<session_id>/files/<path:relative_path>")
def download_session_file(session_id: str, relative_path: str):
    session_dir = _session(session_id).resolve()
    target = (session_dir / unquote(relative_path)).resolve()
    if not str(target).startswith(str(session_dir)) or not target.exists() or not target.is_file():
        abort(404)
    return send_file(target, as_attachment=False)


@app.get("/sessions/<session_id>/download/output")
def download_output_bundle(session_id: str):
    return send_file(build_output_bundle(_session(session_id)), as_attachment=True)


@app.get("/sessions/<session_id>/download/reproducibility")
def download_reproducibility_bundle(session_id: str):
    return send_file(build_reproducibility_bundle(_session(session_id)), as_attachment=True)


if __name__ == "__main__":
    app.run(host="0.0.0.0", port=int(os.environ.get("PORT", "8050")), debug=False)
