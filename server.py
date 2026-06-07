from __future__ import annotations

import shutil
import os
from pathlib import Path

from flask import Flask, abort, redirect, render_template, request, send_file, send_from_directory, url_for

from mbatchnet.artifacts import build_output_bundle, build_reproducibility_bundle, write_output_summary
from mbatchnet.jobs import runner
from mbatchnet.methods import load_methods
from mbatchnet.paths import ASSETS_DIR, EXAMPLE_DIR
from mbatchnet.runtime import cleanup_old_sessions, get_session_dir, new_session_id, run_method, run_preprocess
from mbatchnet.validation import standardize_metadata, validate_uploaded_inputs, write_validation_report


app = Flask(__name__, static_folder=None)
app.config["MAX_CONTENT_LENGTH"] = 128 * 1024 * 1024


def _example_pair() -> tuple[Path, Path]:
    raw = EXAMPLE_DIR / "raw_ad.csv"
    meta = EXAMPLE_DIR / "metadata_ad.csv"
    if not raw.exists() or not meta.exists():
        abort(404)
    return raw, meta


def _session(session_id: str) -> Path:
    return get_session_dir(session_id)


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
    return render_template("upload.html", active_path="/upload", active_tab=active_tab)


@app.get("/help")
def help_page():
    return render_template("help.html", active_path="/")


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
    return redirect(url_for("configure_session", session_id=session_id, example="1"))


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
    return redirect(url_for("configure_session", session_id=session_id))


@app.get("/sessions/<session_id>/configure")
def configure_session(session_id: str):
    session_dir = _session(session_id)
    report = validate_uploaded_inputs(session_dir / "raw.csv", session_dir / "metadata_origin.csv")
    write_validation_report(session_dir, report)
    selected = {
        "batch": "Batch" if "Batch" in report.metadata_columns else "batch" if "batch" in report.metadata_columns else "",
        "target": "Initial Phenol Concentration" if "Initial Phenol Concentration" in report.metadata_columns else "",
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
                selected={"batch": batch_column, "target": target_column},
            ),
            400,
        )
    standardize_metadata(session_dir, batch_column=batch_column, target_column=target_column)
    job = runner.submit("preprocess", session_id, lambda: run_preprocess(session_dir))
    return redirect(url_for("job_status_page", job_id=job.id))


@app.get("/jobs/<job_id>")
def job_status_page(job_id: str):
    job = runner.get(job_id)
    if job is None:
        abort(404)
    return render_template("job.html", active_path="/upload", job=job)


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
    files = sorted(path.name for path in session_dir.iterdir() if path.is_file())
    return render_template(
        "results.html",
        active_path="/correction",
        session_id=session_id,
        methods=methods,
        files=files,
    )


@app.post("/sessions/<session_id>/methods/<method_code>")
def run_method_route(session_id: str, method_code: str):
    session_dir = _session(session_id)
    job = runner.submit(f"method:{method_code}", session_id, lambda: run_method(session_dir, method_code))
    return redirect(url_for("job_status_page", job_id=job.id))


@app.get("/sessions/<session_id>/logs")
def download_log(session_id: str):
    log_path = _session(session_id) / "run.log"
    if not log_path.exists():
        abort(404)
    return send_file(log_path, as_attachment=False)


@app.get("/sessions/<session_id>/download/output")
def download_output_bundle(session_id: str):
    return send_file(build_output_bundle(_session(session_id)), as_attachment=True)


@app.get("/sessions/<session_id>/download/reproducibility")
def download_reproducibility_bundle(session_id: str):
    return send_file(build_reproducibility_bundle(_session(session_id)), as_attachment=True)


if __name__ == "__main__":
    app.run(host="0.0.0.0", port=int(os.environ.get("PORT", "8050")), debug=False)
