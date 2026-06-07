from __future__ import annotations

import json
import zipfile
from datetime import datetime
from pathlib import Path
from typing import Iterable


def _iter_files(session_dir: Path) -> Iterable[Path]:
    for path in sorted(session_dir.rglob("*")):
        if path.is_file():
            yield path


def write_output_summary(session_dir: Path) -> Path:
    files = list(_iter_files(session_dir))
    summary = {
        "generated_at": datetime.now().isoformat(timespec="seconds"),
        "session_id": session_dir.name,
        "file_count": len(files),
        "files": [
            {
                "path": str(path.relative_to(session_dir)),
                "size_bytes": path.stat().st_size,
            }
            for path in files
        ],
    }
    path = session_dir / "output_summary.json"
    path.write_text(json.dumps(summary, indent=2), encoding="utf-8")
    return path


def write_run_config(session_dir: Path) -> Path:
    config = {}
    cfg_path = session_dir / "session_config.json"
    if cfg_path.exists():
        try:
            config = json.loads(cfg_path.read_text(encoding="utf-8"))
        except json.JSONDecodeError:
            config = {}
    payload = {
        "schema": "mbatchnet_reproducibility_bundle",
        "generated_at": datetime.now().isoformat(timespec="seconds"),
        "session_id": session_dir.name,
        "settings": config,
    }
    path = session_dir / "run_config.json"
    path.write_text(json.dumps(payload, indent=2), encoding="utf-8")
    return path


def build_output_bundle(session_dir: Path) -> Path:
    write_output_summary(session_dir)
    bundle = session_dir / "mbatchnet_output_bundle.zip"
    with zipfile.ZipFile(bundle, "w", compression=zipfile.ZIP_DEFLATED) as archive:
        for path in _iter_files(session_dir):
            if path == bundle or path.name == "mbatchnet_reproducibility_bundle.zip":
                continue
            archive.write(path, path.relative_to(session_dir))
    return bundle


def build_reproducibility_bundle(session_dir: Path) -> Path:
    write_run_config(session_dir)
    bundle = session_dir / "mbatchnet_reproducibility_bundle.zip"
    allowed = {
        "raw.csv",
        "metadata_origin.csv",
        "session_config.json",
        "validation_report.json",
        "run_config.json",
    }
    with zipfile.ZipFile(bundle, "w", compression=zipfile.ZIP_DEFLATED) as archive:
        for name in sorted(allowed):
            path = session_dir / name
            if path.exists():
                archive.write(path, path.name)
    return bundle

