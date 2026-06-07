from __future__ import annotations

import os
import subprocess
import sys
import time
import uuid
from datetime import datetime
from pathlib import Path
from typing import Iterable, Optional, Sequence

from .paths import BASE_DIR, OUTPUT_ROOT, PREPROCESS_SCRIPT, METHODS_DIR


RSCRIPT_BASE_COMMAND = (
    "Rscript",
    "--no-save",
    "--no-restore",
    "--no-site-file",
)


def cleanup_old_sessions(max_age_hours: int = 6) -> None:
    if not OUTPUT_ROOT.exists():
        return
    cutoff = time.time() - max_age_hours * 3600
    for session_dir in OUTPUT_ROOT.iterdir():
        try:
            if session_dir.is_dir() and session_dir.stat().st_mtime < cutoff:
                import shutil

                shutil.rmtree(session_dir, ignore_errors=True)
        except OSError:
            continue


def new_session_id() -> str:
    return str(uuid.uuid4())


def get_session_dir(session_id: str) -> Path:
    OUTPUT_ROOT.mkdir(parents=True, exist_ok=True)
    session_dir = OUTPUT_ROOT / session_id
    session_dir.mkdir(parents=True, exist_ok=True)
    return session_dir


def build_rscript_command(script_path: Path, *args: object) -> tuple[str, ...]:
    return RSCRIPT_BASE_COMMAND + (str(script_path),) + tuple(str(arg) for arg in args)


def append_run_log(log_path: Path, message: str, icon: str = "") -> None:
    stamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    prefix = f"{icon} " if icon else ""
    log_path.parent.mkdir(parents=True, exist_ok=True)
    with log_path.open("a", encoding="utf-8", errors="replace") as handle:
        handle.write(f"[{stamp}] {prefix}{str(message).rstrip()}\n")


def _subprocess_env(command: Sequence[str]) -> dict[str, str]:
    env = os.environ.copy()
    env.setdefault("RETICULATE_PYTHON", sys.executable)
    if command and "rscript" in Path(str(command[0])).name.lower():
        null_target = "NUL" if os.name == "nt" else "/dev/null"
        env.setdefault("R_DEFAULT_DEVICE", "pdf")
        env.setdefault("R_PDF_DEFAULT", null_target)
    return env


def run_command_streaming(command: Sequence[str], cwd: Path, log_path: Path) -> bool:
    append_run_log(log_path, "$ " + " ".join(command), icon=">")
    proc = subprocess.Popen(
        command,
        cwd=cwd,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        encoding="utf-8",
        errors="replace",
        env=_subprocess_env(command),
    )
    assert proc.stdout is not None
    with log_path.open("a", encoding="utf-8", errors="replace") as handle:
        for line in proc.stdout:
            handle.write(line)
            handle.flush()
    return proc.wait() == 0


def run_preprocess(session_dir: Path) -> bool:
    command = build_rscript_command(PREPROCESS_SCRIPT, session_dir, session_dir / "raw.csv")
    return run_command_streaming(command, BASE_DIR, session_dir / "run.log")


def normalize_method_code(value: str) -> str:
    return "".join(ch.lower() for ch in str(value or "") if ch.isalnum())


def resolve_method_script(method: str) -> Optional[Path]:
    key = normalize_method_code(method)
    for script in METHODS_DIR.glob("*.R"):
        if normalize_method_code(script.stem) == key:
            return script
    return None


def run_method(session_dir: Path, method: str, params: Optional[dict[str, object]] = None) -> bool:
    script = resolve_method_script(method)
    if script is None:
        append_run_log(session_dir / "run.log", f"Unknown method: {method}", icon="ERROR")
        return False
    flags = []
    for key, value in (params or {}).items():
        if value is None or value == "":
            continue
        flags.append(f"--{key}={str(value).lower() if isinstance(value, bool) else value}")
    command = build_rscript_command(script, session_dir, *flags)
    return run_command_streaming(command, BASE_DIR, session_dir / "run.log")


def run_scripts(session_dir: Path, scripts: Iterable[Path]) -> bool:
    ok = True
    for script in scripts:
        command = build_rscript_command(script, session_dir)
        ok = run_command_streaming(command, BASE_DIR, session_dir / "run.log") and ok
    return ok

