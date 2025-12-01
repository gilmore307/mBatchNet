# ===============================
# File: _2_utils.py
# ===============================
import base64
import csv
import json
import os
import re
import sys
import shutil
import subprocess
import textwrap
import time
import threading
from io import BytesIO
from datetime import datetime
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Sequence, Tuple, Optional, Set, Any

from dash import html, dcc
import dash_ag_grid as dag
import dash_bootstrap_components as dbc


# Paths & constants
BASE_DIR = Path(__file__).resolve().parent
OUTPUT_ROOT = BASE_DIR / "output"
PLOTS_DIR = BASE_DIR / "plots"
CORRECTION_DIR = BASE_DIR / "correction"
METHODS_DIR = CORRECTION_DIR / "methods"
DOCS_DIR = BASE_DIR / "assets" / "doc"
METHODS_REFERENCE_PATH = DOCS_DIR / "methods.csv"
PREPROCESS_SCRIPT = CORRECTION_DIR / "preprocess.R"
CLEANUP_HOURS = 6
PREVIEW_SENTINEL = ".previews_generated"


RSCRIPT_BASE_COMMAND: Tuple[str, ...] = (
    "Rscript",
    "--no-save",
    "--no-restore",
    "--no-site-file",
)


def build_rscript_command(script_path: Path, *args: object) -> Tuple[str, ...]:
    """Compose an Rscript command that keeps user profiles for non-interactive runs."""

    tail = tuple(str(arg) for arg in args)
    return RSCRIPT_BASE_COMMAND + (str(script_path),) + tail


def append_run_log(log_path: Path, message: str, icon: str = "ℹ️") -> str:
    """Append a timestamped, icon-prefixed line to the run log."""

    stamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    icon_part = f"{icon} " if icon else ""
    clean_msg = str(message).rstrip("\n")
    line = f"[{stamp}] {icon_part}{clean_msg}\n"

    if not log_path:
        return line
    try:
        log_path.parent.mkdir(parents=True, exist_ok=True)
        with log_path.open("a", encoding="utf-8", errors="replace") as logf:
            logf.write(line)
    except OSError:
        pass
    return line


def log_file_meta(log_path: Path | None) -> Optional[Dict[str, Any]]:
    """Return basic metadata for a log file if it exists."""

    if not log_path:
        return None
    try:
        stat = Path(log_path).stat()
    except OSError:
        return None
    return {
        "path": str(Path(log_path)),
        "size": stat.st_size,
        "mtime": stat.st_mtime,
    }


# ---- Dataclasses & config ----
@dataclass
class FigureSpec:
    label: str
    filename: str


PRE_FIGURES: Sequence[FigureSpec] = (
    FigureSpec("PCA (Batch grouping)", "pca_batch.tif"),
    FigureSpec("PCA (Target grouping)", "pca_target.tif"),
    FigureSpec("PCoA (Aitchison · batch)", "pcoa_aitchison_batch.tif"),
    FigureSpec("PCoA (Aitchison · target)", "pcoa_aitchison_target.tif"),
    FigureSpec("PCoA (Bray-Curtis · batch)", "pcoa_braycurtis_batch.tif"),
    FigureSpec("PCoA (Bray-Curtis · target)", "pcoa_braycurtis_target.tif"),
    FigureSpec("NMDS (Aitchison · batch)", "nmds_aitchison_batch.tif"),
    FigureSpec("NMDS (Aitchison · target)", "nmds_aitchison_target.tif"),
    FigureSpec("NMDS (Bray-Curtis · batch)", "nmds_braycurtis_batch.tif"),
    FigureSpec("NMDS (Bray-Curtis · target)", "nmds_braycurtis_target.tif"),
    FigureSpec("Dissimilarity heatmaps (Aitchison)", "dissimilarity_heatmaps_aitchison.tif"),
    FigureSpec("Dissimilarity heatmaps (Bray-Curtis)", "dissimilarity_heatmaps_braycurtis.tif"),
    FigureSpec("PERMANOVA R² (Aitchison)", "permanova_aitchison.tif"),
    FigureSpec("PERMANOVA R² (Bray-Curtis)", "permanova_braycurtis.tif"),
    FigureSpec("Feature-wise ANOVA R² (Aitchison)", "anova_aitchison.tif"),
    FigureSpec("pRDA (Aitchison)", "pRDA_aitchison.tif"),
    FigureSpec("PVCA", "PVCA.tif"),
)

POST_EXTRA_FIGURES: Sequence[FigureSpec] = (
    FigureSpec("Alignment score", "alignment_score.tif"),
    FigureSpec("Entropy score", "ebm.tif"),
    FigureSpec("Silhouette score", "silhouette.tif"),
)

POST_FIGURES: Sequence[FigureSpec] = PRE_FIGURES + POST_EXTRA_FIGURES


PRE_SCRIPTS: Sequence[str] = (
    "pca.R",
    "pcoa.R",
    "NMDS.R",
    "Dissimilarity_Heatmaps.R",
    "PERMANOVA.R",
    "ANOVA.R",
    "pRDA.R",
    "pvca.R",
)

POST_SCRIPTS: Sequence[str] = PRE_SCRIPTS + (
    "Alignment_Score.R",
    "Entropy_Score.R",
    "Silhouette.R",
)


PNG_MAX_DISPLAY_SIDE = 1400
# Downscale all PNG previews to a uniform 1400px longest side

def _guess_mime_type(path: Path) -> str:
    ext = path.suffix.lower()
    if ext == ".png":
        return "image/png"
    if ext in {".jpg", ".jpeg"}:
        return "image/jpeg"
    if ext in {".tif", ".tiff"}:
        return "image/tiff"
    return "application/octet-stream"


def _generate_png_preview(
    source_path: Path,
    *,
    max_side: int,
    preview_path: Optional[Path] = None,
    reuse_existing: bool = False,
    write_back: bool = False,
) -> bytes:
    """Return PNG bytes for a source image, optionally caching to disk.

    - If ``reuse_existing`` is True and a ``preview_path`` exists and is fresh
      (mtime not older than the source), the cached bytes are returned directly.
    - Otherwise, the source is opened with PIL, converted to RGBA, downscaled
      to ``max_side`` on the longest edge, and encoded as a compressed PNG.
    - When ``write_back`` is True, the generated bytes are written to
      ``preview_path`` if they are new or resized. For TIFF inputs this keeps a
      persistent preview without redundant rewrites; for PNG inputs it updates
      the on-disk file only when necessary.
    """

    if reuse_existing and preview_path is not None:
        try:
            if preview_path.exists() and preview_path.stat().st_mtime >= source_path.stat().st_mtime:
                return preview_path.read_bytes()
        except OSError:
            pass

    try:
        from PIL import Image

        with Image.open(source_path) as image:
            image = image.convert("RGBA")
            resample = getattr(Image, "Resampling", None)
            resample_method = resample.LANCZOS if resample else Image.LANCZOS

            original_size = image.size
            resized = max(original_size) > max_side
            if resized:
                image.thumbnail((max_side, max_side), resample_method)

            buffer = BytesIO()
            image.save(buffer, format="PNG", optimize=True, compress_level=9)
            data = buffer.getvalue()

        if preview_path is not None:
            try:
                if write_back:
                    needs_update = (
                        resized
                        or not preview_path.exists()
                        or preview_path.stat().st_size != len(data)
                    )
                    if needs_update:
                        preview_path.write_bytes(data)
                else:
                    preview_path.write_bytes(data)
            except OSError:
                pass

        return data
    except Exception:
        if preview_path is not None and preview_path.exists():
            try:
                return preview_path.read_bytes()
            except OSError:
                pass
        return source_path.read_bytes()


def _encode_image_source(path: Path, *, max_png_side: int = PNG_MAX_DISPLAY_SIDE) -> str:
    """Return a data URI for an image.

    PNGs are downscaled/compressed to ease frontend payload size and the
    rewritten file is saved back to disk so the user directory mirrors the
    preview size. TIFF inputs are converted and downscaled to a PNG preview
    that is also persisted on disk for reuse by the app.
    """

    ext = path.suffix.lower()
    mime_type = _guess_mime_type(path)
    if ext == ".png":
        data = _generate_png_preview(
            path,
            max_side=max_png_side,
            preview_path=path,
            reuse_existing=False,
            write_back=True,
        )
    elif ext in {".tif", ".tiff"}:
        png_path = path.with_suffix(".png")
        data = _generate_png_preview(
            path,
            max_side=max_png_side,
            preview_path=png_path,
            reuse_existing=True,
            write_back=True,
        )
        mime_type = "image/png"
    else:
        data = path.read_bytes()

    encoded = base64.b64encode(data).decode("ascii")
    return f"data:{mime_type};base64,{encoded}"


def _resolve_png_max_side(path: Path) -> int:
    """Return the downscale max side for a PNG, falling back to the default."""

    return PNG_MAX_DISPLAY_SIDE


def _materialize_png_sidecar(source_path: Path, *, max_side: int = PNG_MAX_DISPLAY_SIDE) -> Optional[Path]:
    """Ensure a PNG thumbnail exists for the provided TIFF image."""

    if source_path.suffix.lower() not in {".tif", ".tiff"}:
        return None

    png_path = source_path.with_suffix(".png")
    try:
        _generate_png_preview(
            source_path,
            max_side=max_side,
            preview_path=png_path,
            reuse_existing=True,
            write_back=True,
        )
    except Exception:
        pass
    return png_path if png_path.exists() else None


def _ensure_png_previews(directory: Path, *, max_side: int = PNG_MAX_DISPLAY_SIDE) -> None:
    """Create PNG thumbnails next to any TIFF figures in the directory."""

    try:
        for path in directory.iterdir():
            if path.is_file() and path.suffix.lower() in {".tif", ".tiff"}:
                _materialize_png_sidecar(path, max_side=max_side)
    except OSError:
        return


_PREVIEW_ATTEMPTED: Set[Path] = set()
_PREVIEW_LOCK = threading.Lock()


def ensure_png_previews(session_dir: Path, log_path: Optional[Path] = None) -> bool:
    """Run the preview R script once to generate PNG sidecars for TIFF figures."""

    marker = session_dir / PREVIEW_SENTINEL
    if marker.exists():
        return True

    resolved = session_dir.resolve()
    with _PREVIEW_LOCK:
        if resolved in _PREVIEW_ATTEMPTED:
            return marker.exists()
        _PREVIEW_ATTEMPTED.add(resolved)

    script_path = PLOTS_DIR / "preview.R"
    if not script_path.exists():
        append_run_log(log_path, f"Preview script missing: {script_path}", icon="⚠️")
        return False

    append_run_log(log_path, "Generating PNG previews from TIFF figures...", icon="🖼️")
    command = build_rscript_command(script_path, session_dir)
    if log_path is not None:
        success, _ = run_command_streaming(command, cwd=BASE_DIR, log_path=log_path)
    else:
        success, _ = run_command(command, cwd=BASE_DIR)

    if success:
        try:
            marker.touch()
        except OSError:
            pass
        append_run_log(log_path, "Preview generation complete.", icon="✅")
        return True

    append_run_log(log_path, "Preview generation failed; proceeding with TIFF files.", icon="⚠️")
    return False


_METHOD_DISPLAY_NAMES: Dict[str, str] = {
    "DEBIAS": "DEBIAS-M",
    "MetaDICT": "MetaDICT",
    "BMC": "BMC",
    "PLSDA": "PLSDA-batch",
    "ConQuR": "ConQuR",
    "MMUPHin": "MMUPHin",
    "RUV": "RUV-III-NB",
    "ComBatSeq": "ComBat-seq",
    "FSQN": "FSQN",
    "PN": "Percentile Normalization",
    "FAbatch": "FAbatch",
    "limma": "Limma",
    "ComBat": "ComBat",
    "QN": "Quantile Normalization",
}

_METHOD_NAME_TO_CODE: Dict[str, str] = {display: code for code, display in _METHOD_DISPLAY_NAMES.items()}


def _load_method_reference() -> Tuple[Tuple[Tuple[str, str], ...], Dict[str, Dict[str, str]]]:
    supported: List[Tuple[str, str]] = []
    reference: Dict[str, Dict[str, str]] = {}
    seen_codes: Set[str] = set()
    if METHODS_REFERENCE_PATH.exists():
        with open(METHODS_REFERENCE_PATH, newline="", encoding="utf-8-sig") as handle:
            reader = csv.DictReader(handle)
            for row in reader:
                method_name = (row.get("Methods") or "").strip()
                if not method_name:
                    continue
                citation = (row.get("Citations") or "").strip()
                url = (row.get("Url") or "").strip()
                package = (row.get("Package") or "").strip()
                reference[method_name] = {
                    "citation": citation,
                    "url": url,
                    "package": package,
                }
                code = _METHOD_NAME_TO_CODE.get(method_name)
                if code and code not in seen_codes:
                    supported.append((code, method_name))
                    seen_codes.add(code)
    for code, display in _METHOD_DISPLAY_NAMES.items():
        if code not in seen_codes:
            supported.append((code, display))
            reference.setdefault(display, {"citation": "", "url": "", "package": ""})
    return tuple(supported), reference


SUPPORTED_METHODS: Sequence[Tuple[str, str]]
METHOD_REFERENCE_BY_DISPLAY: Dict[str, Dict[str, str]]
SUPPORTED_METHODS, METHOD_REFERENCE_BY_DISPLAY = _load_method_reference()
SUPPORTED_METHODS = tuple(SUPPORTED_METHODS)
METHOD_REFERENCE_BY_DISPLAY = dict(METHOD_REFERENCE_BY_DISPLAY)

METHOD_REFERENCE_BY_CODE: Dict[str, Dict[str, str]] = {
    code: METHOD_REFERENCE_BY_DISPLAY.get(display, {"citation": "", "url": "", "package": ""})
    for code, display in SUPPORTED_METHODS
}


_DETAIL_METRIC_TRENDS: Dict[str, Sequence[tuple[str, str]]] = {
    "nmds": (
        ("NMDS_Stress", "down"),
    ),
    "dissimilarity": (
        ("ANOSIM_R", "down"),
    ),
    "permanova": (
        ("R²", "down"),
    ),
    "r2": (
        ("Median R² (Batch)", "down"),
        ("Median R² (Treatment)", "up"),
    ),
    "prda": (
        ("Treatment", "up"),
        ("Batch", "down"),
        ("Intersection", "down"),
        ("Residuals", "down"),
    ),
    "pvca": (
        ("Treatment", "up"),
        ("Batch", "down"),
        ("Intersection", "down"),
        ("Residuals", "down"),
    ),
    "alignment": (
        ("Alignment Score", "up"),
    ),
    "ebm": (
        ("EBM", "up"),
    ),
    "silhouette": (
        ("Silhouette", "up"),
    ),
}

_DETAIL_COLUMN_ALLOWLIST: Dict[str, Set[str]] = {
    "nmds": {"method", "nmds_stress"},
}

# NMDS assessment tables no longer carry a Geometry column; treat them as single-geometry
# when rendering details so the tables are still displayed.
_MULTI_GEOMETRY_DETAIL_KEYS: Set[str] = {"pcoa", "dissimilarity", "permanova"}
_AITCHISON_GEOMETRY_TOKENS: Set[str] = {"aitchison"}
_BRAY_GEOMETRY_TOKENS: Set[str] = {"braycurtis"}

def _normalize_method_code(value: str) -> str:
    return re.sub(r"[^a-z0-9]+", "", (value or "").lower())


def _discover_method_scripts() -> Dict[str, Path]:
    scripts: Dict[str, Path] = {}
    if not METHODS_DIR.exists():
        return scripts
    for script in METHODS_DIR.glob("*.R"):
        key = _normalize_method_code(script.stem)
        if key:
            scripts[key] = script
    return scripts


_METHOD_SCRIPT_INDEX: Dict[str, Path] = _discover_method_scripts()


def resolve_method_script(method: str) -> Optional[Path]:
    if not method:
        return None
    return _METHOD_SCRIPT_INDEX.get(_normalize_method_code(method))


# Convenience set listing codes whose backing scripts were not found during import.
MISSING_METHOD_SCRIPTS: Set[str] = {
    code for code, _ in SUPPORTED_METHODS if resolve_method_script(code) is None
}

# Map method identifiers (as they may appear in CSVs) to formal display names
_FORMAL_MAP: Dict[str, str] = {code.lower(): display for code, display in SUPPORTED_METHODS}
# Add common lowercase variants used by R outputs
_FORMAL_ALIASES: Dict[str, str] = {
    "qn": _FORMAL_MAP["qn"],
    "bmc": _FORMAL_MAP["bmc"],
    "limma": _FORMAL_MAP["limma"],
    "conqur": _FORMAL_MAP["conqur"],
    "plsda": _FORMAL_MAP["plsda"],
    "plsdabatch": _FORMAL_MAP["plsda"],
    "combat": _FORMAL_MAP["combat"],
    "fsqn": _FORMAL_MAP["fsqn"],
    "mmuphin": _FORMAL_MAP["mmuphin"],
    "ruv": _FORMAL_MAP["ruv"],
    "ruv-iiinb": _FORMAL_MAP["ruv"],
    "ruviiinb": _FORMAL_MAP["ruv"],
    "metadict": _FORMAL_MAP["metadict"],
    "meta-dict": _FORMAL_MAP["metadict"],
    "pn": _FORMAL_MAP["pn"],
    "percentilenormalization": _FORMAL_MAP["pn"],
    "fabatch": _FORMAL_MAP["fabatch"],
    "combatseq": _FORMAL_MAP["combatseq"],
    "combat-seq": _FORMAL_MAP["combatseq"],
    "debias": _FORMAL_MAP["debias"],
    "debias-m": _FORMAL_MAP["debias"],
}

def method_formal_name(name: str) -> str:
    if not name:
        return name
    key = (name or "").strip()
    # Keep special baseline label as-is
    if key.lower() == "before correction":
        return "Before correction"
    lk = key.replace(" ", "").replace("_", "").lower()
    return _FORMAL_ALIASES.get(lk, _FORMAL_MAP.get(lk, key))

# Default correction methods (codes corresponding to SUPPORTED_METHODS)
# Quantile normalization, Batch mean centering, Limma removeBatchEffect,
# Percentile normalization
DEFAULT_METHODS: Sequence[str] = ("QN", "BMC", "limma", "PN")

CODE_TO_DISPLAY: Dict[str, str] = {code: display for code, display in SUPPORTED_METHODS}
DISPLAY_TO_CODE: Dict[str, str] = {display: code for code, display in SUPPORTED_METHODS}
DISPLAY_TO_CODE_LOWER: Dict[str, str] = {display.lower(): code for code, display in SUPPORTED_METHODS}

METHOD_OUTPUT_BASENAMES: Dict[str, str] = {
    _normalize_method_code("QN"): "normalized_qn",
    _normalize_method_code("BMC"): "normalized_bmc",
    _normalize_method_code("limma"): "normalized_limma",
    _normalize_method_code("ConQuR"): "normalized_conqur",
    _normalize_method_code("PLSDA"): "normalized_plsda",
    _normalize_method_code("ComBat"): "normalized_combat",
    _normalize_method_code("FSQN"): "normalized_fsqn",
    _normalize_method_code("MMUPHin"): "normalized_mmuphin",
    _normalize_method_code("RUV"): "normalized_ruv",
    _normalize_method_code("MetaDICT"): "normalized_metadict",
    _normalize_method_code("PN"): "normalized_pn",
    _normalize_method_code("FAbatch"): "normalized_fabatch",
    _normalize_method_code("ComBatSeq"): "normalized_combatseq",
    _normalize_method_code("DEBIAS"): "normalized_debias",
}


# ---- General helpers ----

def human_size(num_bytes: int) -> str:
    units = ["B", "KB", "MB", "GB", "TB"]
    size = float(num_bytes)
    for u in units:
        if size < 1024 or u == units[-1]:
            return f"{size:.1f} {u}" if u != "B" else f"{int(size)} {u}"
        size /= 1024


def ensure_output_root() -> None:
    OUTPUT_ROOT.mkdir(exist_ok=True)


def cleanup_old_sessions(max_age_hours: int = CLEANUP_HOURS) -> None:
    if not OUTPUT_ROOT.exists():
        return
    cutoff = time.time() - max_age_hours * 3600
    for session_dir in OUTPUT_ROOT.iterdir():
        try:
            if not session_dir.is_dir():
                continue
            if session_dir.stat().st_mtime < cutoff:
                shutil.rmtree(session_dir, ignore_errors=True)
        except OSError:
            continue


def get_session_dir(session_id: str) -> Path:
    ensure_output_root()
    session_path = OUTPUT_ROOT / session_id
    session_path.mkdir(exist_ok=True, parents=True)
    return session_path


def decode_contents(contents: str) -> bytes:
    header, encoded = contents.split(",", 1)
    return base64.b64decode(encoded)


def save_uploaded_file(contents: str, directory: Path, dest_name: str) -> None:
    data = decode_contents(contents)
    (directory / dest_name).write_bytes(data)


def _build_subprocess_env(command: Sequence[str]) -> Dict[str, str]:
    """Return an environment with project-wide defaults for subprocesses."""

    env = os.environ.copy()
    # Ensure R's reticulate uses this same Python interpreter by default
    env.setdefault("RETICULATE_PYTHON", sys.executable)
    if command:
        exe_name = Path(str(command[0])).name.lower()
        if "rscript" in exe_name:
            # Direct R's implicit plotting device to a null target so commands that
            # emit base plots do not leave behind stray Rplots.pdf artifacts.
            null_target = "NUL" if os.name == "nt" else "/dev/null"
            env.setdefault("R_DEFAULT_DEVICE", "pdf")
            env.setdefault("R_PDF_DEFAULT", null_target)
    return env


def run_command(command: Sequence[str], cwd: Path) -> Tuple[bool, str]:
    def _s(val) -> str:
        if val is None:
            return ""
        try:
            return val.strip()
        except Exception:
            try:
                return str(val).strip()
            except Exception:
                return ""

    try:
        env = _build_subprocess_env(command)
        result = subprocess.run(
            command,
            cwd=cwd,
            capture_output=True,
            text=True,
            encoding="utf-8",
            errors="replace",
            check=True,
            env=env,
        )
        log = textwrap.dedent(
            f"""
            $ {' '.join(command)}
            {_s(result.stdout)}
            {_s(result.stderr)}
            """
        ).strip()
        return True, log
    except subprocess.CalledProcessError as exc:
        stdout = getattr(exc, "stdout", getattr(exc, "output", None))
        stderr = getattr(exc, "stderr", None)
        log = textwrap.dedent(
            f"""
            $ {' '.join(command)}
            {_s(stdout)}
            {_s(stderr)}
            """
        ).strip()
        return False, log


def _terminate_process(proc: subprocess.Popen, *, timeout: float = 5.0) -> None:
    """Best-effort helper to terminate a child process."""

    if proc.poll() is not None:
        return
    try:
        proc.terminate()
        proc.wait(timeout=timeout)
    except subprocess.TimeoutExpired:
        try:
            proc.kill()
            proc.wait(timeout=timeout)
        except (subprocess.TimeoutExpired, OSError):
            pass
    except OSError:
        pass


def run_command_streaming(command: Sequence[str], cwd: Path, log_path: Path) -> Tuple[bool, str]:
    """Run a command streaming stdout to a log file for real-time viewing.

    Returns (success, combined_log_text_at_end).
    """
    log_path.parent.mkdir(parents=True, exist_ok=True)
    combined: List[str] = []
    env = _build_subprocess_env(command)
    def _stamp(message: str, icon: str) -> str:
        stamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        icon_part = f"{icon} " if icon else ""
        clean_msg = str(message).rstrip("\n")
        return f"[{stamp}] {icon_part}{clean_msg}\n"

    header = _stamp("$ " + " ".join(command), icon="▶️")
    proc: Optional[subprocess.Popen] = None
    try:
        with log_path.open("a", encoding="utf-8", errors="replace") as logf:
            logf.write(header)
            logf.flush()
            proc = subprocess.Popen(
                command,
                cwd=cwd,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                text=True,
                encoding="utf-8",
                errors="replace",
                env=env,
            )
            assert proc.stdout is not None
            try:
                for line in proc.stdout:
                    formatted = _stamp(line, icon="📝")
                    logf.write(formatted)
                    logf.flush()
                    combined.append(formatted)
            except Exception:
                if proc is not None:
                    _terminate_process(proc)
                raise
            finally:
                if proc.stdout is not None:
                    try:
                        proc.stdout.close()
                    except OSError:
                        pass
            rc = proc.wait()
            success = (rc == 0)
            return success, header + "".join(combined)
    except Exception as exc:
        if proc is not None:
            _terminate_process(proc)
        partial = header + "".join(combined)
        message = f"{partial}\n{exc}" if partial else f"$ {' '.join(command)}\n{exc}"
        return False, message


def run_r_scripts(
    script_names: Sequence[str],
    output_dir: Path,
    log_path: Optional[Path] = None,
    extra_args: Optional[Sequence[str]] = None,
) -> Tuple[bool, str]:
    """Run one or more R scripts with the output directory and optional flags.

    extra_args: a sequence of CLI flags (e.g., ["--k=30", "--npcs=50"]).
    """
    logs: List[str] = []
    args = tuple(extra_args) if extra_args else tuple()
    for script in script_names:
        script_path = PLOTS_DIR / script
        if not script_path.exists():
            logs.append(f"Warning: Script not found: {script}")
            continue
        cmd = build_rscript_command(script_path, output_dir, *args)
        if log_path is not None:
            success, log = run_command_streaming(cmd, cwd=BASE_DIR, log_path=log_path)
        else:
            success, log = run_command(cmd, cwd=BASE_DIR)
        logs.append(log)
        if not success:
            return False, "\n\n".join(logs)
    return True, "\n\n".join(logs)


def _normalize_method_name(method: str) -> Tuple[Optional[str], Optional[Path]]:
    """Resolve a user-supplied identifier to a canonical code and script path."""

    method_arg = str(method)
    if not method_arg:
        return None, None
    canonical_code = method_arg
    script_path = resolve_method_script(canonical_code)
    if script_path is not None:
        return canonical_code, script_path
    lookup = DISPLAY_TO_CODE.get(method_arg) or DISPLAY_TO_CODE_LOWER.get(method_arg.lower())
    if lookup:
        canonical_code = lookup
        script_path = resolve_method_script(canonical_code)
        if script_path is not None:
            return canonical_code, script_path
    return None, None


def _format_method_params(params: Optional[Dict[str, object]]) -> Tuple[str, ...]:
    if not params:
        return tuple()
    args: List[str] = []
    for key, value in params.items():
        if value is None:
            continue
        if isinstance(value, str) and value == "":
            continue
        normalized_value: object
        if isinstance(value, bool):
            normalized_value = str(value).lower()
        else:
            normalized_value = value
        args.append(f"--{key}={normalized_value}")
    return tuple(args)


def run_single_method(
    session_dir: Path,
    method: str,
    log_path: Optional[Path] = None,
    params: Optional[Dict[str, object]] = None,
) -> Tuple[bool, str]:
    """Execute a single correction method via its dedicated R script."""

    canonical_code, script_path = _normalize_method_name(method)
    if canonical_code is None or script_path is None:
        message = f"Unknown method: {method}"
        if MISSING_METHOD_SCRIPTS:
            missing = ", ".join(sorted(MISSING_METHOD_SCRIPTS))
            message += f" (missing scripts for: {missing})"
        return False, message
    if not script_path.exists():
        return False, f"Script not found for method {canonical_code}: {script_path.name}"

    param_args = _format_method_params(params)
    command = build_rscript_command(script_path, session_dir, *param_args)
    if log_path is not None:
        success, log = run_command_streaming(command, cwd=BASE_DIR, log_path=log_path)
    else:
        success, log = run_command(command, cwd=BASE_DIR)
    if success:
        return True, log

    failure_note = f"Method failed: {canonical_code}"
    if log_path is not None:
        try:
            with log_path.open("a", encoding="utf-8", errors="replace") as logf:
                logf.write(failure_note + "\n")
        except OSError:
            pass
    combined = "\n".join(filter(None, [log, failure_note]))
    return False, combined


def _append_log_message(log_path: Path, message: str) -> None:
    try:
        with log_path.open("a", encoding="utf-8", errors="replace") as fh:
            fh.write(message + "\n")
    except OSError:
        pass


def _successful_methods_from_summary(session_dir: Path) -> Set[str]:
    summary = _load_session_summary(session_dir)
    if not summary:
        return set()
    methods = summary.get("methods")
    if not isinstance(methods, list):
        return set()
    completed: Set[str] = set()
    for entry in methods:
        if not isinstance(entry, dict):
            continue
        status = entry.get("status")
        name = entry.get("name")
        if status != "success" or not name:
            continue
        code = _normalize_method_code(str(name))
        if code:
            completed.add(code)
    return completed


def method_output_paths(session_dir: Path, method: str) -> List[Path]:
    code = _normalize_method_code(method)
    base = METHOD_OUTPUT_BASENAMES.get(code)
    if not base:
        return []
    return [
        session_dir / f"{base}_tss.csv",
        session_dir / f"{base}_clr.csv",
    ]


def method_output_exists(session_dir: Path, method: str) -> bool:
    for path in method_output_paths(session_dir, method):
        if path.exists():
            return True
    return False


def _failure_flag_path(session_dir: Path, method: str) -> Path | None:
    code = _normalize_method_code(method)
    if not code:
        return None
    return session_dir / f"{code}.failed"


def mark_method_failed(session_dir: Path, method: str) -> None:
    path = _failure_flag_path(session_dir, method)
    if not path:
        return
    try:
        path.write_text("failed", encoding="utf-8")
    except OSError:
        pass


def clear_method_failure(session_dir: Path, method: str) -> None:
    path = _failure_flag_path(session_dir, method)
    if not path:
        return
    try:
        if path.exists():
            path.unlink()
    except OSError:
        pass


def method_failed_last_run(session_dir: Path, method: str) -> bool:
    path = _failure_flag_path(session_dir, method)
    return bool(path and path.exists())


def _remove_method_from_summary(session_dir: Path, method: str) -> bool:
    summary_path = session_dir / "session_summary.json"
    if not summary_path.exists():
        return False
    try:
        data = json.loads(summary_path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError, TypeError):
        return False
    if not isinstance(data, dict):
        return False
    methods = data.get("methods")
    if not isinstance(methods, list):
        return False
    target = _normalize_method_code(method)
    changed = False
    filtered: List[object] = []
    for entry in methods:
        if not isinstance(entry, dict):
            filtered.append(entry)
            continue
        name = entry.get("name")
        if _normalize_method_code(str(name)) == target:
            changed = True
            continue
        filtered.append(entry)
    if not changed:
        return False
    data["methods"] = filtered
    try:
        summary_path.write_text(json.dumps(data, indent=2), encoding="utf-8")
    except OSError:
        return False
    return True


def delete_method_outputs(session_dir: Path, method: str) -> bool:
    removed = False
    for path in method_output_paths(session_dir, method):
        try:
            if path.exists():
                path.unlink()
                removed = True
        except OSError:
            pass
    if _remove_method_from_summary(session_dir, method):
        removed = True
    return removed


def any_method_outputs(session_dir: Path) -> bool:
    for method_code, _ in SUPPORTED_METHODS:
        if method_output_exists(session_dir, method_code):
            return True
    return False


def run_methods(session_dir: Path, methods: Iterable[str], log_path: Optional[Path] = None) -> Tuple[bool, str]:
    """Run correction methods sequentially, one R subprocess per method."""

    logs: List[str] = []
    overall_success = True
    completed_codes = _successful_methods_from_summary(session_dir)

    for method in methods:
        if not method:
            continue

        canonical_code, _ = _normalize_method_name(method)
        normalized_code = _normalize_method_code(canonical_code) if canonical_code else None
        if normalized_code:
            if normalized_code in completed_codes:
                display = CODE_TO_DISPLAY.get(canonical_code, canonical_code)
                message = f"Skipping {display}: already completed."
                logs.append(message)
                if log_path is not None:
                    _append_log_message(log_path, message)
                continue

        success, log = run_single_method(session_dir, method, log_path=log_path)
        if log:
            logs.append(log)
        if success:
            if normalized_code:
                completed_codes.add(normalized_code)
        else:
            overall_success = False

    return overall_success, "\n\n".join(filter(None, logs))


def run_preprocess(session_dir: Path, log_path: Optional[Path] = None) -> Tuple[bool, str]:
    """Run preprocess.R in the project root for a given session directory.

    It writes outputs into the provided session directory and reads the
    matrix from session_dir/raw.csv by default.
    """
    if not PREPROCESS_SCRIPT.exists():
        return False, f"Script not found: {PREPROCESS_SCRIPT.name}"
    matrix_path = session_dir / "raw.csv"
    command = build_rscript_command(PREPROCESS_SCRIPT, session_dir, matrix_path)
    if log_path is not None:
        return run_command_streaming(command, cwd=BASE_DIR, log_path=log_path)
    return run_command(command, cwd=BASE_DIR)


def render_figures(session_dir: Path, figures: Sequence[FigureSpec]):
    cards = []
    for spec in figures:
        file_path = session_dir / spec.filename
        if not file_path.exists():
            continue
        src = _encode_image_source(file_path, max_png_side=_resolve_png_max_side(file_path))
        cards.append(
            dbc.Col(
                dbc.Card(
                    [
                        dbc.CardImg(src=src, top=True),
                        dbc.CardBody(html.H6(spec.label, className="card-title")),
                    ],
                    className="h-100",
                ),
                xs=12,
                sm=6,
                lg=4,
                className="mb-4",
            )
        )
    if not cards:
        return html.Div("No figures available yet. Run the analysis to generate outputs.")
    return dbc.Row(cards, className="gy-2")


def _read_csv_rows(csv_path: Path) -> Tuple[List[str], List[List[str]]]:
    import csv as _csv
    with csv_path.open("r", encoding="utf-8", newline="") as fh:
        reader = _csv.reader(fh)
        rows = list(reader)
    if not rows:
        return [], []
    header = rows[0]
    data = rows[1:] if len(rows) > 1 else []
    return header, data


def _make_ag_grid(
    grid_id: str,
    column_defs: Sequence[Dict[str, object]],
    row_data: Sequence[Dict[str, object]],
    *,
    class_name: str = "ag-theme-alpine be-ag-grid",
    grid_options: Optional[Dict[str, object]] = None,
    default_col_def: Optional[Dict[str, object]] = None,
) -> dag.AgGrid:
    base_options: Dict[str, object] = {
        "domLayout": "autoHeight",
        "ensureDomOrder": True,
        "suppressHorizontalScroll": False,
        "suppressAggFuncInHeader": True,
    }
    if grid_options:
        base_options.update(grid_options)

    base_default_col_def: Dict[str, object] = {
        "resizable": True,
        "sortable": True,
        "filter": True,
        "wrapHeaderText": True,
        "autoHeaderHeight": True,
        "flex": 1,
        "minWidth": 120,
        "cellStyle": {"textAlign": "center"},
    }
    if default_col_def:
        base_default_col_def.update(default_col_def)

    return dag.AgGrid(
        id=grid_id,
        columnDefs=list(column_defs),
        rowData=list(row_data),
        className=class_name,
        dashGridOptions=base_options,
        defaultColDef=base_default_col_def,
        style={"width": "100%"},
    )


def _find_file_case_insensitive(directory: Path, target_name: str) -> Path | None:
    t = target_name.lower()
    for p in directory.iterdir():
        if p.is_file() and p.name.lower() == t:
            return p
    return None


def _candidate_csvs_for_image(filename: str) -> List[str]:
    stem = Path(filename).stem
    s = stem.lower()
    bases: List[str] = []
    # image families with shared tables
    if s.startswith("pcoa_"):
        bases = ["pcoa", "pcoa_aitchison", "pcoa_braycurtis"]
    elif s.startswith("nmds_"):
        bases = ["nmds", "nmds_aitchison", "nmds_braycurtis"]
    elif s.startswith("dissimilarity_") or s.startswith("dissimilarity-") or s.startswith("dissimilarity"):
        bases = ["dissimilarity", "dissimilarity_aitchison", "dissimilarity_braycurtis"]
    elif s.startswith("permanova"):
        bases = ["permanova", "permanova_aitchison", "permanova_braycurtis"]
    elif s.startswith("anova_") or s == "anova":
        bases = ["anova", "r2"]
    elif s.startswith("prda_") or s.startswith("prda"):
        bases = ["pRDA", "prda"]
    elif s == "pvca":
        bases = ["PVCA", "pvca"]
    elif s == "alignment_score":
        bases = ["alignment_score", "alignment"]
    elif s == "ebm":
        bases = ["ebm"]
    elif s == "silhouette":
        bases = ["silhouette"]
    elif s == "pca" or s.startswith("pca_") or s.startswith("pca-"):
        bases = ["pca"]
    else:
        bases = [stem]

    # Generate candidate filenames (prefer ranking first)
    candidates: List[str] = []
    for b in bases:
        # ranking summary across methods
        candidates.append(f"{b}_ranking.csv")
    for b in bases:
        # raw assessment tables (stage-specific names first)
        candidates.append(f"{b}_raw_assessment_pre.csv")
    for b in bases:
        candidates.append(f"{b}_raw_assessment_post.csv")
    for b in bases:
        candidates.append(f"{b}_raw_assessment.csv")
    return candidates


RAW_ASSESSMENT_SUFFIXES: Tuple[str, ...] = (
    "_raw_assessment_pre.csv",
    "_raw_assessment_post.csv",
    "_raw_assessment.csv",
)


_RAW_ASSESSMENT_STAGE_ORDER = {
    "pre": ("_raw_assessment_pre.csv", "_raw_assessment.csv", "_raw_assessment_post.csv"),
    "post": ("_raw_assessment_post.csv", "_raw_assessment.csv", "_raw_assessment_pre.csv"),
}


def _raw_assessment_stage_suffixes(stage: str) -> Tuple[str, ...]:
    return _RAW_ASSESSMENT_STAGE_ORDER.get(stage, _RAW_ASSESSMENT_STAGE_ORDER["post"])


_RAW_ASSESSMENT_STRIP_SUFFIXES = ("_raw_assessment_pre", "_raw_assessment_post", "_raw_assessment")


def _raw_assessment_metric_title(path: Path) -> str:
    stem = path.stem
    lower = stem.lower()
    base = stem
    for suffix in _RAW_ASSESSMENT_STRIP_SUFFIXES:
        if lower.endswith(suffix):
            base = stem[: -len(suffix)]
            break
    return base.replace("_", " ").strip().title()


def _raw_assessment_group_key(path: Path) -> str:
    name = path.name
    lower = name.lower()
    for suffix in RAW_ASSESSMENT_SUFFIXES:
        if lower.endswith(suffix):
            return lower[: -len(suffix)]
    return Path(name).stem.lower()


def _sort_raw_assessment_names(names: Iterable[str], stage: str) -> List[str]:
    order = _raw_assessment_stage_suffixes(stage)
    order_index = {suffix: idx for idx, suffix in enumerate(order)}
    def sort_key(name: str) -> Tuple[int, str]:
        lower = name.lower()
        for suffix, idx in order_index.items():
            if lower.endswith(suffix):
                return idx, lower
        return len(order), lower
    return sorted(names, key=sort_key)


def _load_info_table_for_key(
    session_dir: Path,
    stage: str,
    key: str,
    representative: Optional[str],
    geometry_filter: Optional[Set[str]] = None,
) -> Optional[dag.AgGrid]:
    """Load an informational table for the given metric, excluding score/rank columns."""

    if not representative:
        return None

    if key.lower() in {"pca", "pcoa"}:
        return None

    candidates = [
        cand
        for cand in _candidate_csvs_for_image(representative)
        if any(cand.lower().endswith(sfx) for sfx in RAW_ASSESSMENT_SUFFIXES)
    ]
    if not candidates:
        return None

    geometry_filter_set = {token.lower() for token in (geometry_filter or set())}

    column_allowlist = {col.lower() for col in _DETAIL_COLUMN_ALLOWLIST.get(key.lower(), set())}

    def _allowed_column(name: str) -> bool:
        if not column_allowlist:
            return True
        return name.lower() in column_allowlist

    for cand in _sort_raw_assessment_names(candidates, stage):
        found = _find_file_case_insensitive(session_dir, cand)
        if not (found and found.exists()):
            continue

        header, data = _read_csv_rows(found)
        if not header:
            continue

        column_info: List[Tuple[Optional[int], str, str]] = []
        display_headers: List[str] = []
        seen_headers: Set[str] = set()
        seen_canonical: Set[str] = set()
        header_lookup = {str(col).lower(): idx for idx, col in enumerate(header)}

        def _append_column(idx: Optional[int], display: str, canonical: str):
            if not _allowed_column(canonical):
                return
            canonical_key = canonical.lower()
            if display in seen_headers or canonical_key in seen_canonical:
                return
            seen_headers.add(display)
            seen_canonical.add(canonical_key)
            column_info.append((idx, display, canonical))
            display_headers.append(display)

        method_idx = header_lookup.get("method")
        if method_idx is not None:
            _append_column(method_idx, "Method", "Method")

        geometry_idx = header_lookup.get("geometry")
        if geometry_filter_set and geometry_idx is None:
            continue

        trend_spec = _DETAIL_METRIC_TRENDS.get(key.lower(), ())
        arrow_map = {"up": "↑", "down": "↓", "flat": "↔"}
        for column_name, trend in trend_spec:
            idx = header_lookup.get(column_name.lower())
            arrow = arrow_map.get(trend.lower(), "↔")
            display = f"{column_name} {arrow}"
            _append_column(idx, display, column_name)

        for idx, column_name in enumerate(header):
            raw_name = str(column_name)
            lower_name = raw_name.lower()
            if lower_name == "geometry":
                # Geometry is now implied by the table grouping, so omit the column
                continue
            if "rank" in lower_name:
                continue
            display = _display_column_name(column_name)
            if not display:
                continue
            canonical = raw_name
            if display in seen_headers:
                continue
            _append_column(idx, display, canonical)

        if not column_info:
            continue

        formatted_rows: List[Dict[str, object]] = []
        for raw_row in data:
            if geometry_filter_set:
                geometry_raw: Optional[str] = None
                if geometry_idx is not None and geometry_idx < len(raw_row):
                    geometry_raw = raw_row[geometry_idx]
                token = _canonical_geometry_token(geometry_raw)
                if token is None or token.lower() not in geometry_filter_set:
                    continue
            row_dict: Dict[str, object] = {}
            for idx, display, canonical in column_info:
                if idx is None or idx >= len(raw_row):
                    cell = "-"
                else:
                    cell = raw_row[idx]
                key_lower = canonical.lower()
                if key_lower == "method":
                    row_dict[display] = method_formal_name(str(cell))
                elif key_lower == "geometry":
                    row_dict[display] = _format_geometry_value(cell)
                else:
                    numeric_value = _safe_float(cell)
                    row_dict[display] = _rounded(numeric_value) if numeric_value is not None else cell
            formatted_rows.append(row_dict)

        if not formatted_rows:
            continue

        numeric_headers = {
            col for col in display_headers if any(isinstance(row.get(col), (int, float)) for row in formatted_rows)
        }

        column_defs: List[Dict[str, object]] = []
        for _idx, display, canonical in column_info:
            col_def: Dict[str, object] = {
                "headerName": display,
                "field": display,
                "minWidth": 160 if canonical.lower() != "method" else 200,
            }
            if canonical.lower() == "method":
                col_def["pinned"] = "left"
            if display in numeric_headers:
                col_def["type"] = "numericColumn"
            column_defs.append(col_def)

        return _make_ag_grid(
            grid_id=f"{stage}-{key}-details",
            column_defs=column_defs,
            row_data=formatted_rows,
            default_col_def={"minWidth": 140},
        )

    return None


def render_assessment_tabs(session_dir: Path, figures: Sequence[FigureSpec], stage: str = "pre", extra_tabs: Sequence = ()):  # extra dcc.Tab items appended
    """Render a vertical tab set of assessment outputs.

    Group metrics by base (e.g., PCoA, NMDS, ANOVA, pRDA, Dissimilarity heatmaps).
    For groups with multiple geometries (Aitchison/Bray-Curtis), show a single
    top-level tab with sub-tabs: Aitchison, Bray-Curtis, and a third
    "Details" sub-tab containing contextual tables without scores or ranks.
    Single-geometry plots also include the third sub-tab accordingly.
    """

    # Consistent styles for top-level and inner tabs (keeps size stable)
    TOP_TAB_STYLE = {
        "borderBottom": "1px solid #d6d6d6",
        "padding": "1px",
        "fontWeight": "bold",
        "width": "12vw",
        "minWidth": "12vw",
        "maxWidth": "12vw",
        "height": "60px",
        "minHeight": "60px",
        "maxHeight": "60px",
        "display": "flex",
        "alignItems": "center",
        "justifyContent": "center",
        "marginRight": "1vw",
    }
    TOP_TAB_SELECTED_STYLE = {
        "borderTop": "1px solid #d6d6d6",
        "borderBottom": "1px solid #d6d6d6",
        "backgroundColor": "#f8f9fa",
        "color": "#0d6efd",
        "padding": "1px",
        "width": "12vw",
        "minWidth": "12vw",
        "maxWidth": "12vw",
        "height": "60px",
        "minHeight": "60px",
        "maxHeight": "60px",
        "display": "flex",
        "alignItems": "center",
        "justifyContent": "center",
        "marginRight": "1vw",
    }
    SUBTAB_STYLE_BASE = {
        "borderBottom": "1px solid #d6d6d6",
        "padding": "1px",
        "fontWeight": "bold",
        "height": "40px",
        "minHeight": "40px",
        "maxHeight": "40px",
        "display": "flex",
        "alignItems": "center",
        "justifyContent": "center",
        # Auto sizing: distribute across available width
        "flex": "1 1 0",
        "minWidth": "0",
        "width": "auto",
        "boxSizing": "border-box",
        "overflow": "hidden",
        "textOverflow": "ellipsis",
        "whiteSpace": "nowrap",
    }
    SUBTAB_SELECTED_STYLE_BASE = {
        "borderTop": "1px solid #d6d6d6",
        "borderBottom": "1px solid #d6d6d6",
        "backgroundColor": "#f8f9fa",
        "color": "#0d6efd",
        "padding": "1px",
        "height": "40px",
        "minHeight": "40px",
        "maxHeight": "40px",
        "display": "flex",
        "alignItems": "center",
        "justifyContent": "center",
        # Auto sizing: distribute across available width
        "flex": "1 1 0",
        "minWidth": "0",
        "width": "auto",
        "boxSizing": "border-box",
        "overflow": "hidden",
        "textOverflow": "ellipsis",
        "whiteSpace": "nowrap",
    }

    def content_for_image(filename: str) -> html.Div:
        img_path = session_dir / filename
        if not img_path.exists() and img_path.suffix.lower() in {".tif", ".tiff"}:
            png_path = img_path.with_suffix(".png")
            if png_path.exists():
                img_path = png_path
        if not img_path.exists():
            return html.Div("Image not found.")
        src = _encode_image_source(img_path, max_png_side=_resolve_png_max_side(img_path))
        img = html.Img(
            src=src,
            style={
                "maxWidth": "100%",
                "height": "auto",
                "display": "block",
                "margin": "0 auto",
            },
        )
        return html.Div([img], style={"width": "100%"})

    tabs = []
    first_value = None

    # Build groups
    groups = {}

    def add_group_item(key: str, title: str, geom: str, filename: str):
        g = groups.setdefault(key, {"title": title, "ait": None, "bray": None, "single": None})
        if geom == "ait":
            g["ait"] = filename
        elif geom == "bray":
            g["bray"] = filename
        else:
            g["single"] = filename

    for spec in figures:
        fn = spec.filename
        low = fn.lower()
        stem = Path(low).stem
        if low.startswith("pcoa_aitchison"):
            add_group_item("pcoa", "PCoA", "ait", fn)
        elif low.startswith("pcoa_braycurtis"):
            add_group_item("pcoa", "PCoA", "bray", fn)
        elif low.startswith("nmds_aitchison"):
            add_group_item("nmds", "NMDS", "ait", fn)
        elif low.startswith("nmds_braycurtis"):
            add_group_item("nmds", "NMDS", "bray", fn)
        elif low.startswith("dissimilarity_heatmaps_aitchison"):
            add_group_item("dissimilarity", "Dissimilarity heatmaps", "ait", fn)
        elif low.startswith("dissimilarity_heatmaps_braycurtis"):
            add_group_item("dissimilarity", "Dissimilarity heatmaps", "bray", fn)
        elif low.startswith("permanova_aitchison"):
            add_group_item("permanova", "PERMANOVA R²", "ait", fn)
        elif low.startswith("permanova_braycurtis"):
            add_group_item("permanova", "PERMANOVA R²", "bray", fn)
        elif low.startswith("permanova"):
            add_group_item("permanova", "PERMANOVA R²", "single", fn)
        elif low.startswith("anova_aitchison"):
            add_group_item("r2", "Feature-wise ANOVA R²", "ait", fn)
        elif low.startswith("anova_braycurtis"):
            add_group_item("r2", "Feature-wise ANOVA R²", "bray", fn)
        elif low.startswith("prda_aitchison"):
            add_group_item("prda", "pRDA", "ait", fn)
        elif low.startswith("prda_braycurtis"):
            add_group_item("prda", "pRDA", "bray", fn)
        elif stem == "pca":
            add_group_item("pca", "PCA", "ait", fn)
        else:
            # fallback: one tab per figure (with its own assessment sub-tab if any)
            key = f"single:{low}"
            add_group_item(key, spec.label, "single", fn)

    # Build tabs with sub-tabs
    for idx, (key, g) in enumerate(groups.items()):
        title = g["title"]
        sub_tabs = []
        # choose a representative filename for assessment lookup
        rep = g["ait"] or g["bray"] or g["single"]

        # Build sub-tab definitions first so we can size them evenly later
        sub_defs = []
        has_ait = bool(g["ait"])
        has_bray = bool(g["bray"])
        has_single = bool(g["single"])
        if has_ait:
            label = "Plot" if not has_bray and not has_single else "Aitchison"
            sub_defs.append((label, f"{key}-ait", content_for_image(g["ait"])) )
        if has_bray:
            label = "Plot" if not has_ait and not has_single else "Bray-Curtis"
            sub_defs.append((label, f"{key}-bray", content_for_image(g["bray"])) )
        if has_single and not (has_ait or has_bray):
            sub_defs.append(("Plot", f"{key}-plot", content_for_image(g["single"])) )

        # Third sub-tab: informational summary table (no scoring)
        third_label = "Details"
        third_content = _load_info_table_for_key(session_dir, stage, key, rep)
        if third_content is not None:
            sub_defs.append((third_label, f"{key}-third", html.Div(third_content, style={"width": "100%"})))

        # Create sub-tabs with fixed width; allow horizontal scrolling in container
        SUBTAB_STYLE = dict(SUBTAB_STYLE_BASE)
        SUBTAB_SELECTED_STYLE = dict(SUBTAB_SELECTED_STYLE_BASE)
        sub_tabs = [
            dcc.Tab(label=lbl, value=val, children=child, style=SUBTAB_STYLE, selected_style=SUBTAB_SELECTED_STYLE)
            for (lbl, val, child) in sub_defs
        ]

        inner_default = sub_tabs[0].value if sub_tabs else None
        inner = dcc.Tabs(
            className="be-subtabs",
            children=sub_tabs,
            value=inner_default,
            vertical=False,
            mobile_breakpoint=0,
            # Keep in one row and allow horizontal scroll
            style={
                "width": "100%",
            },
        )

        top_value = f"tab-{key}"
        tabs.append(dcc.Tab(label=title, value=top_value, children=html.Div(inner), style=TOP_TAB_STYLE, selected_style=TOP_TAB_SELECTED_STYLE))
        if first_value is None:
            first_value = top_value

    if not tabs and not extra_tabs:
        return html.Div("No figures available yet. Run the analysis to generate outputs.")

    # Append any extra tabs (e.g., additional summaries)
    for extra in extra_tabs:
        if first_value is None:
            first_value = getattr(extra, 'value', None) or 'extra-0'
        tabs.append(extra)

    return dcc.Tabs(
        children=tabs,
        value=first_value or (extra_tabs[0].value if extra_tabs else None),
        vertical=True,
        className="be-results-tabs",
    )


def build_group_subtab_definitions(session_dir: Path, stage: str, key: str):
    """Return subtab definitions as (label, value, content Div).

    Used by both the renderer and the subtab content switch callback.
    """
    figures = PRE_FIGURES if stage == "pre" else POST_FIGURES

    def content_for_image(filename: str) -> html.Div:
        img_path = session_dir / filename
        if not img_path.exists() and img_path.suffix.lower() in {".tif", ".tiff"}:
            png_path = img_path.with_suffix(".png")
            if png_path.exists():
                img_path = png_path
        if not img_path.exists():
            return html.Div("Image not found.")
        src = _encode_image_source(img_path, max_png_side=_resolve_png_max_side(img_path))
        img = html.Img(
            src=src,
            style={
                "maxWidth": "100%",
                "height": "auto",
                "display": "block",
                "margin": "0 auto",
            },
        )
        return html.Div([img], style={"width": "100%"})

    # Resolve filenames in this group
    def _init_group_config(group_key: str) -> Dict[str, Optional[str]]:
        if group_key == "pcoa":
            return {
                "title": "PCoA",
                "ait_batch": None,
                "ait_target": None,
                "bray_batch": None,
                "bray_target": None,
            }
        if group_key == "nmds":
            return {
                "title": "NMDS",
                "ait_batch": None,
                "ait_target": None,
                "bray_batch": None,
                "bray_target": None,
            }
        if group_key == "pca":
            return {
                "title": group_key.upper(),
                "batch": None,
                "target": None,
            }
        return {"ait": None, "bray": None, "single": None, "title": group_key}

    g: Dict[str, Optional[str]] = _init_group_config(key)
    for spec in figures:
        low = spec.filename.lower()
        stem = Path(low).stem
        if key == "pcoa":
            if low.startswith("pcoa_aitchison_batch"):
                g["ait_batch"] = spec.filename
            elif low.startswith("pcoa_aitchison_target"):
                g["ait_target"] = spec.filename
            elif low.startswith("pcoa_braycurtis_batch"):
                g["bray_batch"] = spec.filename
            elif low.startswith("pcoa_braycurtis_target"):
                g["bray_target"] = spec.filename
        elif key == "nmds":
            if low.startswith("nmds_aitchison_batch"):
                g["ait_batch"] = spec.filename
            elif low.startswith("nmds_aitchison_target"):
                g["ait_target"] = spec.filename
            elif low.startswith("nmds_braycurtis_batch"):
                g["bray_batch"] = spec.filename
            elif low.startswith("nmds_braycurtis_target"):
                g["bray_target"] = spec.filename
        elif key == "dissimilarity":
            if low.startswith("dissimilarity_heatmaps_aitchison"):
                g["ait"] = spec.filename
            elif low.startswith("dissimilarity_heatmaps_braycurtis"):
                g["bray"] = spec.filename
            g["title"] = "Dissimilarity heatmaps"
        elif key == "permanova":
            if low.startswith("permanova_aitchison"):
                g["ait"] = spec.filename
            elif low.startswith("permanova_braycurtis"):
                g["bray"] = spec.filename
            elif low.startswith("permanova"):
                g["single"] = spec.filename
            g["title"] = "PERMANOVA R²"
        elif key == "r2":
            if low.startswith("anova_aitchison"):
                g["ait"] = spec.filename
            elif low.startswith("anova_braycurtis"):
                g["bray"] = spec.filename
            g["title"] = "Feature-wise ANOVA R²"
        elif key == "prda":
            if low.startswith("prda_aitchison"):
                g["ait"] = spec.filename
            elif low.startswith("prda_braycurtis"):
                g["bray"] = spec.filename
            g["title"] = "pRDA"
        elif key == "alignment" and stem in {"alignment_score", "alignment"}:
            g["single"] = spec.filename
            g["title"] = "Alignment score"
        elif key == "pca":
            if stem == "pca_batch":
                g["batch"] = spec.filename
            elif stem == "pca_target":
                g["target"] = spec.filename
        elif key == "pvca" and stem == "pvca":
            g["single"] = spec.filename; g["title"] = "PVCA"
        elif key == "ebm" and stem == "ebm":
            g["single"] = spec.filename; g["title"] = "Entropy score"
        elif key == "silhouette" and stem == "silhouette":
            g["single"] = spec.filename; g["title"] = "Silhouette score"

    sub_defs: List[Tuple[str, str, html.Div]] = []
    rep_candidates: List[Optional[str]] = []

    if key == "pcoa":
        mapping = [
            ("Aitchison (Batch)", f"{key}-ait-batch", g.get("ait_batch")),
            ("Aitchison (Target)", f"{key}-ait-target", g.get("ait_target")),
            ("Bray-Curtis (Batch)", f"{key}-bray-batch", g.get("bray_batch")),
            ("Bray-Curtis (Target)", f"{key}-bray-target", g.get("bray_target")),
        ]
        rep_candidates.extend([g.get("ait_batch"), g.get("ait_target"), g.get("bray_batch"), g.get("bray_target")])
        for label, value, filename in mapping:
            if filename:
                sub_defs.append((label, value, content_for_image(filename)))
    elif key == "nmds":
        mapping = [
            ("Aitchison (Batch)", f"{key}-ait-batch", g.get("ait_batch")),
            ("Aitchison (Target)", f"{key}-ait-target", g.get("ait_target")),
            ("Bray-Curtis (Batch)", f"{key}-bray-batch", g.get("bray_batch")),
            ("Bray-Curtis (Target)", f"{key}-bray-target", g.get("bray_target")),
        ]
        rep_candidates.extend([g.get("ait_batch"), g.get("ait_target"), g.get("bray_batch"), g.get("bray_target")])
        for label, value, filename in mapping:
            if filename:
                sub_defs.append((label, value, content_for_image(filename)))
    elif key == "pca":
        mapping = [
            ("Batch grouping", f"{key}-batch", g.get("batch")),
            ("Target grouping", f"{key}-target", g.get("target")),
        ]
        rep_candidates.extend([g.get("batch"), g.get("target")])
        for label, value, filename in mapping:
            if filename:
                sub_defs.append((label, value, content_for_image(filename)))
    else:
        has_ait = bool(g.get("ait"))
        has_bray = bool(g.get("bray"))
        has_single = bool(g.get("single"))
        if has_ait:
            label = "Plot" if not has_bray and not has_single else "Aitchison"
            sub_defs.append((label, f"{key}-ait", content_for_image(g["ait"])) )
            rep_candidates.append(g.get("ait"))
        if has_bray:
            label = "Plot" if not has_ait and not has_single else "Bray-Curtis"
            sub_defs.append((label, f"{key}-bray", content_for_image(g["bray"])) )
            rep_candidates.append(g.get("bray"))
        if has_single and not (has_ait or has_bray):
            sub_defs.append(("Plot", f"{key}-plot", content_for_image(g["single"])) )
            rep_candidates.append(g.get("single"))

    if not rep_candidates:
        rep_candidates.append(g.get("single"))

    rep = next((cand for cand in rep_candidates if cand), None)
    third_label = "Details"
    third_content: Optional[html.Div] = None
    key_lower = key.lower()

    if key_lower in _MULTI_GEOMETRY_DETAIL_KEYS:
        geometry_sections: List[html.Div] = []
        for geom_label, geom_filter in (
            ("Aitchison", _AITCHISON_GEOMETRY_TOKENS),
            ("Bray-Curtis", _BRAY_GEOMETRY_TOKENS),
        ):
            table = _load_info_table_for_key(
                session_dir,
                stage,
                key,
                rep,
                geometry_filter=geom_filter,
            )
            if table is None:
                continue
            geometry_sections.append(
                html.Div(
                    [
                        html.H5(geom_label, className="be-detail-geometry-heading"),
                        table,
                    ],
                    style={"marginBottom": "24px"},
                )
            )
        if geometry_sections:
            third_content = html.Div(geometry_sections, style={"width": "100%"})
    else:
        third_content = _load_info_table_for_key(session_dir, stage, key, rep)

    if third_content is None and key_lower == "r2":
        third_content = _load_info_table_for_key(session_dir, stage, key, "anova.tif")

    if third_content is not None:
        sub_defs.append((third_label, f"{key}-third", html.Div(third_content, style={"width": "100%"})))

    return sub_defs


def build_group_subtab_content(session_dir: Path, stage: str, key: str, selected_value: str):
    """Return the content Div for the given subtab value."""
    sub_defs = build_group_subtab_definitions(session_dir, stage, key)
    for (_lbl, val, child) in sub_defs:
        if val == selected_value:
            return child
    # default: first item content or message
    return (sub_defs[0][2] if sub_defs else html.Div("No content."))


def render_group_tabset(session_dir: Path, stage: str, key: str):
    """Render one group's subtabs as two components: tab bar + content panel.

    - Subtab options arranged horizontally and scroll when overflow.
    - Content panel separated below with margin and centered content.
    """
    # Styles for tab headers
    SUBTAB_STYLE_BASE = {
        "borderBottom": "1px solid #d6d6d6",
        "padding": "6px",
        "fontWeight": "bold",
        "height": "40px",
        "minHeight": "40px",
        "maxHeight": "40px",
        "display": "flex",
        "alignItems": "center",
        "justifyContent": "center",
        # Auto sizing: distribute across available width
        "flex": "1 1 0",
        "minWidth": "0",
        "width": "auto",
        "boxSizing": "border-box",
        "overflow": "hidden",
        "textOverflow": "ellipsis",
        "whiteSpace": "nowrap",
    }
    SUBTAB_SELECTED_STYLE_BASE = {
        "borderTop": "1px solid #d6d6d6",
        "borderBottom": "1px solid #d6d6d6",
        "backgroundColor": "#f8f9fa",
        "color": "#0d6efd",
        "padding": "6px",
        "height": "40px",
        "minHeight": "40px",
        "maxHeight": "40px",
        "display": "flex",
        "alignItems": "center",
        "justifyContent": "center",
        # Auto sizing: distribute across available width
        "flex": "1 1 0",
        "minWidth": "0",
        "width": "auto",
        "boxSizing": "border-box",
        "overflow": "hidden",
        "textOverflow": "ellipsis",
        "whiteSpace": "nowrap",
    }

    sub_defs = build_group_subtab_definitions(session_dir, stage, key)
    subtab_items = [
        dcc.Tab(label=lbl, value=val, style=dict(SUBTAB_STYLE_BASE), selected_style=dict(SUBTAB_SELECTED_STYLE_BASE))
        for (lbl, val, _child) in sub_defs
    ]
    default_val = sub_defs[0][1] if sub_defs else None

    sid = f"{stage}-{key}"
    tabs_bar = dcc.Tabs(
        id=f"{sid}-subtabs",
        children=subtab_items,
        value=default_val,
        className="be-subtabs",
        style={
            "width": "100%",
        },
        vertical=False,
        mobile_breakpoint=0,
    )

    initial_content = build_group_subtab_content(session_dir, stage, key, default_val) if default_val else html.Div("No content.")
    content_panel = html.Div(
        id=f"{sid}-subtab-content",
        children=initial_content,
        style={
            "width": "100%",
            "minWidth": "100%",
            "marginTop": "12px",
        },
        className="be-subtab-content",
    )

    card = dbc.Card(
        [
            dbc.CardHeader(
                tabs_bar,
                className="be-subtab-card-header",
            ),
            dbc.CardBody(
                content_panel,
                className="be-subtab-card-body",
            ),
        ],
        style={
            "width": "83vw",
            "minWidth": "83vw",
            "maxWidth": "83vw",
            "marginTop": "8px",
            "marginLeft": "0",
        },
        className="be-subtab-card",
    )

    return card


@dataclass(frozen=True)
class ScoreSpec:
    field: Optional[str] = None  # column name containing the absolute score (case-insensitive)
    baseline_label: str = "Before correction"


@dataclass
class MethodRankingEntry:
    method: str
    method_code: Optional[str]
    rank: Optional[float]
    absolute: Optional[float]
    raw: Dict[str, str]
    is_baseline: bool


@dataclass
class RankingData:
    metric_key: str
    display_name: str
    columns: List[str]
    entries: List[MethodRankingEntry]


def _safe_float(value: Optional[str]) -> Optional[float]:
    if value is None:
        return None
    try:
        stripped = str(value).strip()
        if stripped == "" or stripped.lower() == "na":
            return None
        return float(stripped)
    except Exception:
        return None


def _display_column_name(name: str) -> str:
    if name is None:
        return ""
    cleaned = str(name).replace("_", " ")
    cleaned = re.sub(r"\s+", " ", cleaned).strip(" -")

    ait_pattern = re.compile(r"\b(aitchison|ait|clr)\b", re.IGNORECASE)
    ait_paren_pattern = re.compile(r"\(\s*(aitchison|ait|clr)\s*(?:geometry)?\s*\)", re.IGNORECASE)
    bc_pattern = re.compile(r"\b(bray[-\s]*curtis|bc|tss)\b", re.IGNORECASE)
    bc_paren_pattern = re.compile(r"\(\s*(bray[-\s]*curtis|bc|tss)\s*(?:geometry)?\s*\)", re.IGNORECASE)

    geometry: Optional[str] = None
    if ait_pattern.search(cleaned) or ait_paren_pattern.search(cleaned):
        geometry = "Ait"
    elif bc_pattern.search(cleaned) or bc_paren_pattern.search(cleaned):
        geometry = "BC"

    for pattern in (ait_paren_pattern, bc_paren_pattern):
        cleaned = pattern.sub("", cleaned)

    cleaned = ait_pattern.sub("", cleaned)
    cleaned = bc_pattern.sub("", cleaned)

    cleaned = re.sub(r"\s+", " ", cleaned)
    cleaned = re.sub(r"\s*-\s*", " ", cleaned)
    cleaned = cleaned.strip(" -")

    cleaned = re.sub(r"\bR\^?2\b", "R\u00B2", cleaned)

    if geometry:
        if cleaned:
            cleaned = f"{cleaned} ({geometry})"
        else:
            cleaned = f"({geometry})"

    return cleaned


_AITCHISON_GEOMETRY_ALIASES: Set[str] = {"ait", "aitch", "aitchison", "clr"}
_BRAY_GEOMETRY_ALIASES: Set[str] = {"bc", "bray", "braycurtis", "tss"}


def _canonical_geometry_token(value: object) -> Optional[str]:
    if not isinstance(value, str):
        return None
    cleaned = value.strip().lower()
    if not cleaned:
        return None
    normalized = re.sub(r"[^a-z]", "", cleaned)
    if normalized in _AITCHISON_GEOMETRY_ALIASES:
        return "aitchison"
    if normalized in _BRAY_GEOMETRY_ALIASES:
        return "braycurtis"
    return normalized or None


def _format_geometry_value(value: object) -> object:
    if not isinstance(value, str):
        return value
    token = _canonical_geometry_token(value)
    if token == "aitchison":
        return "Aitchison"
    if token == "braycurtis":
        return "Bray-Curtis"
    return value.strip()


def _rounded(value: Optional[float], digits: int = 3) -> Optional[float]:
    if value is None:
        return None
    try:
        return round(float(value), digits)
    except Exception:
        return value


def _score_from_fields(row: Dict[str, str], *candidates: str) -> Optional[float]:
    row_lower = {k.lower(): v for k, v in row.items()}
    for name in candidates:
        score = _safe_float(row_lower.get(name.lower()))
        if score is not None:
            return score
    return None


RANKING_SCORE_SPECS: Dict[str, ScoreSpec] = {
    "alignment": ScoreSpec(field="absolute score"),
    "dissimilarity": ScoreSpec(field="absolute score"),
    "ebm": ScoreSpec(field="absolute score"),
    "nmds": ScoreSpec(),
    "pca": ScoreSpec(),
    "pcoa": ScoreSpec(),
    "prda": ScoreSpec(field="absolute score"),
    "pvca": ScoreSpec(field="absolute score"),
    "r2": ScoreSpec(field="absolute score"),
    "silhouette": ScoreSpec(field="absolute score"),
}


RANKING_FILE_ALIASES: Dict[str, List[str]] = {
    "alignment": ["alignment", "alignment_score"],
    "dissimilarity": ["dissimilarity"],
    "ebm": ["ebm", "entropy_score"],
    "nmds": ["nmds"],
    "pca": ["pca"],
    "pcoa": ["pcoa"],
    "prda": ["prda"],
    "pvca": ["pvca"],
    "r2": ["r2", "anova"],
    "silhouette": ["silhouette"],
}


REQUIRED_RANKING_KEYS: Set[str] = {
    "pca",
    "pcoa",
    "nmds",
    "dissimilarity",
    "r2",
    "prda",
    "pvca",
    "ebm",
    "silhouette",
}


def _compute_absolute_score(metric_key: str, row: Dict[str, str]) -> Optional[float]:
    spec = RANKING_SCORE_SPECS.get(metric_key)
    row_lower = {k.lower(): v for k, v in row.items()}
    if spec:
        if spec.field:
            return _safe_float(row_lower.get(spec.field.lower()))
    return _score_from_fields(row, "Score", "Combined_Score", "Value")


def _parse_ranking_file(csv_path: Path) -> RankingData:
    metric_key = csv_path.stem[:-len("_ranking")] if csv_path.name.lower().endswith("_ranking.csv") else csv_path.stem
    metric_key = metric_key.lower()
    display_name = csv_path.stem.replace("_ranking", "").replace("_", " ").title()
    entries: List[MethodRankingEntry] = []
    columns: List[str] = []

    with csv_path.open("r", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        if not reader.fieldnames:
            return RankingData(metric_key, display_name, [], [])
        original_columns = list(reader.fieldnames)
        field_map = {name.lower(): name for name in original_columns}
        method_col = field_map.get("method")
        rank_col = field_map.get("rank")
        absolute_col = field_map.get("absolute score")
        spec = RANKING_SCORE_SPECS.get(metric_key)
        baseline_label = (spec.baseline_label if spec else "Before correction").lower()

        column_display_map: Dict[str, str] = {}
        seen_display: Set[str] = set()
        columns = []
        for name in original_columns:
            display = _display_column_name(name)
            if display.lower() == "rank":
                display = "Average rank"
            if display in seen_display:
                # skip duplicate display headers
                continue
            seen_display.add(display)
            column_display_map[name] = display
            columns.append(display)

        for raw_row in reader:
            if method_col is None:
                continue
            method_raw = (raw_row.get(method_col) or "").strip()
            if not method_raw:
                continue
            method_display = method_formal_name(method_raw)
            method_code = _method_code_from_display(method_display)
            rank_value = _safe_float(raw_row.get(rank_col)) if rank_col else None
            absolute = _safe_float(raw_row.get(absolute_col)) if absolute_col else None
            if absolute is None:
                absolute = _compute_absolute_score(metric_key, raw_row)
            is_baseline = method_display.lower() == baseline_label
            entries.append(MethodRankingEntry(
                method=method_display,
                method_code=method_code,
                rank=rank_value,
                absolute=absolute,
                raw={column_display_map[k]: raw_row.get(k, "") for k in original_columns if column_display_map.get(k)},
                is_baseline=is_baseline,
            ))

    # If the CSV lacks a Rank column, derive ranks from absolute score (higher is better)
    scored = [e for e in entries if e.absolute is not None]
    scored.sort(key=lambda item: item.absolute, reverse=True)
    for idx, entry in enumerate(scored, start=1):
        entry.rank = float(idx)

    return RankingData(metric_key, display_name, columns, entries)


def _build_ranking_table_component(data: RankingData) -> Optional[dag.AgGrid]:
    if not data.entries:
        return None
    has_rank = any(entry.rank is not None for entry in data.entries)
    has_absolute = any(entry.absolute is not None for entry in data.entries)

    base_columns: List[str] = ["Method"]
    if has_rank:
        base_columns.append("Average rank")
    if has_absolute:
        base_columns.append("Absolute score")

    excluded = {"method", "average rank", "absolute score", "relative score"}
    excluded.update(col.lower() for col in base_columns)
    extra_cols = [
        col for col in data.columns
        if col and col.lower() not in excluded
    ]
    header = base_columns + extra_cols

    row_data: List[Dict[str, object]] = []
    numeric_candidates: Dict[str, bool] = {col: False for col in header}
    for entry in data.entries:
        rank_val = _rounded(entry.rank)
        absolute_val = _rounded(entry.absolute)
        row: Dict[str, object] = {"Method": entry.method}
        if "Average rank" in base_columns:
            row["Average rank"] = rank_val
            if rank_val is not None:
                numeric_candidates["Average rank"] = True
        if "Absolute score" in base_columns:
            row["Absolute score"] = absolute_val
            if absolute_val is not None:
                numeric_candidates["Absolute score"] = True
        for col in extra_cols:
            raw_value = entry.raw.get(col, "")
            numeric_value = _safe_float(raw_value)
            if numeric_value is not None:
                row[col] = _rounded(numeric_value)
                numeric_candidates[col] = True
            else:
                row[col] = raw_value
        row_data.append(row)

    column_defs: List[Dict[str, object]] = []
    for col in header:
        col_def: Dict[str, object] = {
            "headerName": col,
            "field": col,
        }
        if col == "Method":
            col_def["minWidth"] = 220
            col_def["flex"] = 0
        elif col == "Average rank":
            col_def["width"] = 110
        else:
            col_def["minWidth"] = 160
        if numeric_candidates.get(col):
            col_def["type"] = "numericColumn"
            if col == "Average rank":
                col_def["type"] = "numericColumn"
            else:
                col_def["type"] = "numericColumn"
        column_defs.append(col_def)

    return _make_ag_grid(
        grid_id=f"ranking-grid-{data.metric_key}",
        column_defs=column_defs,
        row_data=row_data,
        default_col_def={"filter": True, "minWidth": 150},
        grid_options={"suppressPaginationPanel": True},
    )


def _load_ranking_table_for_key(session_dir: Path, key: str) -> Optional[dag.AgGrid]:
    aliases = RANKING_FILE_ALIASES.get(key, [key])
    for alias in aliases:
        filename = f"{alias}_ranking.csv"
        found = _find_file_case_insensitive(session_dir, filename)
        if found and found.exists():
            table = _build_ranking_table_component(_parse_ranking_file(found))
            if table is not None:
                return table
    alias_lower = {alias.lower() for alias in aliases}
    for candidate in session_dir.glob("*_ranking.csv"):
        stem = candidate.stem
        metric_key = stem[:-len("_ranking")] if stem.lower().endswith("_ranking") else stem
        if metric_key.lower() in alias_lower:
            table = _build_ranking_table_component(_parse_ranking_file(candidate))
            if table is not None:
                return table
    return None


def aggregate_rankings(session_dir: Path) -> Tuple[List[str], List[Dict[str, str]], Dict[str, List[MethodRankingEntry]]]:
    ranking_files = sorted(session_dir.glob("*_ranking.csv"))
    if not ranking_files:
        return [], [], {}

    ranking_data: List[RankingData] = []
    for csv_path in ranking_files:
        data = _parse_ranking_file(csv_path)
        if data.entries:
            ranking_data.append(data)
    if not ranking_data:
        return [], [], {}

    metric_names = [data.display_name for data in ranking_data]
    metric_entries: Dict[str, List[MethodRankingEntry]] = {
        data.display_name: data.entries for data in ranking_data
    }

    method_entries: Dict[str, Dict[str, MethodRankingEntry]] = {}
    method_ranks: defaultdict[str, List[float]] = defaultdict(list)
    for data in ranking_data:
        for entry in data.entries:
            method_entries.setdefault(entry.method, {})[data.display_name] = entry
            if entry.rank is not None:
                method_ranks[entry.method].append(entry.rank)

    methods = sorted(method_entries)
    rows: List[Dict[str, str]] = []
    for method in methods:
        ranks = method_ranks.get(method, [])
        avg_rank = (sum(ranks) / len(ranks)) if ranks else None
        row = {
            "Method": method,
            "Average rank": f"{_rounded(avg_rank):.3f}" if avg_rank is not None else "-",
            "Metrics": str(len(method_entries.get(method, {}))),
        }
        for metric in metric_names:
            entry = method_entries.get(method, {}).get(metric)
            if not entry:
                row[metric] = "-"
                continue
            cell_parts: List[str] = []
            if entry.rank is not None:
                rounded_rank = _rounded(entry.rank)
                if rounded_rank is not None and abs(rounded_rank - round(rounded_rank)) < 1e-6:
                    cell_parts.append(f"#{int(round(rounded_rank))}")
                else:
                    cell_parts.append(f"#{rounded_rank:.3f}" if rounded_rank is not None else "-")
            if entry.absolute is not None:
                cell_parts.append(f"{_rounded(entry.absolute):.3f}")
            row[metric] = " | ".join(cell_parts) if cell_parts else "-"
        rows.append(row)

    def _row_sort_key(item: Dict[str, str]) -> Tuple[float, str]:
        try:
            return float(item["Average rank"]), item["Method"]
        except Exception:
            return float("inf"), item["Method"]

    rows.sort(key=_row_sort_key)
    return metric_names, rows, metric_entries


def build_ranking_tab(session_dir: Path):
    metrics, rows, _ = aggregate_rankings(session_dir)
    if not metrics:
        content = html.Div("No ranking files found. Run the post-correction assessment first.")
    else:
        column_defs: List[Dict[str, object]] = [
            {
                "headerName": "Method",
                "field": "Method",
                "minWidth": 220,
                "flex": 0,
                "pinned": "left",
            },
            {
                "headerName": "Average rank",
                "field": "Average rank",
                "type": "numericColumn",
                "width": 150,
            },
            {
                "headerName": "Metrics",
                "field": "Metrics",
                "type": "numericColumn",
                "width": 110,
            },
        ]
        for metric in metrics:
            column_defs.append(
                {
                    "headerName": metric,
                    "field": metric,
                    "minWidth": 200,
                    "wrapHeaderText": True,
                    "autoHeaderHeight": True,
                }
            )

        row_data: List[Dict[str, object]] = []
        for row in rows:
            record: Dict[str, object] = {
                "Method": row.get("Method"),
                "Average rank": _rounded(_safe_float(row.get("Average rank"))),
            }
            metrics_count = row.get("Metrics")
            try:
                record["Metrics"] = int(metrics_count) if metrics_count is not None else None
            except Exception:
                record["Metrics"] = _safe_float(metrics_count)
            for metric in metrics:
                record[metric] = row.get(metric, "")
            row_data.append(record)

        content = _make_ag_grid(
            grid_id="method-ranking-summary-grid",
            column_defs=column_defs,
            row_data=row_data,
            default_col_def={"minWidth": 160},
            grid_options={"suppressPaginationPanel": True},
        )
    return dcc.Tab(label="Method Ranking", value="tab-ranking", children=html.Div(content, style={"width": "100%"}))


def build_assessment_overview_table(session_dir: Path, stage: str):
    # Build a single integrated table: Test | Criteria | Test Result | Comment
    csv_files = list(session_dir.glob("*.csv"))
    grouped: Dict[str, Dict[str, Path]] = {}
    for path in csv_files:
        lower = path.name.lower()
        for suffix in RAW_ASSESSMENT_SUFFIXES:
            if lower.endswith(suffix):
                base_key = lower[: -len(suffix)]
                grouped.setdefault(base_key, {})[suffix] = path
                break

    chosen_paths: List[Path] = []
    priority = _raw_assessment_stage_suffixes(stage)
    for base_key, suffix_map in grouped.items():
        selected = None
        for suffix in priority:
            candidate = suffix_map.get(suffix)
            if candidate is not None:
                selected = candidate
                break
        if selected is not None:
            chosen_paths.append(selected)

    rows: List[Dict[str, str]] = []

    def pick_value(header: List[str], data_row: List[str]) -> str:
        hl = [h.lower() for h in header]
        pri = ["score", "silhouette", "entropy", "r2", "mean_between"]
        for key in pri:
            if key in hl:
                idx = hl.index(key)
                val = data_row[idx]
                try:
                    return f"{float(val):.3f}"
                except Exception:
                    return val
        # fallback to first numeric-looking cell
        for i, v in enumerate(data_row):
            try:
                return f"{float(v):.3f}"
            except Exception:
                continue
        return "-"

    for path in sorted(chosen_paths, key=_raw_assessment_metric_title):
        test_name = _raw_assessment_metric_title(path)
        header, data = _read_csv_rows(path)
        if not header or not data:
            continue
        # Prefer the row with Method == 'Before correction'
        method_idx = None
        try:
            method_idx = [h.lower() for h in header].index("method")
        except ValueError:
            method_idx = None
        row = None
        if method_idx is not None:
            for r in data:
                if (r[method_idx] or "").strip().lower() == "before correction":
                    row = r
                    break
        if row is None:
            row = data[0]
        # Criteria
        crit = "-"
        if "criteria" in [h.lower() for h in header]:
            crit = row[[h.lower() for h in header].index("criteria")]
        rows.append({
            "Test": test_name,
            "Criteria": crit,
            "Test Result": pick_value(header, row),
        })

    column_defs = [
        {
            "headerName": "Test",
            "field": "Test",
            "minWidth": 240,
        },
        {
            "headerName": "Criteria",
            "field": "Criteria",
            "minWidth": 220,
        },
        {
            "headerName": "Test Result",
            "field": "Test Result",
            "minWidth": 160,
        },
    ]
    return _make_ag_grid(
        grid_id=f"{stage}-assessment-overview-grid",
        column_defs=column_defs,
        row_data=rows,
        default_col_def={"minWidth": 180},
    )


def build_overall_div(session_dir: Path, stage: str):
    if stage == "post":
        metrics, _rows, metric_entries = aggregate_rankings(session_dir)
        if not metrics:
            body = html.Div("No ranking files found. Run post-assessment tabs to generate *_ranking.csv files.")
        else:
            method_metric_map: Dict[str, Dict[str, MethodRankingEntry]] = {}
            for metric_name, entries in metric_entries.items():
                for entry in entries:
                    method_metric_map.setdefault(entry.method, {})[metric_name] = entry

            typed_rows: List[Dict[str, object]] = []
            for method, per_metric in method_metric_map.items():
                ranks = [entry.rank for entry in per_metric.values() if entry.rank is not None]
                avg_rank = (sum(ranks) / len(ranks)) if ranks else None
                rounded_avg = _rounded(avg_rank)
                row: Dict[str, object] = {
                    "Method": method,
                    "Average rank": rounded_avg,
                }
                for metric_name in metrics:
                    entry = per_metric.get(metric_name)
                    row[metric_name] = _rounded(entry.absolute) if entry and entry.absolute is not None else None
                typed_rows.append(row)

            def _sort_key(item: Dict[str, object]) -> Tuple[float, str]:
                avg = item.get("Average rank")
                if isinstance(avg, (int, float)) and avg == avg:
                    return float(avg), str(item.get("Method", ""))
                return float("inf"), str(item.get("Method", ""))

            typed_rows.sort(key=_sort_key)
            for idx, row in enumerate(typed_rows, start=1):
                row["Rank"] = idx

            column_defs: List[Dict[str, object]] = [
                {
                    "headerName": "Rank",
                    "field": "Rank",
                    "type": "numericColumn",
                    "width": 90,
                    "sort": "asc",
                },
                {
                    "headerName": "Method",
                    "field": "Method",
                    "minWidth": 220,
                    "flex": 0,
                    "pinned": "left",
                },
                {
                    "headerName": "Average rank",
                    "field": "Average rank",
                    "type": "numericColumn",
                    "width": 160,
                },
            ]

            for metric_name in metrics:
                column_defs.append(
                    {
                        "headerName": metric_name,
                        "field": metric_name,
                        "type": "numericColumn",
                        "minWidth": 170,
                    }
                )

            body = _make_ag_grid(
                f"{stage}-overall-ranking-grid",
                column_defs=column_defs,
                row_data=typed_rows,
                default_col_def={"minWidth": 150},
                grid_options={"suppressPaginationPanel": True},
            )

        return dbc.Card(
            [
                dbc.CardHeader(html.H6("Overall Ranking", className="mb-0")),
                dbc.CardBody(body),
            ],
            className="be-subtab-card",
        )
    summary_grid = build_assessment_overview_table(session_dir, stage)
    return dbc.Card(
        [
            dbc.CardHeader(html.H6("Overall Summary", className="mb-0")),
            dbc.CardBody(summary_grid),
        ],
        className="be-subtab-card",
    )


def build_raw_assessments_tab(session_dir: Path):
    # Collect all *_raw_assessment*.csv files (case-insensitive)
    csv_files = list(session_dir.glob("*.csv"))
    candidates = [
        p
        for p in csv_files
        if any(p.name.lower().endswith(sfx) for sfx in RAW_ASSESSMENT_SUFFIXES)
    ]
    if not candidates:
        body = html.Div("No raw assessment files found.")
        return dcc.Tab(label="Raw Assessments", value="tab-raw", children=html.Div(body))

    items = []
    for path in sorted(candidates, key=lambda p: (_raw_assessment_group_key(p), p.name.lower())):
        # Derive metric name from filename
        metric = _raw_assessment_metric_title(path)
        header, data = _read_csv_rows(path)
        if not header:
            continue
        # Format numerics to 3 decimals
        column_info: List[Tuple[int, str]] = []
        display_headers: List[str] = []
        seen_headers: Set[str] = set()
        for idx, column_name in enumerate(header):
            display = _display_column_name(column_name)
            if display.lower() == "rank":
                display = "Average rank"
            if display in seen_headers:
                continue
            seen_headers.add(display)
            column_info.append((idx, display))
            display_headers.append(display)

        formatted_rows: List[Dict[str, object]] = []
        for raw_row in data:
            record: Dict[str, object] = {}
            for idx, display in column_info:
                cell = raw_row[idx] if idx < len(raw_row) else ""
                key_lower = display.lower()
                if key_lower == "method":
                    record[display] = method_formal_name(str(cell))
                elif key_lower == "geometry":
                    record[display] = _format_geometry_value(cell)
                else:
                    numeric_value = _safe_float(cell)
                    record[display] = numeric_value if numeric_value is not None else cell
            formatted_rows.append(record)
        numeric_headers = {
            col for col in display_headers if any(isinstance(row.get(col), (int, float)) for row in formatted_rows)
        }
        column_defs: List[Dict[str, object]] = []
        for display in display_headers:
            col_def: Dict[str, object] = {
                "headerName": display,
                "field": display,
                "minWidth": 170 if display.lower() != "method" else 220,
            }
            if display.lower() == "method":
                col_def["flex"] = 0
                col_def["pinned"] = "left"
            if display in numeric_headers:
                col_def["type"] = "numericColumn"
            column_defs.append(col_def)
        slug = "".join(ch.lower() if ch.isalnum() else "-" for ch in metric)
        while "--" in slug:
            slug = slug.replace("--", "-")
        slug = slug.strip("-") or "metric"
        table = _make_ag_grid(
            grid_id=f"raw-assessment-{slug}-grid",
            column_defs=column_defs,
            row_data=formatted_rows,
            default_col_def={"minWidth": 160},
            grid_options={"suppressPaginationPanel": True},
        )
        items.append(
            dbc.AccordionItem(html.Div(table, style={"width": "100%"}), title=metric)
        )

    if not items:
        body = html.Div("No valid raw assessment tables found.")
    else:
        body = dbc.Accordion(items, always_open=False)

    return dcc.Tab(label="Raw Assessments", value="tab-raw", children=html.Div(body))


def _method_code_from_display(display: str) -> Optional[str]:
    if not display:
        return None
    code = DISPLAY_TO_CODE.get(display)
    if code:
        return code
    code = DISPLAY_TO_CODE_LOWER.get(display.lower())
    if code:
        return code
    canonical = display.replace(" ", "").replace("-", "").replace("_", "").lower()
    for method_code, nice_name in SUPPORTED_METHODS:
        if canonical == nice_name.replace(" ", "").replace("-", "").replace("_", "").lower():
            return method_code
    return None


def _load_session_summary(session_dir: Path) -> Optional[Dict[str, object]]:
    summary_path = session_dir / "session_summary.json"
    if not summary_path.exists():
        return None
    try:
        return json.loads(summary_path.read_text(encoding="utf-8"))
    except Exception:
        return None


