# ===============================
# File: _2_utils.py
# ===============================
import base64
import csv
import json
import os
import sys
import shutil
import subprocess
import textwrap
import time
import hashlib
from datetime import datetime
from statistics import mean
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Sequence, Tuple, Optional, Set

from dash import html, dcc
import dash_ag_grid as dag
import dash_bootstrap_components as dbc

from _7_description import RANKING_SCORE_DESCRIPTIONS

# Paths & constants
BASE_DIR = Path(__file__).resolve().parent
OUTPUT_ROOT = BASE_DIR / "output"
PLOTS_DIR = BASE_DIR / "plots"
METHODS_SCRIPT = BASE_DIR / "methods.R"
PREPROCESS_SCRIPT = BASE_DIR / "preprocess.R"
CLEANUP_HOURS = 6
SESSION_SIGNATURES_PATH = OUTPUT_ROOT / "session_signatures.json"


# ---- Dataclasses & config ----
@dataclass
class FigureSpec:
    label: str
    filename: str


PRE_FIGURES: Sequence[FigureSpec] = (
    FigureSpec("PCA", "pca.png"),
    FigureSpec("PCoA (Aitchison)", "pcoa_aitchison.png"),
    FigureSpec("PCoA (Bray-Curtis)", "pcoa_braycurtis.png"),
    FigureSpec("NMDS (Aitchison)", "nmds_aitchison.png"),
    FigureSpec("NMDS (Bray-Curtis)", "nmds_braycurtis.png"),
    FigureSpec("Dissimilarity heatmaps (Aitchison)", "dissimilarity_heatmaps_aitchison.png"),
    FigureSpec("Dissimilarity heatmaps (Bray-Curtis)", "dissimilarity_heatmaps_braycurtis.png"),
    FigureSpec("R^2 (Aitchison)", "R2_aitchison.png"),
    FigureSpec("pRDA (Aitchison)", "pRDA_aitchison.png"),
    FigureSpec("PVCA", "PVCA.png"),
    FigureSpec("Alignment score", "alignment_score.png"),
    FigureSpec("AUC", "auroc.png"),
)

POST_EXTRA_FIGURES: Sequence[FigureSpec] = (
    FigureSpec("LISI", "LISI.png"),
    FigureSpec("Entropy score", "ebm.png"),
    FigureSpec("Silhouette score", "silhouette.png"),
)

POST_FIGURES: Sequence[FigureSpec] = PRE_FIGURES + POST_EXTRA_FIGURES


PRE_SCRIPTS: Sequence[str] = (
    "pca.R",
    "pcoa.R",
    "NMDS.R",
    "Dissimilarity_Heatmaps.R",
    "R2.R",
    "pRDA.R",
    "pvca.R",
    "Alignment_Score.R",
    "AUC.R",
)

POST_SCRIPTS: Sequence[str] = PRE_SCRIPTS + (
    "LISI.R",
    "Entropy_Score.R",
    "Silhouette.R",
)

RANKING_SCORE_LABELS: Dict[str, str] = {
    "pca": "PCA score",
    "pcoa": "PCoA score",
    "nmds": "NMDS score",
    "dissimilarity": "Dissimilarity score",
    "r2": "R² score",
    "prda": "pRDA score",
    "pvca": "PVCA score",
    "alignment": "Alignment score",
    "auc": "AUC score",
    "lisi": "LISI score",
    "ebm": "Entropy score",
    "silhouette": "Silhouette score",
}


SUPPORTED_METHODS: Sequence[Tuple[str, str]] = (
    ("QN", "QN"),
    ("BMC", "BMC"),
    ("limma", "Limma"),
    ("ConQuR", "ConQuR"),
    ("PLSDA", "PLSDA-batch"),
    ("ComBat", "ComBat"),
    ("FSQN", "FSQN"),
    ("MMUPHin", "MMUPHin"),
    ("RUV", "RUV-III-NB"),
    ("MetaDICT", "MetaDICT"),
    ("SVD", "SVD"),
    ("PN", "PN"),
    ("FAbatch", "FAbatch"),
    ("ComBatSeq", "ComBat-seq"),
    ("DEBIAS", "DEBIAS-M"),
)

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
    "svd": _FORMAL_MAP["svd"],
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
# Surrogate variable decomposition, Percentile normalization
DEFAULT_METHODS: Sequence[str] = ("QN", "BMC", "limma", "SVD", "PN")

CODE_TO_DISPLAY: Dict[str, str] = {code: display for code, display in SUPPORTED_METHODS}
DISPLAY_TO_CODE: Dict[str, str] = {display: code for code, display in SUPPORTED_METHODS}
DISPLAY_TO_CODE_LOWER: Dict[str, str] = {display.lower(): code for code, display in SUPPORTED_METHODS}


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
        env = os.environ.copy()
        # Ensure R's reticulate uses this same Python interpreter by default
        env.setdefault("RETICULATE_PYTHON", sys.executable)
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


def run_command_streaming(command: Sequence[str], cwd: Path, log_path: Path) -> Tuple[bool, str]:
    """Run a command streaming stdout to a log file for real-time viewing.

    Returns (success, combined_log_text_at_end).
    """
    log_path.parent.mkdir(parents=True, exist_ok=True)
    combined: List[str] = []
    try:
        env = os.environ.copy()
        env.setdefault("RETICULATE_PYTHON", sys.executable)
        with log_path.open("a", encoding="utf-8", errors="replace") as logf:
            header = "$ " + " ".join(command) + "\n"
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
            for line in proc.stdout:
                logf.write(line)
                logf.flush()
                combined.append(line)
            rc = proc.wait()
            success = (rc == 0)
            return success, header + "".join(combined)
    except Exception as exc:
        return False, f"$ {' '.join(command)}\n{exc}"


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
        cmd = ("Rscript", str(script_path), str(output_dir), *args)
        if log_path is not None:
            success, log = run_command_streaming(cmd, cwd=BASE_DIR, log_path=log_path)
        else:
            success, log = run_command(cmd, cwd=BASE_DIR)
        logs.append(log)
        if not success:
            return False, "\n\n".join(logs)
    return True, "\n\n".join(logs)


def run_methods(session_dir: Path, methods: Iterable[str], log_path: Optional[Path] = None) -> Tuple[bool, str]:
    method_arg = ",".join(methods)
    command = ("Rscript", str(METHODS_SCRIPT), method_arg, str(session_dir))
    if log_path is not None:
        return run_command_streaming(command, cwd=BASE_DIR, log_path=log_path)
    return run_command(command, cwd=BASE_DIR)


def run_preprocess(session_dir: Path, log_path: Optional[Path] = None) -> Tuple[bool, str]:
    """Run preprocess.R in the project root for a given session directory.

    It writes outputs into the provided session directory and reads the
    matrix from session_dir/raw.csv by default.
    """
    if not PREPROCESS_SCRIPT.exists():
        return False, f"Script not found: {PREPROCESS_SCRIPT.name}"
    matrix_path = session_dir / "raw.csv"
    command = ("Rscript", str(PREPROCESS_SCRIPT), str(session_dir), str(matrix_path))
    if log_path is not None:
        return run_command_streaming(command, cwd=BASE_DIR, log_path=log_path)
    return run_command(command, cwd=BASE_DIR)


def render_figures(session_dir: Path, figures: Sequence[FigureSpec]):
    cards = []
    for spec in figures:
        file_path = session_dir / spec.filename
        if not file_path.exists():
            continue
        encoded = base64.b64encode(file_path.read_bytes()).decode("ascii")
        src = f"data:image/png;base64,{encoded}"
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
        bases = ["pcoa"]
    elif s.startswith("nmds_"):
        bases = ["nmds"]
    elif s.startswith("dissimilarity_") or s.startswith("dissimilarity-") or s.startswith("dissimilarity"):
        bases = ["dissimilarity"]
    elif s.startswith("r2_"):
        bases = ["r2"]
    elif s.startswith("prda_") or s.startswith("prda"):
        bases = ["pRDA", "prda"]
    elif s == "pvca":
        bases = ["PVCA", "pvca"]
    elif s == "alignment_score":
        bases = ["alignment_score"]
    elif s == "auroc":
        bases = ["auroc"]
    elif s == "lisi":
        bases = ["LISI", "lisi"]
    elif s == "ebm":
        bases = ["ebm"]
    elif s == "silhouette":
        bases = ["silhouette"]
    elif s == "pca":
        bases = ["pca"]
    else:
        bases = [stem]

    # Generate candidate filenames (prefer ranking first)
    candidates: List[str] = []
    for b in bases:
        # ranking summary across methods
        candidates.append(f"{b}_ranking.csv")
    for b in bases:
        # baseline-only assessment
        candidates.append(f"{b}_raw_assessment.csv")
    # Special LISI grid file
    for b in bases:
        if b.lower() == "lisi":
            candidates.append(f"{b}_k_grid.csv")
    return candidates


def render_assessment_tabs(session_dir: Path, figures: Sequence[FigureSpec], stage: str = "pre", extra_tabs: Sequence = ()):  # extra dcc.Tab items appended
    """Render a vertical tab set of assessment outputs.

    Group metrics by base (e.g., PCoA, NMDS, R2, pRDA, Dissimilarity heatmaps).
    For groups with multiple geometries (Aitchison/Bray-Curtis), show a single
    top-level tab with sub-tabs: Aitchison, Bray-Curtis, and a third
    sub-tab that is either Assessment (stage='pre') or Rank (stage='post').
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
        if not img_path.exists():
            return html.Div("Image not found.")
        encoded = base64.b64encode(img_path.read_bytes()).decode("ascii")
        src = f"data:image/png;base64,{encoded}"
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
        elif low.startswith("r2_aitchison"):
            add_group_item("r2", "Feature-wise ANOVA R²", "ait", fn)
        elif low.startswith("r2_braycurtis"):
            add_group_item("r2", "Feature-wise ANOVA R²", "bray", fn)
        elif low.startswith("prda_aitchison"):
            add_group_item("prda", "pRDA", "ait", fn)
        elif low.startswith("prda_braycurtis"):
            add_group_item("prda", "pRDA", "bray", fn)
        elif low == "pca.png":
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
        if g["ait"]:
            sub_defs.append(("Aitchison", f"{key}-ait", content_for_image(g["ait"])) )
        if g["bray"]:
            sub_defs.append(("Bray-Curtis", f"{key}-bray", content_for_image(g["bray"])) )
        if g["single"] and not (g["ait"] or g["bray"]):
            sub_defs.append(("Plot", f"{key}-plot", content_for_image(g["single"])) )

        # Third sub-tab: Assessment (pre) or Rank (post)
        third_label = "Assessment" if stage == "pre" else "Rank"
        third_content = None
        if stage == "pre" and rep:
            for cand in _candidate_csvs_for_image(rep):
                if not cand.endswith("_raw_assessment.csv"):
                    continue
                found = _find_file_case_insensitive(session_dir, cand)
                if not (found and found.exists()):
                    continue
                header, data = _read_csv_rows(found)
                if not header:
                    break
                column_info: List[Tuple[int, str]] = []
                display_headers: List[str] = []
                seen_headers: Set[str] = set()
                for idx, column_name in enumerate(header):
                    if column_name.lower() == "score":
                        continue
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
                    row_dict: Dict[str, object] = {}
                    for idx, display in column_info:
                        cell = raw_row[idx] if idx < len(raw_row) else ""
                        key_lower = display.lower()
                        if key_lower == "method":
                            row_dict[display] = method_formal_name(str(cell))
                        elif key_lower == "geometry":
                            row_dict[display] = _format_geometry_value(cell)
                        else:
                            numeric_value = _safe_float(cell)
                            row_dict[display] = _rounded(numeric_value) if numeric_value is not None else cell
                    formatted_rows.append(row_dict)
                numeric_headers = {
                    col for col in display_headers if any(isinstance(row.get(col), (int, float)) for row in formatted_rows)
                }
                column_defs: List[Dict[str, object]] = []
                for display in display_headers:
                    col_def: Dict[str, object] = {
                        "headerName": display,
                        "field": display,
                        "minWidth": 160 if display.lower() != "method" else 200,
                    }
                    if display.lower() == "method":
                        col_def["pinned"] = "left"
                    if display in numeric_headers:
                        col_def["type"] = "numericColumn"
                    column_defs.append(col_def)
                third_content = _make_ag_grid(
                    grid_id=f"{stage}-{key}-assessment",
                    column_defs=column_defs,
                    row_data=formatted_rows,
                    default_col_def={"minWidth": 140},
                )
                break
        if stage == "post":
            third_content = _load_ranking_table_for_key(session_dir, key)
        # Only include the third tab when a corresponding table exists
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

    # Append any extra tabs (e.g., raw assessments, ranking)
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
        if not img_path.exists():
            return html.Div("Image not found.")
        encoded = base64.b64encode(img_path.read_bytes()).decode("ascii")
        src = f"data:image/png;base64,{encoded}"
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
    g: Dict[str, Optional[str]] = {"ait": None, "bray": None, "single": None, "title": key}
    for spec in figures:
        low = spec.filename.lower()
        if key == "pcoa":
            if low.startswith("pcoa_aitchison"):
                g["ait"] = spec.filename
            elif low.startswith("pcoa_braycurtis"):
                g["bray"] = spec.filename
            g["title"] = "PCoA"
        elif key == "nmds":
            if low.startswith("nmds_aitchison"):
                g["ait"] = spec.filename
            elif low.startswith("nmds_braycurtis"):
                g["bray"] = spec.filename
            g["title"] = "NMDS"
        elif key == "dissimilarity":
            if low.startswith("dissimilarity_heatmaps_aitchison"):
                g["ait"] = spec.filename
            elif low.startswith("dissimilarity_heatmaps_braycurtis"):
                g["bray"] = spec.filename
            g["title"] = "Dissimilarity heatmaps"
        elif key == "r2":
            if low.startswith("r2_aitchison"):
                g["ait"] = spec.filename
            elif low.startswith("r2_braycurtis"):
                g["bray"] = spec.filename
            g["title"] = "Feature-wise ANOVA R²"
        elif key == "prda":
            if low.startswith("prda_aitchison"):
                g["ait"] = spec.filename
            elif low.startswith("prda_braycurtis"):
                g["bray"] = spec.filename
            g["title"] = "pRDA"
        elif key == "pca" and spec.filename.lower() == "pca.png":
            g["single"] = spec.filename; g["title"] = "PCA"
        elif key == "pvca" and spec.filename.lower() == "pvca.png":
            g["single"] = spec.filename; g["title"] = "PVCA"
        elif key == "alignment" and spec.filename.lower() == "alignment_score.png":
            g["single"] = spec.filename; g["title"] = "Alignment score"
        elif key == "auc" and spec.filename.lower() == "auroc.png":
            g["single"] = spec.filename; g["title"] = "AUC"
        elif key == "lisi" and spec.filename.lower() == "lisi.png":
            g["single"] = spec.filename; g["title"] = "LISI"
        elif key == "ebm" and spec.filename.lower() == "ebm.png":
            g["single"] = spec.filename; g["title"] = "Entropy score"
        elif key == "silhouette" and spec.filename.lower() == "silhouette.png":
            g["single"] = spec.filename; g["title"] = "Silhouette score"

    sub_defs: List[Tuple[str, str, html.Div]] = []
    if g["ait"]:
        sub_defs.append(("Aitchison", f"{key}-ait", content_for_image(g["ait"])) )
    if g["bray"]:
        sub_defs.append(("Bray-Curtis", f"{key}-bray", content_for_image(g["bray"])) )
    if g["single"] and not (g["ait"] or g["bray"]):
        sub_defs.append(("Plot", f"{key}-plot", content_for_image(g["single"])) )

    # Third subtab content (Assessment/Rank)
    third_label = "Assessment" if stage == "pre" else "Rank"
    third_content = None
    if stage == "pre":
        rep = g["ait"] or g["bray"] or g["single"]
        if rep:
            for cand in _candidate_csvs_for_image(rep):
                if not cand.endswith("_raw_assessment.csv"):
                    continue
                found = _find_file_case_insensitive(session_dir, cand)
                if found and found.exists():
                    header, data = _read_csv_rows(found)
                    if header:
                        lower = [h.lower() for h in header]
                        def _fmt3(v):
                            try:
                                return f"{float(v):.3f}"
                            except Exception:
                                return v
                        data = [[_fmt3(cell) for cell in row] for row in data]
                        formatted_rows: List[Dict[str, object]] = []
                        for row in data:
                            record: Dict[str, object] = {}
                            for idx, col_name in enumerate(header):
                                cell = row[idx] if idx < len(row) else ""
                                if col_name.lower() == "method":
                                    record[col_name] = method_formal_name(str(cell))
                                else:
                                    numeric_value = _safe_float(cell)
                                    record[col_name] = numeric_value if numeric_value is not None else cell
                            formatted_rows.append(record)
                        numeric_headers = {
                            col for col in header if any(isinstance(row.get(col), (int, float)) for row in formatted_rows)
                        }
                        column_defs: List[Dict[str, object]] = []
                        for col in header:
                            col_def: Dict[str, object] = {
                                "headerName": col,
                                "field": col,
                                "minWidth": 160 if col.lower() != "method" else 200,
                            }
                            if col.lower() == "method":
                                col_def["pinned"] = "left"
                                col_def["flex"] = 0
                            if col in numeric_headers:
                                col_def["type"] = "numericColumn"
                            column_defs.append(col_def)
                        third_content = _make_ag_grid(
                            grid_id=f"{stage}-{key}-assessment",
                            column_defs=column_defs,
                            row_data=formatted_rows,
                            default_col_def={"minWidth": 140},
                        )
                    break
    else:
        third_content = _load_ranking_table_for_key(session_dir, key)

    if third_content is not None:
        description = RANKING_SCORE_DESCRIPTIONS.get(key)
        if description:
            third_content = html.Div(
                [
                    third_content,
                    dcc.Markdown(
                        description,
                        className="text-muted be-score-description",
                        mathjax=True,
                        style={
                            "marginTop": "12px",
                            "fontSize": "16pt",
                        },
                    ),
                ],
                style={"width": "100%"},
            )
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
    relative: Optional[float]
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
    # collapse multiple consecutive spaces that may result from replacements
    while "  " in cleaned:
        cleaned = cleaned.replace("  ", " ")
    return cleaned.strip()


def _format_geometry_value(value: object) -> object:
    if not isinstance(value, str):
        return value
    cleaned = value.strip()
    lower = cleaned.lower()
    if lower in {"ait", "aitch", "aitchison", "clr"}:
        return "Aitchison"
    if lower in {"bc", "bray", "braycurtis", "bray-curtis", "tss"}:
        return "Bray-Curtis"
    return cleaned


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
    "alignment_score": ScoreSpec(field="absolute score"),
    "auroc": ScoreSpec(field="absolute score"),
    "dissimilarity": ScoreSpec(field="absolute score"),
    "ebm": ScoreSpec(field="absolute score"),
    "lisi": ScoreSpec(field="absolute score"),
    "nmds": ScoreSpec(),
    "pca": ScoreSpec(),
    "pcoa": ScoreSpec(),
    "prda": ScoreSpec(field="absolute score"),
    "pvca": ScoreSpec(field="absolute score"),
    "r2": ScoreSpec(field="absolute score"),
    "silhouette": ScoreSpec(field="absolute score"),
}


RANKING_FILE_ALIASES: Dict[str, List[str]] = {
    "alignment": ["alignment_score"],
    "alignment_score": ["alignment_score"],
    "auc": ["auroc"],
    "auroc": ["auroc"],
    "dissimilarity": ["dissimilarity"],
    "ebm": ["ebm", "entropy_score"],
    "lisi": ["lisi"],
    "nmds": ["nmds"],
    "pca": ["pca"],
    "pcoa": ["pcoa"],
    "prda": ["prda"],
    "pvca": ["pvca"],
    "r2": ["r2"],
    "silhouette": ["silhouette"],
}


REQUIRED_METHOD_CODES: Set[str] = {code.lower() for code, _ in SUPPORTED_METHODS}

REQUIRED_RANKING_KEYS: Set[str] = {
    "pca",
    "pcoa",
    "nmds",
    "dissimilarity",
    "r2",
    "prda",
    "pvca",
    "alignment",
    "auc",
    "lisi",
    "ebm",
    "silhouette",
}

RANKING_KEY_LOOKUP: Dict[str, str] = {
    alias.lower(): canonical
    for canonical, aliases in RANKING_FILE_ALIASES.items()
    for alias in [canonical, *aliases]
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
        relative_col = field_map.get("relative score")
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
            relative_value = _safe_float(raw_row.get(relative_col)) if relative_col else None
            is_baseline = method_display.lower() == baseline_label
            entries.append(MethodRankingEntry(
                method=method_display,
                method_code=method_code,
                rank=rank_value,
                absolute=absolute,
                relative=relative_value,
                raw={column_display_map[k]: raw_row.get(k, "") for k in original_columns if column_display_map.get(k)},
                is_baseline=is_baseline,
            ))

    baseline_entry = next((e for e in entries if e.is_baseline and e.absolute not in (None, 0)), None)
    baseline_abs = baseline_entry.absolute if baseline_entry else None
    if baseline_entry and baseline_entry.relative is None:
        baseline_entry.relative = 1.0
    if baseline_abs not in (None, 0):
        for entry in entries:
            if entry.relative is None and entry.absolute is not None:
                entry.relative = entry.absolute / baseline_abs
    else:
        for entry in entries:
            entry.relative = None

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
    has_relative = any(entry.relative is not None for entry in data.entries)

    base_columns: List[str] = ["Method"]
    if has_rank:
        base_columns.append("Average rank")
    if has_absolute:
        base_columns.append("Absolute score")
    if has_relative:
        base_columns.append("Relative score")

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
        relative_val = _rounded(entry.relative)
        row: Dict[str, object] = {"Method": entry.method}
        if "Average rank" in base_columns:
            row["Average rank"] = rank_val
            if rank_val is not None:
                numeric_candidates["Average rank"] = True
        if "Absolute score" in base_columns:
            row["Absolute score"] = absolute_val
            if absolute_val is not None:
                numeric_candidates["Absolute score"] = True
        if "Relative score" in base_columns:
            row["Relative score"] = relative_val
            if relative_val is not None:
                numeric_candidates["Relative score"] = True
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
            elif col == "Relative score":
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
            if entry.relative is not None:
                cell_parts.append(f"{_rounded(entry.relative):.3f}x")
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
    candidates = [p for p in csv_files if p.name.lower().endswith("_raw_assessment.csv")]
    rows: List[Dict[str, str]] = []

    def pick_value(header: List[str], data_row: List[str]) -> str:
        hl = [h.lower() for h in header]
        pri = ["score", "auc", "silhouette", "entropy", "alignment", "r2", "mean_between"]
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

    for path in sorted(candidates):
        test_name = path.stem.replace("_raw_assessment", "").replace("_", " ").title()
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
                    row = r; break
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
                    row[metric_name] = _rounded(entry.relative) if entry and entry.relative is not None else None
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
    # Collect all *_raw_assessment.csv files (case-insensitive)
    csv_files = list(session_dir.glob("*.csv"))
    candidates = [p for p in csv_files if p.name.lower().endswith("_raw_assessment.csv")]
    if not candidates:
        body = html.Div("No raw assessment files found.")
        return dcc.Tab(label="Raw Assessments", value="tab-raw", children=html.Div(body))

    items = []
    for path in sorted(candidates):
        # Derive metric name from filename
        metric = path.stem.replace("_raw_assessment", "").replace("_", " ").title()
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


def _file_sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _example_signatures() -> List[tuple[str, str]]:
    ex_dir = BASE_DIR / "assets" / "example"
    if not ex_dir.exists():
        return []
    raws: Dict[str, Path] = {}
    metas: Dict[str, Path] = {}
    for p in ex_dir.iterdir():
        if not p.is_file():
            continue
        name = p.name
        if name.lower().startswith("raw_"):
            key = name.split("_", 1)[1]
            raws[key] = p
        elif name.lower().startswith("metadata_"):
            key = name.split("_", 1)[1]
            metas[key] = p
    signatures: List[tuple[str, str]] = []
    for key in set(raws) & set(metas):
        try:
            signatures.append((_file_sha256(raws[key]), _file_sha256(metas[key])))
        except Exception:
            continue
    return signatures


def _session_signature(session_dir: Path) -> tuple[str, str] | None:
    raw_path = session_dir / "raw.csv"
    meta_path = session_dir / "metadata.csv"
    if not raw_path.exists() or not meta_path.exists():
        return None
    try:
        return _file_sha256(raw_path), _file_sha256(meta_path)
    except Exception:
        return None


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


def _load_known_signatures() -> Set[tuple[str, str]]:
    if not SESSION_SIGNATURES_PATH.exists():
        return set()
    try:
        data = json.loads(SESSION_SIGNATURES_PATH.read_text(encoding="utf-8"))
    except Exception:
        return set()
    signatures: Set[tuple[str, str]] = set()
    if isinstance(data, list):
        for item in data:
            if isinstance(item, (list, tuple)) and len(item) == 2:
                signatures.add((str(item[0]), str(item[1])))
    return signatures


def _store_known_signatures(signatures: Set[tuple[str, str]]) -> None:
    try:
        serialisable = sorted([list(sig) for sig in signatures])
        SESSION_SIGNATURES_PATH.write_text(json.dumps(serialisable, indent=2), encoding="utf-8")
    except Exception:
        pass


def _load_ranking_deltas(session_dir: Path) -> Dict[str, Dict[str, float]]:
    """Return {method_code: {metric_name: score_delta}} for a session."""
    ret: Dict[str, Dict[str, float]] = defaultdict(dict)
    for csv_path in sorted(session_dir.glob("*_ranking.csv")):
        data = _parse_ranking_file(csv_path)
        if not data.entries:
            continue
        baseline_entry = next((e for e in data.entries if e.is_baseline and e.absolute not in (None, 0)), None)
        baseline_abs = baseline_entry.absolute if baseline_entry else None
        for entry in data.entries:
            if entry.is_baseline:
                continue
            method_code = entry.method_code
            if not method_code:
                continue
            absolute = entry.absolute
            if absolute is None:
                continue
            if entry.relative is not None:
                ratio = entry.relative
            elif baseline_abs not in (None, 0):
                ratio = absolute / baseline_abs
            else:
                continue
            if ratio is None:
                continue
            ret[method_code][data.display_name] = ratio - 1.0
    return ret


def _collect_ranking_metric_keys(session_dir: Path) -> Set[str]:
    metrics: Set[str] = set()
    for csv_path in session_dir.glob("*_ranking.csv"):
        stem = csv_path.stem
        if stem.lower().endswith("_ranking"):
            stem = stem[:-len("_ranking")]
        canonical = RANKING_KEY_LOOKUP.get(stem.lower())
        if canonical:
            metrics.add(canonical)
    return metrics


def _all_methods_selected(summary: Dict[str, object]) -> bool:
    methods = summary.get("methods")
    if not isinstance(methods, list):
        return False
    codes: Set[str] = set()
    for entry in methods:
        if not isinstance(entry, dict):
            continue
        raw_name = (entry.get("name") or "").strip()
        if not raw_name:
            continue
        lowered = raw_name.lower()
        if lowered in REQUIRED_METHOD_CODES:
            codes.add(lowered)
            continue
        code = _method_code_from_display(raw_name)
        if code:
            codes.add(code.lower())
    return REQUIRED_METHOD_CODES.issubset(codes)


def compute_integrated_summary() -> Dict[str, object]:
    """Aggregate cross-session performance statistics and persist the summary."""
    ensure_output_root()
    example_signatures = set(_example_signatures())
    known_signatures = _load_known_signatures()
    seen_signatures: Set[tuple[str, str]] = set()
    method_stats: Dict[str, Dict[str, object]] = {
        code: {
            "code": code,
            "display": CODE_TO_DISPLAY.get(code, code),
            "runs": 0,
            "elapsed": [],
            "deltas": [],
            "metric_deltas": defaultdict(list),
            "sessions": set(),
        }
        for code, _ in SUPPORTED_METHODS
    }
    included_sessions: List[str] = []
    for session_dir in sorted(p for p in OUTPUT_ROOT.iterdir() if p.is_dir()):
        signature = _session_signature(session_dir)
        if signature:
            if signature in example_signatures:
                continue
            if signature in known_signatures or signature in seen_signatures:
                continue
        summary = _load_session_summary(session_dir)
        ranking = _load_ranking_deltas(session_dir)
        metrics_seen = _collect_ranking_metric_keys(session_dir)
        if not summary or not ranking:
            continue
        if not _all_methods_selected(summary):
            continue
        if not REQUIRED_RANKING_KEYS.issubset(metrics_seen):
            continue
        if signature:
            seen_signatures.add(signature)
            known_signatures.add(signature)
        session_id = session_dir.name
        included_sessions.append(session_id)
        for entry in summary.get("methods", []):
            name = entry.get("name")
            status = (entry.get("status") or "").lower()
            if status != "success":
                continue
            code = name if name in method_stats else _method_code_from_display(str(name))
            if not code or code not in method_stats:
                continue
            stat = method_stats[code]
            stat["runs"] = int(stat["runs"]) + 1
            stat["sessions"].add(session_id)
            elapsed_val = entry.get("elapsed_sec")
            try:
                elapsed_float = float(elapsed_val)
            except (TypeError, ValueError):
                elapsed_float = None
            if elapsed_float is not None:
                stat["elapsed"].append(elapsed_float)
        if ranking:
            for code, metric_map in ranking.items():
                if code not in method_stats:
                    continue
                stat = method_stats[code]
                stat["sessions"].add(session_id)
                for metric_name, delta in metric_map.items():
                    stat["deltas"].append(delta)
                    stat["metric_deltas"][metric_name].append(delta)

    rows: List[Dict[str, object]] = []
    methods_payload: Dict[str, Dict[str, object]] = {}
    for code, payload in method_stats.items():
        elapsed_vals: List[float] = payload["elapsed"]  # type: ignore[assignment]
        delta_vals: List[float] = payload["deltas"]  # type: ignore[assignment]
        metric_map = payload["metric_deltas"]  # type: ignore[assignment]
        avg_elapsed = mean(elapsed_vals) if elapsed_vals else None
        avg_delta = mean(delta_vals) if delta_vals else None
        metric_avgs = {metric: mean(vals) for metric, vals in metric_map.items() if vals}
        best_metric = max(metric_avgs, key=metric_avgs.get) if metric_avgs else None
        worst_metric = min(metric_avgs, key=metric_avgs.get) if metric_avgs else None
        methods_payload[code] = {
            "code": code,
            "display": payload["display"],
            "runs": int(payload["runs"]),
            "sessions": sorted(payload["sessions"]),
            "avg_elapsed_sec": avg_elapsed,
            "avg_score_delta": avg_delta,
            "best_metric": best_metric,
            "worst_metric": worst_metric,
            "metrics": {
                metric: {
                    "avg_delta": metric_avgs[metric],
                    "count": len(metric_map[metric]),
                }
                for metric in metric_avgs
            },
        }
        rows.append(
            {
                "code": code,
                "method": payload["display"],
                "runs": int(payload["runs"]),
                "avg_elapsed_sec": round(avg_elapsed, 3) if avg_elapsed is not None else None,
                "avg_score_delta": round(avg_delta, 3) if avg_delta is not None else None,
                "best_metric": best_metric or "-",
                "worst_metric": worst_metric or "-",
            }
        )
    rows.sort(key=lambda item: (-(item["avg_score_delta"] or 0.0), item["method"]))
    summary = {
        "generated_at": datetime.utcnow().strftime("%Y-%m-%d %H:%M:%S"),
        "sessions": included_sessions,
        "methods": methods_payload,
        "table_rows": rows,
        "notes": "Normalized score ratios use the uncorrected baseline (Before correction) as 1.0.",
    }
    try:
        integrated_path = OUTPUT_ROOT / "integrated_summary.json"
        integrated_path.write_text(json.dumps(summary, indent=2), encoding="utf-8")
    except Exception:
        pass
    _store_known_signatures(known_signatures)
    return summary


def load_integrated_summary() -> Dict[str, object]:
    integrated_path = OUTPUT_ROOT / "integrated_summary.json"
    if integrated_path.exists():
        try:
            return json.loads(integrated_path.read_text(encoding="utf-8"))
        except Exception:
            pass
    return compute_integrated_summary()

