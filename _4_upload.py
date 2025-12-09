# ===============================
# File: _4_upload.py
# ===============================
from typing import List, Tuple, Dict, Optional
from pathlib import Path
import shutil
import json
from urllib.parse import parse_qs
from dash import dcc, html
from dash.dependencies import Input, Output, State
import dash
import dash_bootstrap_components as dbc

from _1_components import build_navbar
from _2_utils import (
    get_session_dir,
    save_uploaded_file,
    human_size,
    run_preprocess,
    run_r_scripts,
    log_file_meta,
    BASE_DIR,
    _make_ag_grid,
    _encode_image_source,
)


EXAMPLE_DIR = BASE_DIR / "assets" / "example"
PREVIEW_MAX_ROWS = 5
PREVIEW_MAX_COLS = 50

# Mapping presets for example datasets (case-insensitive keys)
EXAMPLE_COLUMN_MAP: Dict[str, Dict[str, object]] = {
    "ad": {
        "batch": "Batch",
        "target": "Initial Phenol Concentration",
        "covariates": ["Treatment Duration"],
    },
}


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
            dbc.CardBody(img),
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
                                                            dbc.CardHeader(html.Strong("Count matrix (CSV)")),
                                                            dbc.CardBody(
                                                                dcc.Upload(
                                                                    id="upload-matrix",
                                                                    children=html.Div(["Drag & drop or click to upload count matrix"]),
                                                                    multiple=False,
                                                                    className="border border-secondary rounded p-4 text-center bg-light",
                                                                    accept=".csv,text/csv",
                                                                )
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
                                                                dcc.Upload(
                                                                    id="upload-metadata",
                                                                    children=html.Div(["Drag & drop or click to upload metadata"]),
                                                                    multiple=False,
                                                                    className="border border-secondary rounded p-4 text-center bg-light",
                                                                    accept=".csv,text/csv",
                                                                )
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
                                                        html.P(
                                                            "Load a curated example dataset to explore the workflow without uploading your own files."
                                                        ),
                                                        html.Div(
                                                            [
                                                                html.Div(
                                                                    "Example dataset: Anaerobic Digestion (ad)",
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

        if desired_tab in {"manual", "example"} and desired_tab != current_tab:
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
        Input("upload-matrix", "contents"),
        Input("upload-metadata", "contents"),
        Input("load-example", "n_clicks"),
        State("upload-matrix", "filename"),
        State("upload-metadata", "filename"),
        State("session-id", "data"),
        State("example-select", "value"),
        prevent_initial_call=True,
    )
    def handle_upload(
        matrix_contents,
        metadata_contents,
        load_example_clicks,
        matrix_name,
        metadata_name,
        session_id,
        example_key,
    ):
        if not session_id:
            return False, "No file uploaded yet.", "No file uploaded yet.", dash.no_update, dash.no_update

        session_dir = get_session_dir(session_id)
        saved_items: List[str] = []
        matrix_info = dash.no_update
        metadata_info = dash.no_update
        example_status = dash.no_update
        example_loaded_out = dash.no_update

        # Determine trigger
        ctx = dash.callback_context
        trig = ctx.triggered[0]["prop_id"].split(".")[0] if ctx.triggered else None

        if trig == "load-example":
            # Copy selected example files into the session directory
            try:
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
            except Exception:
                example_status = html.Span("Failed to load example files.", className="text-danger")

        if matrix_contents and trig == "upload-matrix":
            save_uploaded_file(matrix_contents, session_dir, "raw.csv")
            size = (session_dir / "raw.csv").stat().st_size
            matrix_info = (
                dbc.Badge("Uploaded", color="success", className="me-2"),
                html.Span(f"Source: {matrix_name} | Saved as: raw.csv | {human_size(size)}"),
            )
            saved_items.append(f"Matrix saved as raw.csv (source: {matrix_name})")

        if metadata_contents and trig == "upload-metadata":
            save_uploaded_file(metadata_contents, session_dir, "metadata_origin.csv")
            size = (session_dir / "metadata_origin.csv").stat().st_size
            metadata_info = (
                dbc.Badge("Uploaded", color="success", className="me-2"),
                html.Span(f"Source: {metadata_name} | Saved as: metadata_origin.csv | {human_size(size)}"),
            )
            saved_items.append(f"Metadata saved as metadata_origin.csv (source: {metadata_name})")

        upload_complete = (session_dir / "raw.csv").exists() and (session_dir / "metadata_origin.csv").exists()
        return upload_complete, matrix_info, metadata_info, example_status, example_loaded_out

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
                dbc.CardHeader(html.Strong("Map Metadata Columns")),
                dbc.CardBody(
                    [
                        html.P(
                            "Select the control phenotype (will be treated as Negative) and the reference batch before generating the mosaic plot.",
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
            mapping_display = dbc.Card([
                dbc.CardHeader(html.Strong("Metadata mapping")),
                dbc.CardBody([
                    html.Div("Using the following mapping for example data:"),
                    html.Ul([
                        html.Li([html.Code(batch_label), " column -> batch"]),
                        html.Li([html.Code(target_label), " column -> target_binary (binary copy written for correction)"]),
                        html.Li([html.Code(", ".join(map(str, covar_labels)) or "(none)"), " column(s) -> covariates"]),
                    ], className="mb-0"),
                ]),
            ], className="mt-2")

            info = html.Div([
                html.H6("Columns in metadata_origin.csv:"),
                chips,
            ])
            return info, mapping_display
        else:
            # Manual flow: require explicit user selection (no defaults)
            opts = [{"label": name, "value": name} for name in col_names]
            mapping_ui = dbc.Card([
                dbc.CardHeader(html.Strong("Map Metadata Columns")),
                dbc.CardBody([
                    dbc.Row([
                        dbc.Col([
                            dbc.Label("Batch ID column"),
                            dcc.Dropdown(
                                id="map-batch-id",
                                options=opts,
                                value=None,
                                placeholder="Select batch column",
                                clearable=True,
                            ),
                        ], md=6),
                        dbc.Col([
                            dbc.Label("Target (binary) column"),
                            dcc.Dropdown(
                                id="map-target-binary",
                                options=opts,
                                value=None,
                                placeholder="Select column to convert to target_binary (e.g., positive/negative)",
                                clearable=True,
                            ),
                        ], md=6),
                    ], className="gy-2"),
                    dbc.Button(
                        "Apply mapping and preprocess",
                        id="apply-mapping",
                        color="secondary",  # gray until both selections made
                        className="mt-3",
                        disabled=True,
                        style={"width": "250px"},
                        size="sm",
                    ),
                ])
            ], className="mt-2")

            info = html.Div([
                html.H6("Columns in metadata_origin.csv:"),
                chips,
            ])
            return info, mapping_ui

    @app.callback(
        Output("preprocess-complete", "data", allow_duplicate=True),
        Output("runlog-path", "data", allow_duplicate=True),
        Output("runlog-file-meta", "data", allow_duplicate=True),
        Output("runlog-modal", "is_open", allow_duplicate=True),
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
            return False, dash.no_update, dash.no_update, dash.no_update
        if not batch_col or not pheno_col:
            return False, dash.no_update, dash.no_update, dash.no_update

        session_dir = get_session_dir(session_id)
        meta_path = session_dir / "metadata_origin.csv"
        if not meta_path.exists():
            return False, dash.no_update, dash.no_update, dash.no_update

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
            return False, dash.no_update, dash.no_update, dash.no_update

        # Run preprocess.R in the session directory
        # Use a single session-wide log file and append to it
        log_path = session_dir / "run.log"
        ok, log = run_preprocess(session_dir, log_path=log_path)
        # Do not auto-open logs modal; user can open manually
        return bool(ok), str(log_path), log_file_meta(log_path), dash.no_update

    # Auto-preprocess when example data is loaded (no button press needed)
    @app.callback(
        Output("preprocess-complete", "data", allow_duplicate=True),
        Output("runlog-path", "data", allow_duplicate=True),
        Output("runlog-file-meta", "data", allow_duplicate=True),
        Output("runlog-modal", "is_open", allow_duplicate=True),
        Output("mosaic-preview", "children", allow_duplicate=True),
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
            return False, dash.no_update, dash.no_update, dash.no_update, dash.no_update
        session_dir = get_session_dir(session_id)
        meta_path = session_dir / "metadata_origin.csv"
        if not meta_path.exists():
            return False, dash.no_update, dash.no_update, dash.no_update, dash.no_update

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

        # Kick off preprocess and append to the session-wide log
        log_path = session_dir / "run.log"
        ok, _ = run_preprocess(session_dir, log_path=log_path)
        # Do not auto-open logs modal; user can open manually
        mosaic_children = dash.no_update
        if ok:
            cfg_ok, _ = _persist_study_settings(session_dir, "0", "A")
            if cfg_ok:
                mosaic_ok, _ = _generate_mosaic(session_dir)
                if mosaic_ok:
                    mosaic_children = _render_mosaic_card(session_dir)
        return bool(ok), str(log_path), log_file_meta(log_path), dash.no_update, mosaic_children

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

    # Hide the entire Process section when Example tab is active
    @app.callback(
        Output("process-area", "style"),
        Input("upload-tabs", "value"),
    )
    def toggle_process_area(active_tab: str):
        if active_tab == "example":
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
