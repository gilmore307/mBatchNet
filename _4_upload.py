# ===============================
# File: _4_upload.py
# ===============================
from typing import List, Tuple, Dict, Optional
from pathlib import Path
import shutil
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
    BASE_DIR,
)


EXAMPLE_DIR = BASE_DIR / "assets" / "example"


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


def upload_layout(active_path: str):
    # Prepare example set options
    example_pairs = _scan_example_sets()
    example_options = [
        {"label": f"Example {k}", "value": k} for (k, _r, _m) in example_pairs
    ]
    default_example = example_options[0]["value"] if example_options else None
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
                                                            [
                                                                "Select an example pair from ",
                                                                html.Code("assets/example"),
                                                                ". Files are matched by suffix: ",
                                                                html.Code("raw_<key>"),
                                                                " and ",
                                                                html.Code("metadata_<key>"),
                                                            ]
                                                        ),
                                                        dcc.Dropdown(
                                                            id="example-select",
                                                            options=example_options,
                                                            value=default_example,
                                                            placeholder="Select an example dataset",
                                                            clearable=False,
                                                            style={"maxWidth": "420px"},
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
                ],
                fluid=True,
            ),
        ]
    )


def register_upload_callbacks(app):
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
                if example_key:
                    res = _example_pair_for(example_key)
                    if res:
                        pair = res
                if pair is None:
                    pairs = _scan_example_sets()
                    if pairs:
                        _key, raw_src, meta_src = pairs[0]
                        pair = (raw_src, meta_src)
                if pair is None:
                    example_status = html.Span("No example datasets found in assets/example.", className="text-danger")
                else:
                    raw_src, meta_src = pair
                    if raw_src.exists():
                        shutil.copy2(raw_src, session_dir / "raw.csv")
                    if meta_src.exists():
                        shutil.copy2(meta_src, session_dir / "metadata.csv")
                    # Build file info
                    if (session_dir / "raw.csv").exists():
                        size = (session_dir / "raw.csv").stat().st_size
                        matrix_info = (
                            dbc.Badge("Loaded", color="info", className="me-2"),
                            html.Span(f"Source: {raw_src.relative_to(BASE_DIR)} | Saved as: raw.csv | {human_size(size)}"),
                        )
                    if (session_dir / "metadata.csv").exists():
                        size = (session_dir / "metadata.csv").stat().st_size
                        metadata_info = (
                            dbc.Badge("Loaded", color="info", className="me-2"),
                            html.Span(f"Source: {meta_src.relative_to(BASE_DIR)} | Saved as: metadata.csv | {human_size(size)}"),
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
            save_uploaded_file(metadata_contents, session_dir, "metadata.csv")
            size = (session_dir / "metadata.csv").stat().st_size
            metadata_info = (
                dbc.Badge("Uploaded", color="success", className="me-2"),
                html.Span(f"Source: {metadata_name} | Saved as: metadata.csv | {human_size(size)}"),
            )
            saved_items.append(f"Metadata saved as metadata.csv (source: {metadata_name})")

        upload_complete = (session_dir / "raw.csv").exists() and (session_dir / "metadata.csv").exists()
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
        meta_path = session_dir / "metadata.csv"
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
            mapping_display = dbc.Card([
                dbc.CardHeader(html.Strong("Metadata mapping")),
                dbc.CardBody([
                    html.Div("Using the following mapping for example data:"),
                    html.Ul([
                        html.Li([html.Code("batch_id"), " column -> batch_id"]),
                        html.Li([html.Code("phenotype"), " column -> phenotype"]),
                    ], className="mb-0"),
                ]),
            ], className="mt-2")

            info = html.Div([
                html.H6("Columns in metadata.csv:"),
                chips,
            ])
            return info, mapping_display
        else:
            # Manual flow: require explicit user selection (no defaults)
            opts = [{"label": name, "value": name} for name in col_names]
            mapping_ui = dbc.Card([
                dbc.CardHeader(html.Strong("Map metadata columns")),
                dbc.CardBody([
                    dbc.Row([
                        dbc.Col([
                            dbc.Label("Batch ID column"),
                            dcc.Dropdown(
                                id="map-batch-id",
                                options=opts,
                                value=None,
                                placeholder="Select batch_id column",
                                clearable=True,
                            ),
                        ], md=6),
                        dbc.Col([
                            dbc.Label("Phenotype column"),
                            dcc.Dropdown(
                                id="map-phenotype",
                                options=opts,
                                value=None,
                                placeholder="Select phenotype column",
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
                html.H6("Columns in metadata.csv:"),
                chips,
            ])
            return info, mapping_ui

    @app.callback(
        Output("preprocess-complete", "data", allow_duplicate=True),
        Output("runlog-path", "data", allow_duplicate=True),
        Output("runlog-modal", "is_open", allow_duplicate=True),
        Output("runlog-interval", "disabled", allow_duplicate=True),
        Input("apply-mapping", "n_clicks"),
        State("map-batch-id", "value"),
        State("map-phenotype", "value"),
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
        meta_path = session_dir / "metadata.csv"
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
                    new_fieldnames.append("batch_id")
                elif name == pheno_col:
                    new_fieldnames.append("phenotype")
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
        except Exception as exc:
            return False, dash.no_update, dash.no_update, dash.no_update

        # Run preprocess.R in the session directory
        # Use a single session-wide log file and append to it
        log_path = session_dir / "run.log"
        ok, log = run_preprocess(session_dir, log_path=log_path)
        # Do not auto-open logs modal; user can open manually
        return bool(ok), str(log_path), dash.no_update, dash.no_update

    # Auto-preprocess when example data is loaded (no button press needed)
    @app.callback(
        Output("preprocess-complete", "data", allow_duplicate=True),
        Output("runlog-path", "data", allow_duplicate=True),
        Output("runlog-modal", "is_open", allow_duplicate=True),
        Output("runlog-interval", "disabled", allow_duplicate=True),
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
            return False, dash.no_update, dash.no_update, dash.no_update
        session_dir = get_session_dir(session_id)
        meta_path = session_dir / "metadata.csv"
        if not meta_path.exists():
            return False, dash.no_update, dash.no_update, dash.no_update

        # Ensure standard header names if needed (batch_id, phenotype)
        try:
            import csv
            with meta_path.open("r", encoding="utf-8", newline="") as fh:
                reader = csv.DictReader(fh)
                orig_fieldnames = reader.fieldnames or []
                rows = list(reader)
            # If already present, no rewrite; else try case-insensitive match
            lower = [c.lower() for c in orig_fieldnames]
            need_write = False
            new_fieldnames = list(orig_fieldnames)
            if "batch_id" not in lower:
                need_write = True
            if "phenotype" not in lower:
                need_write = True
            if need_write:
                # Map exact matches ignoring case
                def pick(name, target):
                    for i, c in enumerate(orig_fieldnames):
                        if c.lower() == target:
                            return i
                    return None
                bi = pick("batch_id", "batch_id")
                pi = pick("phenotype", "phenotype")
                # Build new headers where found
                new_fieldnames = []
                for i, name in enumerate(orig_fieldnames):
                    if bi is not None and i == bi:
                        new_fieldnames.append("batch_id")
                    elif pi is not None and i == pi:
                        new_fieldnames.append("phenotype")
                    else:
                        new_fieldnames.append(name)
                with meta_path.open("w", encoding="utf-8", newline="") as fh:
                    writer = csv.DictWriter(fh, fieldnames=new_fieldnames)
                    writer.writeheader()
                    for row in rows:
                        out_row = {}
                        for old, new in zip(orig_fieldnames, new_fieldnames):
                            out_row[new] = row.get(old)
                        writer.writerow(out_row)
        except Exception:
            pass

        # Kick off preprocess and append to the session-wide log
        log_path = session_dir / "run.log"
        ok, _ = run_preprocess(session_dir, log_path=log_path)
        # Do not auto-open logs modal; user can open manually
        return bool(ok), str(log_path), dash.no_update, dash.no_update

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
        Input("map-phenotype", "value"),
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
        State("page-url", "pathname"),
        prevent_initial_call=False,
    )
    def preview_example(selected_key: str, pathname: str):
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

        def _table_for(path: Path, title: str):
            try:
                rows_preview = []
                row_count = 0
                header = None
                with path.open("r", encoding="utf-8", newline="") as fh:
                    reader = _csv.reader(fh)
                    for i, row in enumerate(reader):
                        if title == "Metadata" and i == 0:
                            header = row
                            continue
                        row_count += 1
                        if len(rows_preview) < 5:
                            rows_preview.append(row)
                if title == "Metadata" and header is None:
                    return html.Div(f"{title}: empty file", className="text-muted")
                col_count = (
                    len(header) if title == "Metadata" else (len(rows_preview[0]) if rows_preview else 0)
                )
                thead = (
                    html.Thead(html.Tr([html.Th(c) for c in header])) if title == "Metadata" else None
                )
                def _fmt_cell(val: str) -> str:
                    if title == "Raw Matrix":
                        try:
                            return f"{float(val):.3f}"
                        except Exception:
                            return val
                    return val
                tbody = html.Tbody([
                    html.Tr([html.Td(_fmt_cell(c)) for c in r]) for r in rows_preview
                ])
                size = path.stat().st_size if path.exists() else 0
                meta = f"{row_count} rows × {col_count} cols"
                return dbc.Card([
                    dbc.CardHeader(html.Strong(f"{title} — {meta} ({human_size(size)})")),
                    dbc.CardBody(
                        html.Div(
                            dbc.Table(
                                [thead, tbody],
                                bordered=True,
                                hover=True,
                                size="sm",
                                className="mb-0",
                                style={"whiteSpace": "nowrap"}
                            ),
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
