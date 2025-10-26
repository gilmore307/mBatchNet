# ===============================
# File: _6_correction.py
# ===============================
from typing import Dict, Sequence, List
from dash import dcc, html
from dash.dependencies import Input, Output, State
import dash
import dash_bootstrap_components as dbc
import dash_ag_grid as dag

from _1_components import build_navbar
from _2_utils import (
    get_session_dir,
    run_methods,
    SUPPORTED_METHODS,
    DEFAULT_METHODS,
    compute_integrated_summary,
    load_integrated_summary,
)


METHOD_GRID_COLUMNS = [
    {
        "headerName": "Method",
        "field": "method",
        "checkboxSelection": True,
        "headerCheckboxSelection": True,
        "flex": 1,
        "minWidth": 200,
        "pinned": "left",
        "headerTooltip": "Display name of the batch-correction method. Use the checkboxes to select methods.",
    },
    {
        "headerName": "Runs",
        "field": "runs",
        "type": "numericColumn",
        "width": 90,
        "headerTooltip": "Number of historical correction runs (excluding example sessions).",
    },
    {
        "headerName": "Avg Time (s)",
        "field": "avg_elapsed_sec",
        "type": "numericColumn",
        "width": 140,
        "headerTooltip": "Average runtime in seconds, taken from session logs.",
    },
    {
        "headerName": "Avg Δ (normalized)",
        "field": "avg_score_delta",
        "type": "numericColumn",
        "width": 150,
        "headerTooltip": "Average normalized score change relative to the uncorrected baseline (1.0).",
    },
    {
        "headerName": "Best Metric",
        "field": "best_metric",
        "flex": 1,
        "minWidth": 160,
        "headerTooltip": "Metric for which the method achieved its highest average improvement.",
    },
    {
        "headerName": "Worst Metric",
        "field": "worst_metric",
        "flex": 1,
        "minWidth": 160,
        "headerTooltip": "Metric where the method performed the weakest on average.",
    },
    {
        "headerName": "",
        "field": "run_action",
        "cellRenderer": "ButtonRenderer",
        "cellRendererParams": {
            "label": "Run",
            "className": "btn btn-sm btn-primary",
            "tooltip": "Run this correction method for the current session.",
            "onClick": {"function": "function(e){return {code: e.data.code};}"},
        },
        "width": 120,
        "minWidth": 120,
        "maxWidth": 160,
        "pinned": "right",
        "suppressMenu": True,
        "sortable": False,
        "filter": False,
    },
]

METHOD_DISPLAY = {code: name for code, name in SUPPORTED_METHODS}


def correction_layout(active_path: str):
    return html.Div(
        [
            build_navbar(active_path),
            dcc.Store(id="method-summary-store", data=None),
            dbc.Container(
                [
                    html.H2("Batch Effect Correction"),
                    html.P(
                        "Select the correction methods to run. Historical performance statistics are used to help "
                        "prioritise methods that typically perform well for your datasets."
                    ),
                    dag.AgGrid(
                        id="method-grid",
                        columnDefs=METHOD_GRID_COLUMNS,
                        rowData=[],
                        className="ag-theme-alpine mb-3",
                        dashGridOptions={
                            "rowSelection": "multiple",
                            "animateRows": False,
                            "suppressRowClickSelection": False,
                            "ensureDomOrder": True,
                        },
                        defaultColDef={
                            "resizable": True,
                            "sortable": True,
                            "filter": True,
                        },
                        style={"height": "420px", "width": "100%"},
                    ),
                    dbc.Alert(
                        "Results remain private unless you choose to share anonymised summaries using the consent prompt "
                        "on this page.",
                        color="info",
                        dismissable=False,
                        className="mb-3",
                    ),
                    html.Div(id="correction-status", className="text-muted mb-3"),
                ],
                fluid=True,
            ),
        ]
    )


def register_correction_callbacks(app):
    @app.callback(
        Output("method-summary-store", "data"),
        Input("page-url", "pathname"),
        Input("correction-complete", "data"),
        prevent_initial_call=False,
    )
    def refresh_method_summary(pathname: str, correction_complete: bool):
        ctx = dash.callback_context
        triggered = ctx.triggered[0]["prop_id"].split(".")[0] if ctx.triggered else None
        current_path = (pathname or "/").split("?", 1)[0]
        if triggered == "page-url" and current_path != "/correction":
            return dash.no_update
        if triggered is None and current_path != "/correction":
            return dash.no_update
        try:
            # Recompute on demand to keep the cached summary fresh
            return compute_integrated_summary()
        except Exception:
            return load_integrated_summary()

    @app.callback(
        Output("method-grid", "rowData"),
        Output("method-grid", "selectedRows"),
        Input("method-summary-store", "data"),
        State("selected-methods", "data"),
        prevent_initial_call=False,
    )
    def populate_method_grid(summary: Dict[str, object] | None, selected_codes: Sequence[str] | None):
        rows: List[Dict[str, object]] = []
        if summary and isinstance(summary, dict):
            rows = list(summary.get("table_rows", []))  # type: ignore[arg-type]
        for row in rows:
            for key in ("avg_elapsed_sec", "avg_score_delta"):
                val = row.get(key)
                if isinstance(val, (int, float)):
                    row[key] = round(val, 3)
        if not rows:
            rows = [
                {
                    "code": code,
                    "method": display,
                    "runs": 0,
                    "avg_elapsed_sec": None,
                    "avg_score_delta": None,
                    "best_metric": "-",
                    "worst_metric": "-",
                }
                for code, display in SUPPORTED_METHODS
            ]
        selected_set = set(selected_codes or DEFAULT_METHODS)
        selected_rows = [row for row in rows if row.get("code") in selected_set]
        return rows, selected_rows

    @app.callback(
        Output("selected-methods", "data", allow_duplicate=True),
        Input("method-grid", "selectedRows"),
        prevent_initial_call=True,
    )
    def sync_selected_methods(selected_rows: List[Dict[str, object]] | None):
        if not selected_rows:
            return []
        codes = [row.get("code") for row in selected_rows if row.get("code")]
        return codes

    @app.callback(
        Output("correction-status", "children"),
        Output("correction-complete", "data"),
        Output("runlog-path", "data", allow_duplicate=True),
        Output("runlog-file-meta", "data", allow_duplicate=True),
        Output("runlog-modal", "is_open", allow_duplicate=True),
        Output("runlog-interval", "disabled", allow_duplicate=True),
        Input("method-grid", "cellRendererData"),
        State("session-id", "data"),
        prevent_initial_call=True,
    )
    def perform_correction(cell_event: Dict[str, object] | List[Dict[str, object]], session_id: str):
        if not cell_event:
            raise dash.exceptions.PreventUpdate
        # Support both the dict (single event) and list (history) payload shapes
        if isinstance(cell_event, list):
            if not cell_event:
                raise dash.exceptions.PreventUpdate
            event = cell_event[-1]
        else:
            event = cell_event
        if not isinstance(event, dict):
            raise dash.exceptions.PreventUpdate
        column_id = (
            event.get("colId")
            or event.get("columnId")
            or event.get("col")
            or (event.get("column") or {}).get("colId")
        )
        if column_id and "run" not in str(column_id).lower():
            raise dash.exceptions.PreventUpdate
        method_code = (
            (event.get("code") if isinstance(event.get("code"), str) else None)
            or (event.get("value") if isinstance(event.get("value"), str) else None)
            or ((event.get("rowData") or event.get("data") or {}).get("code") if isinstance(event.get("rowData") or event.get("data"), dict) else None)
        )
        if not method_code:
            raise dash.exceptions.PreventUpdate
        method_code = method_code.strip()
        if not method_code:
            raise dash.exceptions.PreventUpdate
        if not session_id:
            return (
                "Upload data before running corrections.",
                False,
                dash.no_update,
                dash.no_update,
                dash.no_update,
                dash.no_update,
            )
        session_dir = get_session_dir(session_id)
        if not (session_dir / "raw.csv").exists() or not (session_dir / "metadata.csv").exists():
            return (
                "Upload data before running corrections.",
                False,
                dash.no_update,
                dash.no_update,
                dash.no_update,
                dash.no_update,
            )
        log_path = session_dir / "run.log"
        success, log = run_methods(session_dir, [method_code], log_path=log_path)
        display_name = METHOD_DISPLAY.get(method_code, method_code)
        status_msg = (
            f"Method {display_name} completed successfully."
            if success
            else f"Method {display_name} failed. Check logs for details."
        )
        return status_msg, bool(success), str(log_path), None, dash.no_update, dash.no_update
