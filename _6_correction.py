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
        "headerName": "Code",
        "field": "code",
        "checkboxSelection": True,
        "headerCheckboxSelection": True,
        "width": 110,
        "pinned": "left",
        "headerTooltip": "Internal method code. Select methods using the checkboxes.",
    },
    {
        "headerName": "Method",
        "field": "method",
        "flex": 1,
        "minWidth": 180,
        "headerTooltip": "Display name of the batch-correction method.",
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
        "valueFormatter": "value == null ? '' : Number(value).toFixed(3)",
        "width": 140,
        "headerTooltip": "Average runtime in seconds, taken from session logs.",
    },
    {
        "headerName": "Avg Score Δ",
        "field": "avg_score_delta",
        "type": "numericColumn",
        "valueFormatter": "value == null ? '' : Number(value).toFixed(3)",
        "width": 150,
        "headerTooltip": "Average score improvement relative to the uncorrected baseline (Before correction).",
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
]


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
                    dcc.Loading(
                        [
                            dbc.Button(
                                "Run correction",
                                id="run-correction",
                                color="secondary",
                                className="mb-2",
                                disabled=True,
                                size="sm",
                                style={"width": "250px"},
                            ),
                            html.Div(id="correction-status", className="text-muted mb-3"),
                        ],
                        type="default",
                    ),
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
        Output("runlog-modal", "is_open", allow_duplicate=True),
        Output("runlog-interval", "disabled", allow_duplicate=True),
        Input("run-correction", "n_clicks"),
        State("selected-methods", "data"),
        State("session-id", "data"),
        prevent_initial_call=True,
    )
    def perform_correction(n_clicks: int, methods: Sequence[str], session_id: str):
        if not n_clicks:
            raise dash.exceptions.PreventUpdate
        if not session_id:
            return dash.no_update, False, dash.no_update, dash.no_update, dash.no_update
        session_dir = get_session_dir(session_id)
        if not (session_dir / "raw.csv").exists() or not (session_dir / "metadata.csv").exists():
            return dash.no_update, False, dash.no_update, dash.no_update, dash.no_update
        methods = methods or []
        if not methods:
            return dash.no_update, False, dash.no_update, dash.no_update, dash.no_update

        # Append all correction logs to a single session-wide log file
        log_path = session_dir / "run.log"
        success, log = run_methods(session_dir, methods, log_path=log_path)
        # Do not auto-open logs modal; user can open manually
        status_msg = "Correction complete." if success else "Correction failed. Check Logs."
        return status_msg, bool(success), str(log_path), dash.no_update, dash.no_update
