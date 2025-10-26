# ===============================
# File: _6_correction.py
# ===============================
from typing import Dict, List, Set
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
    compute_integrated_summary,
    load_integrated_summary,
    get_completed_methods,
    clear_method_outputs,
    normalize_method_code,
)


METHOD_GRID_COLUMNS = [
    {
        "headerName": "Method",
        "field": "method",
        "headerTooltip": "Display name of the batch-correction method.",
    },
    {
        "headerName": "Runs",
        "field": "runs",
        "type": "numericColumn",
        "headerTooltip": "Number of historical correction runs (excluding example sessions).",
    },
    {
        "headerName": "Avg Time (s)",
        "field": "avg_elapsed_sec",
        "type": "numericColumn",
        "headerTooltip": "Average runtime in seconds, taken from session logs.",
    },
    {
        "headerName": "Status",
        "field": "status_display",
        "suppressMenu": True,
        "sortable": False,
        "filter": False,
        "headerTooltip": "Current run status for this method in the active session.",
    },
    {
        "headerName": "Run",
        "field": "run_label",
        "cellRenderer": "RunActionButton",
        "cellRendererParams": {
            "className": "btn btn-sm btn-primary",
            "tooltip": "Run this correction method for the current session.",
            "style": {"width": "120px"},
        },
        "suppressMenu": True,
        "sortable": False,
        "filter": False,
    },
    {
        "headerName": "Delete",
        "field": "delete_label",
        "cellRenderer": "DeleteActionButton",
        "cellRendererParams": {
            "className": "btn btn-sm btn-outline-danger",
            "tooltip": "Delete this method's outputs and reset its status to unselected.",
            "style": {"width": "120px"},
        },
        "suppressMenu": True,
        "sortable": False,
        "filter": False,
    },
]

METHOD_DISPLAY = {code: name for code, name in SUPPORTED_METHODS}

STATUS_ICONS = {
    "finished": "✅ Finished",
    "unselected": "⭕ Unselected",
}


def correction_layout(active_path: str):
    return html.Div(
        [
            build_navbar(active_path),
            dcc.Store(id="method-summary-store", data=None),
            dcc.Store(id="method-status-store", data={"finished": []}),
            dbc.Container(
                [
                    html.H2("Batch Effect Correction"),
                    html.P(
                        "Select the correction methods to run. Historical performance statistics are used to help "
                        "prioritise methods that typically perform well for your datasets."
                    ),
                    dcc.Loading(
                        id="method-action-loading",
                        type="default",
                        className="w-100",
                        parent_className="be-method-grid-loading-parent",
                        children=html.Div(
                            [
                                dag.AgGrid(
                                    id="method-grid",
                                    columnDefs=METHOD_GRID_COLUMNS,
                                    rowData=[],
                                    className="ag-theme-alpine be-ag-grid mb-3",
                                    dashGridOptions={
                                        "animateRows": False,
                                        "ensureDomOrder": True,
                                        "suppressRowClickSelection": True,
                                        "rowSelection": "none",
                                        "getRowId": {
                                            "function": (
                                                "function(params) {"
                                                " const data = params && params.data ? params.data : {};"
                                                " const code = data.code || data.method;"
                                                " return code ? code : params.rowIndex; }"
                                            ),
                                        },
                                    },
                                    defaultColDef={
                                        "resizable": True,
                                        "sortable": True,
                                        "filter": True,
                                        "flex": 1,
                                        "minWidth": 140,
                                        "cellClass": "be-ag-center",
                                        "headerClass": "be-ag-header-center",
                                        "cellStyle": {
                                            "display": "flex",
                                            "alignItems": "center",
                                            "justifyContent": "center",
                                            "textAlign": "center",
                                        },
                                    },
                                    style={"height": "420px", "width": "100%"},
                                ),
                                html.Div(id="method-action-spinner", style={"display": "none"}),
                            ],
                            className="position-relative",
                        ),
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
    def _status_payload(session_dir) -> Dict[str, List[str]]:
        finished = sorted(get_completed_methods(session_dir)) if session_dir else []
        return {"finished": finished}

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
        Input("method-summary-store", "data"),
        Input("method-status-store", "data"),
        prevent_initial_call=False,
    )
    def populate_method_grid(
        summary: Dict[str, object] | None,
        status_data: Dict[str, object] | None,
    ):
        rows: List[Dict[str, object]] = []
        if summary and isinstance(summary, dict):
            rows = list(summary.get("table_rows", []))  # type: ignore[arg-type]
        for row in rows:
            val = row.get("avg_elapsed_sec")
            if isinstance(val, (int, float)):
                row["avg_elapsed_sec"] = round(val, 3)
        if not rows:
            rows = [
                {
                    "code": code,
                    "method": display,
                    "runs": 0,
                    "avg_elapsed_sec": None,
                }
                for code, display in SUPPORTED_METHODS
            ]
        finished_set: Set[str] = set()
        if isinstance(status_data, dict):
            finished_raw = status_data.get("finished")
            if finished_raw is None:
                finished_raw = status_data.get("completed", [])
            if isinstance(finished_raw, (list, tuple, set)):
                for item in finished_raw:
                    normalized = normalize_method_code(str(item))
                    if normalized:
                        finished_set.add(normalized)
        for row in rows:
            code = row.get("code")
            normalized_code = normalize_method_code(str(code)) if code else ""
            is_finished = normalized_code in finished_set
            row["status_display"] = (
                STATUS_ICONS["finished"] if is_finished else STATUS_ICONS["unselected"]
            )
            row["delete_enabled"] = is_finished
            row["run_label"] = row.get("run_label") or "Run"
            row["delete_label"] = row.get("delete_label") or "Delete"
            row["run_enabled"] = True
        return rows

    @app.callback(
        Output("method-status-store", "data"),
        Input("session-id", "data"),
        prevent_initial_call=False,
    )
    def refresh_method_status(session_id: str | None):
        if not session_id:
            return {"finished": []}
        session_dir = get_session_dir(session_id)
        return _status_payload(session_dir)

    @app.callback(
        Output("correction-status", "children"),
        Output("correction-complete", "data"),
        Output("runlog-path", "data", allow_duplicate=True),
        Output("runlog-file-meta", "data", allow_duplicate=True),
        Output("runlog-modal", "is_open", allow_duplicate=True),
        Output("runlog-interval", "disabled", allow_duplicate=True),
        Output("method-status-store", "data", allow_duplicate=True),
        Output("method-action-spinner", "children", allow_duplicate=True),
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
        event_data = event.get("data") if isinstance(event.get("data"), dict) else event
        if not isinstance(event_data, dict):
            raise dash.exceptions.PreventUpdate
        action = event_data.get("__action") or event_data.get("action")
        if not isinstance(action, str):
            raise dash.exceptions.PreventUpdate
        action_key = action.strip().lower()
        is_run_action = action_key == "run"
        is_delete_action = action_key == "delete"
        if not (is_run_action or is_delete_action):
            raise dash.exceptions.PreventUpdate
        method_code = event_data.get("code")
        if not isinstance(method_code, str):
            raise dash.exceptions.PreventUpdate
        method_code = method_code.strip()
        if not method_code:
            raise dash.exceptions.PreventUpdate
        status_store_update = dash.no_update
        if not session_id:
            return (
                "Upload data before running corrections.",
                False,
                dash.no_update,
                dash.no_update,
                dash.no_update,
                dash.no_update,
                status_store_update,
                dash.no_update,
            )
        session_dir = get_session_dir(session_id)
        if is_run_action and (
            not (session_dir / "raw.csv").exists() or not (session_dir / "metadata.csv").exists()
        ):
            return (
                "Upload data before running corrections.",
                False,
                dash.no_update,
                dash.no_update,
                dash.no_update,
                dash.no_update,
                status_store_update,
                dash.no_update,
            )
        log_path = session_dir / "run.log"
        display_name = METHOD_DISPLAY.get(method_code, method_code)
        if is_delete_action:
            success, message = clear_method_outputs(session_dir, method_code)
            detail = f" ({message})" if message else ""
            status_msg = (
                f"Removed outputs for {display_name} and reset status to unselected.{detail}"
                if success
                else f"Failed to delete outputs for {display_name}: {message}"
            )
            if success:
                status_store_update = _status_payload(session_dir)
            return (
                status_msg,
                False,
                dash.no_update,
                dash.no_update,
                dash.no_update,
                dash.no_update,
                status_store_update,
                "",
            )

        success, log = run_methods(session_dir, [method_code], log_path=log_path)
        status_msg = (
            f"Method {display_name} completed successfully."
            if success
            else f"Method {display_name} failed. Check logs for details."
        )
        if success:
            status_store_update = _status_payload(session_dir)
        return (
            status_msg,
            bool(success),
            str(log_path),
            None,
            dash.no_update,
            dash.no_update,
            status_store_update,
            "",
        )
