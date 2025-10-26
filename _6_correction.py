# ===============================
# File: _6_correction.py
# ===============================
from typing import Dict, Sequence, List, Set
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
    get_completed_methods,
    clear_method_outputs,
    normalize_method_code,
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
        "headerName": "状态",
        "field": "status_display",
        "width": 120,
        "minWidth": 120,
        "maxWidth": 150,
        "pinned": "right",
        "suppressMenu": True,
        "sortable": False,
        "filter": False,
        "cellClass": "text-center",
        "headerTooltip": "当前会话中该方法的运行状态。",
    },
    {
        "headerName": "",
        "field": "run_label",
        "cellRenderer": "RunActionButton",
        "cellRendererParams": {
            "className": "btn btn-sm btn-primary",
            "tooltip": "Run this correction method for the current session.",
        },
        "width": 120,
        "minWidth": 120,
        "maxWidth": 160,
        "pinned": "right",
        "suppressMenu": True,
        "sortable": False,
        "filter": False,
    },
    {
        "headerName": "",
        "field": "delete_label",
        "cellRenderer": "DeleteActionButton",
        "cellRendererParams": {
            "className": "btn btn-sm btn-outline-danger",
            "tooltip": "删除该方法的结果并将状态重置为待定。",
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

STATUS_ICONS = {
    "completed": "✅ 已完成",
    "pending": "⏳ 待定",
}


def correction_layout(active_path: str):
    return html.Div(
        [
            build_navbar(active_path),
            dcc.Store(id="method-summary-store", data=None),
            dcc.Store(id="method-status-store", data={"completed": []}),
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
    def _status_payload(session_dir) -> Dict[str, List[str]]:
        completed = sorted(get_completed_methods(session_dir)) if session_dir else []
        return {"completed": completed}

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
        Input("method-status-store", "data"),
        State("selected-methods", "data"),
        prevent_initial_call=False,
    )
    def populate_method_grid(
        summary: Dict[str, object] | None,
        status_data: Dict[str, object] | None,
        selected_codes: Sequence[str] | None,
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
        completed_set: Set[str] = set()
        if isinstance(status_data, dict):
            completed_raw = status_data.get("completed", [])
            if isinstance(completed_raw, (list, tuple, set)):
                for item in completed_raw:
                    normalized = normalize_method_code(str(item))
                    if normalized:
                        completed_set.add(normalized)
        for row in rows:
            code = row.get("code")
            normalized_code = normalize_method_code(str(code)) if code else ""
            is_completed = normalized_code in completed_set
            row["status_display"] = STATUS_ICONS["completed"] if is_completed else STATUS_ICONS["pending"]
            row["delete_enabled"] = is_completed
            row["run_label"] = row.get("run_label") or "运行"
            row["delete_label"] = row.get("delete_label") or "删除"
            row["run_enabled"] = True
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
        Output("method-status-store", "data"),
        Input("session-id", "data"),
        prevent_initial_call=False,
    )
    def refresh_method_status(session_id: str | None):
        if not session_id:
            return {"completed": []}
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
            )
        log_path = session_dir / "run.log"
        display_name = METHOD_DISPLAY.get(method_code, method_code)
        if is_delete_action:
            success, message = clear_method_outputs(session_dir, method_code)
            detail = f" ({message})" if message else ""
            status_msg = (
                f"已删除 {display_name} 的输出，并将状态重置为待定。{detail}"
                if success
                else f"删除 {display_name} 输出失败：{message}"
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
        )
