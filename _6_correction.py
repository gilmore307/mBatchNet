from typing import Dict, List

import json

import dash
from dash import dcc, html
from dash.dependencies import Input, Output, State, MATCH, ALL
import dash_bootstrap_components as dbc

from _1_components import build_navbar
from _2_utils import (
    CODE_TO_DISPLAY,
    SUPPORTED_METHODS,
    any_method_outputs,
    compute_integrated_summary,
    delete_method_outputs,
    get_session_dir,
    load_integrated_summary,
    mark_method_completed,
    method_output_exists,
    run_single_method,
    clear_method_completion,
)


def correction_layout(active_path: str):
    return html.Div(
        [
            build_navbar(active_path),
            dcc.Store(id="method-summary-store", data=None),
            dcc.Store(id="method-operation-trigger", data=0),
            dbc.Container(
                [
                    html.H2("Batch Effect Correction"),
                    html.P(
                        "Review available correction methods. Historical statistics highlight how often each method "
                        "has been selected and the typical runtime."
                    ),
                    html.Div(id="method-table-container", className="mb-3"),
                    html.Div(id="correction-status", className="text-muted"),
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
            return compute_integrated_summary()
        except Exception:
            return load_integrated_summary()

    @app.callback(
        Output("method-table-container", "children"),
        Input("method-summary-store", "data"),
        Input("session-id", "data"),
        Input("method-operation-trigger", "data"),
        prevent_initial_call=False,
    )
    def render_method_table(summary: Dict[str, object] | None, session_id: str | None, _: int):
        summary_lookup: Dict[str, Dict[str, object]] = {}
        if summary and isinstance(summary, dict):
            methods_block = summary.get("methods")
            if isinstance(methods_block, dict):
                for key, payload in methods_block.items():
                    if isinstance(payload, dict):
                        summary_lookup[str(key)] = payload
                        summary_lookup[str(key).lower()] = payload
        session_dir = get_session_dir(session_id) if session_id else None
        session_ready = False
        if session_dir and session_dir.exists():
            session_ready = (session_dir / "raw.csv").exists() and (session_dir / "metadata.csv").exists()
        header = html.Thead(
            html.Tr(
                [
                    html.Th("Method"),
                    html.Th("Times Selected"),
                    html.Th("Avg Time (s)"),
                    html.Th("Status"),
                    html.Th(
                        dcc.Loading(
                            html.Span("Run Correction", id="run-column-loading-indicator"),
                            type="default",
                            parent_className="be-column-loading",
                            className="be-column-loading",
                        ),
                        className="text-center",
                    ),
                    html.Th(
                        dcc.Loading(
                            html.Span("Delete", id="delete-column-loading-indicator"),
                            type="default",
                            parent_className="be-column-loading",
                            className="be-column-loading",
                        ),
                        className="text-center",
                    ),
                ]
            )
        )
        body_rows: List[html.Tr] = []
        row_stores: List[dcc.Store] = []

        def button_color(disabled: bool) -> str:
            return "secondary" if disabled else "success"
        for code, display in SUPPORTED_METHODS:
            stats = summary_lookup.get(code) or summary_lookup.get(code.lower())
            selections = 0
            avg_elapsed = None
            if isinstance(stats, dict):
                try:
                    selections = int(
                        stats.get("selections", stats.get("runs", 0))  # backward compatibility
                    )
                except (TypeError, ValueError):
                    selections = 0
                avg_elapsed = stats.get("avg_elapsed_sec")
            if isinstance(avg_elapsed, str):
                try:
                    avg_elapsed = float(avg_elapsed)
                except ValueError:
                    avg_elapsed = None
            avg_display = "-" if avg_elapsed in (None, "") else f"{float(avg_elapsed):.2f}"
            outputs_present = bool(session_dir and method_output_exists(session_dir, code))
            status_text = "Selected" if outputs_present else "Not selected"
            run_disabled = not session_ready or outputs_present
            delete_disabled = not outputs_present
            status_cell = html.Td(
                dcc.Loading(
                    html.Span(status_text, id={"type": "method-status-label", "code": code}),
                    type="default",
                ),
                className="text-center",
            )
            run_button = dbc.Button(
                "Run Correction",
                id={"type": "method-run-button", "code": code},
                color=button_color(run_disabled),
                size="sm",
                style={"width": "250px"},
                disabled=run_disabled,
                n_clicks=0,
            )
            run_cell = html.Td(
                dcc.Loading(
                    html.Div(run_button, className="d-grid gap-1"),
                    type="default",
                    parent_className="be-column-loading",
                    className="be-column-loading",
                ),
                className="text-center",
            )
            delete_button = dbc.Button(
                "Delete",
                id={"type": "method-delete-button", "code": code},
                color=button_color(delete_disabled),
                size="sm",
                style={"width": "250px"},
                disabled=delete_disabled,
                n_clicks=0,
            )
            delete_cell = html.Td(
                dcc.Loading(
                    html.Div(delete_button, className="d-grid gap-1"),
                    type="default",
                    parent_className="be-column-loading",
                    className="be-column-loading",
                ),
                className="text-center",
            )
            row = html.Tr(
                [
                    html.Td(display),
                    html.Td(str(selections)),
                    html.Td(avg_display),
                    status_cell,
                    run_cell,
                    delete_cell,
                ]
            )
            body_rows.append(row)
            row_stores.append(
                dcc.Store(id={"type": "method-operation-result", "code": code}, data=None)
            )
        table = dbc.Table([header, html.Tbody(body_rows)], bordered=True, hover=True, responsive=True, striped=True, className="align-middle")
        if row_stores:
            return html.Div([table, *row_stores])
        return table

    @app.callback(
        Output({"type": "method-status-label", "code": MATCH}, "children", allow_duplicate=True),
        Output({"type": "method-run-button", "code": MATCH}, "disabled", allow_duplicate=True),
        Output({"type": "method-run-button", "code": MATCH}, "color", allow_duplicate=True),
        Output({"type": "method-delete-button", "code": MATCH}, "disabled", allow_duplicate=True),
        Output({"type": "method-delete-button", "code": MATCH}, "color", allow_duplicate=True),
        Output("run-column-loading-indicator", "children", allow_duplicate=True),
        Output("delete-column-loading-indicator", "children", allow_duplicate=True),
        Output({"type": "method-operation-result", "code": MATCH}, "data", allow_duplicate=True),
        Input({"type": "method-run-button", "code": MATCH}, "n_clicks"),
        State({"type": "method-run-button", "code": MATCH}, "id"),
        State("session-id", "data"),
        State("method-operation-trigger", "data"),
        prevent_initial_call=True,
    )
    def run_correction_method(n_clicks: int, component_id: Dict[str, str], session_id: str | None, refresh_token: int | None):
        if not n_clicks:
            raise dash.exceptions.PreventUpdate
        method_code = component_id.get("code") if isinstance(component_id, dict) else None
        if not method_code:
            raise dash.exceptions.PreventUpdate
        refresh_value = int(refresh_token or 0)
        display_name = CODE_TO_DISPLAY.get(method_code, method_code)
        if not session_id:
            message = "Session not initialised. Upload data before running corrections."
            payload = {"message": message}
            return (
                "Not selected",
                True,
                "secondary",
                True,
                "secondary",
                "Run Correction",
                "Delete",
                payload,
            )
        session_dir = get_session_dir(session_id)
        if not (session_dir / "raw.csv").exists() or not (session_dir / "metadata.csv").exists():
            message = "Upload data before running corrections."
            payload = {"message": message}
            return (
                "Not selected",
                True,
                "secondary",
                True,
                "secondary",
                "Run Correction",
                "Delete",
                payload,
            )
        log_path = session_dir / "run.log"
        success, _ = run_single_method(session_dir, method_code, log_path=log_path)
        new_refresh = refresh_value + 1
        if success:
            mark_method_completed(session_dir, method_code)
            status_text = "Selected"
            run_disabled = True
            delete_disabled = False
            message = f"{display_name} correction complete."
        else:
            clear_method_completion(session_dir, method_code)
            status_text = "Not selected"
            run_disabled = False
            delete_disabled = True
            message = f"{display_name} correction failed. Check logs."
        complete_flag = any_method_outputs(session_dir)
        payload = {
            "message": message,
            "complete": bool(complete_flag),
            "refresh": new_refresh,
            "log_path": str(log_path),
            "log_meta": None,
        }
        run_color = "secondary" if run_disabled else "success"
        delete_color = "secondary" if delete_disabled else "success"
        return (
            status_text,
            run_disabled,
            run_color,
            delete_disabled,
            delete_color,
            "Run Correction",
            "Delete",
            payload,
        )

    @app.callback(
        Output("correction-status", "children"),
        Output("correction-complete", "data"),
        Output("method-operation-trigger", "data"),
        Output("runlog-path", "data", allow_duplicate=True),
        Output("runlog-file-meta", "data", allow_duplicate=True),
        Output("runlog-modal", "is_open", allow_duplicate=True),
        Output("runlog-interval", "disabled", allow_duplicate=True),
        Input({"type": "method-operation-result", "code": ALL}, "data"),
        State({"type": "method-operation-result", "code": ALL}, "id"),
        prevent_initial_call=True,
    )
    def fanout_method_operation(results: List[Dict[str, object]] | None, ids: List[Dict[str, str]] | None):
        ctx = dash.callback_context
        if not ctx.triggered:
            raise dash.exceptions.PreventUpdate
        triggered_raw = ctx.triggered[0]["prop_id"].split(".")[0]
        try:
            triggered_id = json.loads(triggered_raw)
        except json.JSONDecodeError:
            raise dash.exceptions.PreventUpdate

        payload: Dict[str, object] | None = None
        if isinstance(results, list) and isinstance(ids, list):
            for idx, store_id in enumerate(ids):
                if store_id == triggered_id:
                    candidate = results[idx]
                    if isinstance(candidate, dict):
                        payload = candidate
                    break
        if not isinstance(payload, dict):
            raise dash.exceptions.PreventUpdate

        message = payload.get("message") if "message" in payload else dash.no_update
        complete = payload.get("complete") if "complete" in payload else dash.no_update
        refresh = payload.get("refresh") if "refresh" in payload else dash.no_update
        log_path = payload.get("log_path") if "log_path" in payload else dash.no_update
        log_meta = payload.get("log_meta") if "log_meta" in payload else dash.no_update
        open_log = payload.get("open_log") if "open_log" in payload else dash.no_update
        interval_disabled = (
            payload.get("interval_disabled") if "interval_disabled" in payload else dash.no_update
        )

        status_message = message if message is not None else dash.no_update
        complete_flag = bool(complete) if isinstance(complete, bool) else complete

        return (
            status_message,
            complete_flag,
            refresh,
            log_path,
            log_meta,
            open_log,
            interval_disabled,
        )

    @app.callback(
        Output({"type": "method-status-label", "code": MATCH}, "children", allow_duplicate=True),
        Output({"type": "method-run-button", "code": MATCH}, "disabled", allow_duplicate=True),
        Output({"type": "method-run-button", "code": MATCH}, "color", allow_duplicate=True),
        Output({"type": "method-delete-button", "code": MATCH}, "disabled", allow_duplicate=True),
        Output({"type": "method-delete-button", "code": MATCH}, "color", allow_duplicate=True),
        Output("run-column-loading-indicator", "children", allow_duplicate=True),
        Output("delete-column-loading-indicator", "children", allow_duplicate=True),
        Output({"type": "method-operation-result", "code": MATCH}, "data", allow_duplicate=True),
        Input({"type": "method-delete-button", "code": MATCH}, "n_clicks"),
        State({"type": "method-delete-button", "code": MATCH}, "id"),
        State("session-id", "data"),
        State("method-operation-trigger", "data"),
        prevent_initial_call=True,
    )
    def delete_correction_outputs(n_clicks: int, component_id: Dict[str, str], session_id: str | None, refresh_token: int | None):
        if not n_clicks:
            raise dash.exceptions.PreventUpdate
        method_code = component_id.get("code") if isinstance(component_id, dict) else None
        if not method_code:
            raise dash.exceptions.PreventUpdate
        refresh_value = int(refresh_token or 0)
        display_name = CODE_TO_DISPLAY.get(method_code, method_code)
        if not session_id:
            message = "Session not initialised."
            payload = {"message": message}
            return (
                "Not selected",
                True,
                "secondary",
                True,
                "secondary",
                "Run Correction",
                "Delete",
                payload,
            )
        session_dir = get_session_dir(session_id)
        removed = delete_method_outputs(session_dir, method_code)
        session_ready = (session_dir / "raw.csv").exists() and (session_dir / "metadata.csv").exists()
        status_text = "Not selected"
        run_disabled = not session_ready
        delete_disabled = True
        message = f"Removed outputs for {display_name}." if removed else f"No outputs found for {display_name}."
        complete_flag = any_method_outputs(session_dir)
        payload = {
            "message": message,
            "complete": bool(complete_flag),
            "refresh": refresh_value + 1,
        }
        run_color = "secondary" if run_disabled else "success"
        delete_color = "secondary" if delete_disabled else "success"
        return (
            status_text,
            run_disabled,
            run_color,
            delete_disabled,
            delete_color,
            "Run Correction",
            "Delete",
            payload,
        )
