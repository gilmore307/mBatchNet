from typing import Dict, List

import json
import re

import dash
from dash import dcc, html
from dash.dependencies import Input, Output, State, MATCH, ALL
import dash_bootstrap_components as dbc

from _1_components import build_navbar
from _2_utils import (
    CODE_TO_DISPLAY,
    METHOD_REFERENCE_BY_CODE,
    SUPPORTED_METHODS,
    _method_code_from_display,
    _load_session_summary,
    extract_method_timings_from_log,
    any_method_outputs,
    delete_method_outputs,
    get_session_dir,
    method_output_exists,
    _remove_method_from_summary,
    clear_method_failure,
    mark_method_failed,
    method_failed_last_run,
    log_file_meta,
    run_single_method,
)


_ITALIC_PATTERN = re.compile(r"<(?:i|em)>(.*?)</(?:i|em)>", re.IGNORECASE)


def _parse_italic_text(text: str) -> object:
    if not text:
        return "-"
    match_found = False
    parts: List[object] = []
    last = 0
    for match in _ITALIC_PATTERN.finditer(text):
        match_found = True
        start, end = match.span()
        if start > last:
            segment = text[last:start]
            if segment:
                parts.append(segment)
        italic_content = match.group(1)
        if italic_content:
            parts.append(html.I(italic_content))
        last = end
    if last < len(text):
        tail = text[last:]
        if tail:
            parts.append(tail)
    if not match_found:
        return text
    return parts


def _parameter_input(code: str, spec: Dict[str, object]) -> dbc.Col:
    name = spec.get("name") or "parameter"
    input_type = spec.get("type") or "text"
    default_value = spec.get("default")
    label = html.Div(name, className="fw-semibold mb-1")
    control: object
    common_props = {
        "id": {"type": "method-config-input", "code": code, "param": name},
    }
    if input_type == "dropdown":
        control = dcc.Dropdown(
            options=spec.get("options"),
            value=default_value,
            clearable=False,
            className="method-config-dropdown",
            **common_props,
        )
    elif input_type == "number":
        control = dbc.Input(
            type="number",
            value=default_value,
            min=spec.get("min"),
            max=spec.get("max"),
            step=spec.get("step"),
            **common_props,
        )
    else:
        control = dbc.Input(type="text", value=default_value, **common_props)
    return dbc.Col([label, control], lg=3, md=6, sm=12, className="mb-3")


def _build_parameter_layout(code: str) -> object | None:
    parameters = _PARAMETER_CONFIG.get(code)
    if not parameters:
        return html.Div(
            [
                html.Div(
                    [html.H6("Correction Parameters", className="mb-0 p-3 bg-light border-bottom")]
                ),
                html.Div(
                    "No configurable parameters available for this method.",
                    className="p-3 text-muted",
                ),
            ],
            className="method-config-wrapper border-top",
        )
    cols: List[dbc.Col] = []
    rows: List[dbc.Row] = []
    for spec in parameters:
        cols.append(_parameter_input(code, spec))
        if len(cols) == 4:
            rows.append(dbc.Row(cols, className="px-3 pt-3"))
            cols = []
    if cols:
        rows.append(dbc.Row(cols, className="px-3 pt-3"))
    header = html.Div(
        [
            html.H6("Correction Parameters", className="mb-0"),
            dbc.Button(
                "Reset to defaults",
                id={"type": "method-config-reset", "code": code},
                color="secondary",
                size="sm",
                className="ms-2",
                outline=False,
            ),
        ],
        className="d-flex align-items-center justify-content-between p-3 bg-light border-bottom gap-2",
    )
    content = html.Div(
        [
            header,
            html.Div(rows, className="method-config-body"),
        ],
        className="method-config-wrapper border-top",
    )
    return content


_LABEL_COLUMN_WIDTH = {"width": "150px", "minWidth": "150px"}
_ACTION_COLUMN_WIDTH = {"width": "250px", "minWidth": "250px"}
_CONFIG_COLUMN_WIDTH = {"width": "120px", "minWidth": "120px"}


_PARAMETER_CONFIG = {
    "ComBat": [
        {
            "name": "par.prior",
            "type": "dropdown",
            "options": [
                {"label": "True", "value": True},
                {"label": "False", "value": False},
            ],
            "default": False,
        }
    ],
    "ConQuR": [
        {
            "name": "logistic_lasso",
            "type": "dropdown",
            "options": [
                {"label": "True", "value": True},
                {"label": "False", "value": False},
            ],
            "default": False,
        },
        {"name": "quantile_type", "type": "text", "default": "standard"},
        {"name": "lambda_quantile", "type": "text", "default": "2p/n"},
        {
            "name": "interplt",
            "type": "dropdown",
            "options": [
                {"label": "True", "value": True},
                {"label": "False", "value": False},
            ],
            "default": False,
        },
        {
            "name": "delta",
            "type": "number",
            "default": 0.4999,
            "min": 0,
            "max": 1,
            "step": 0.0001,
        },
        {"name": "taus", "type": "text", "default": "seq(0.05,0.95,0.05)"},
    ],
    "FAbatch": [
        {
            "name": "minerr",
            "type": "number",
            "default": 0.000001,
            "min": 0,
            "max": 1,
            "step": 0.000001,
        },
        {
            "name": "probcrossbatch",
            "type": "dropdown",
            "options": [
                {"label": "True", "value": True},
                {"label": "False", "value": False},
            ],
            "default": False,
        },
        {
            "name": "maxnbf",
            "type": "number",
            "default": 8,
            "min": 1,
            "step": 1,
        },
    ],
    "MetaDICT": [
        {
            "name": "alpha",
            "type": "number",
            "default": 0.05,
            "min": 0,
            "max": 1,
            "step": 0.01,
        },
        {
            "name": "beta",
            "type": "number",
            "default": 0.2,
            "min": 0,
            "max": 1,
            "step": 0.01,
        },
        {
            "name": "normalization",
            "type": "dropdown",
            "options": [
                {"label": "Upper Quartile (uq)", "value": "uq"},
                {"label": "TMM", "value": "tmm"},
                {"label": "Total Sum Scaling", "value": "tss"},
                {"label": "None", "value": "none"},
            ],
            "default": "uq",
        },
    ],
    "MMUPHin": [
        {
            "name": "zero_inflation",
            "type": "dropdown",
            "options": [
                {"label": "True", "value": True},
                {"label": "False", "value": False},
            ],
            "default": False,
        },
        {
            "name": "conv",
            "type": "number",
            "default": 0.0001,
            "min": 0,
            "step": 0.0001,
        },
    ],
    "PLSDA": [
        {
            "name": "ncomp.trt",
            "type": "number",
            "default": 1,
            "min": 1,
            "step": 1,
        },
        {
            "name": "ncomp.bat",
            "type": "number",
            "default": 5,
            "min": 1,
            "step": 1,
        },
        {
            "name": "keepX.trt",
            "type": "number",
            "default": 50,
            "min": 1,
            "step": 1,
        },
        {
            "name": "near.zero.var",
            "type": "dropdown",
            "options": [
                {"label": "True", "value": True},
                {"label": "False", "value": False},
            ],
            "default": False,
        },
        {
            "name": "balance",
            "type": "dropdown",
            "options": [
                {"label": "True", "value": True},
                {"label": "False", "value": False},
            ],
            "default": False,
        },
    ],
    "RUV": [
        {
            "name": "k",
            "type": "number",
            "default": 2,
            "min": 1,
            "step": 1,
        },
        {
            "name": "use.pseudosample",
            "type": "dropdown",
            "options": [
                {"label": "True", "value": True},
                {"label": "False", "value": False},
            ],
            "default": False,
        },
        {
            "name": "batch.disp",
            "type": "dropdown",
            "options": [
                {"label": "True", "value": True},
                {"label": "False", "value": False},
            ],
            "default": False,
        },
        {
            "name": "zeroinf",
            "type": "dropdown",
            "options": [
                {"label": "True", "value": True},
                {"label": "False", "value": False},
            ],
            "default": False,
        },
    ],
}


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
                        "Review available correction methods along with their runtime for this session and citation details."
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
        Input("method-operation-trigger", "data"),
        Input("session-id", "data"),
        prevent_initial_call=False,
    )
    def refresh_method_summary(
        pathname: str, correction_complete: bool, refresh_token: int, session_id: str | None
    ):
        ctx = dash.callback_context
        triggered = ctx.triggered[0]["prop_id"].split(".")[0] if ctx.triggered else None
        current_path = (pathname or "/").split("?", 1)[0]
        if current_path != "/correction" and triggered in (None, "page-url"):
            return dash.no_update
        session_dir = get_session_dir(session_id) if session_id else None
        if not session_dir or not session_dir.exists():
            return {"methods": {}}
        log_timings = extract_method_timings_from_log(session_dir)
        method_timings: Dict[str, Dict[str, object]] = {
            code: {"elapsed_sec": elapsed} for code, elapsed in log_timings.items()
        }
        try:
            session_summary = _load_session_summary(session_dir)
        except Exception:
            session_summary = None
        if session_summary and isinstance(session_summary, dict):
            methods_block = session_summary.get("methods")
            if isinstance(methods_block, list):
                for entry in methods_block:
                    if not isinstance(entry, dict):
                        continue
                    name = entry.get("name")
                    if not name:
                        continue
                    code = _method_code_from_display(str(name)) or str(name)
                    if code in method_timings:
                        continue
                    elapsed_val = entry.get("elapsed_sec")
                    try:
                        elapsed_float = float(elapsed_val)
                    except (TypeError, ValueError):
                        elapsed_float = None
                    method_timings[code] = {"elapsed_sec": elapsed_float}
        return {"methods": method_timings}

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
                    html.Th("Config", className="text-center", style=_CONFIG_COLUMN_WIDTH),
                    html.Th("Methods", className="text-center", style=_LABEL_COLUMN_WIDTH),
                    html.Th("Time (s)", className="text-center", style=_LABEL_COLUMN_WIDTH),
                    html.Th("Status", className="text-center", style=_LABEL_COLUMN_WIDTH),
                    html.Th("Run Correction", className="text-center", style=_ACTION_COLUMN_WIDTH),
                    html.Th("Delete", className="text-center", style=_ACTION_COLUMN_WIDTH),
                    html.Th("Citation", className="text-center"),
                ]
            )
        )
        body_rows: List[html.Tr] = []
        row_extras: List[object] = []

        def button_color(disabled: bool) -> str:
            return "secondary" if disabled else "success"
        for code, display in SUPPORTED_METHODS:
            stats = summary_lookup.get(code) or summary_lookup.get(code.lower())
            elapsed = None
            if isinstance(stats, dict):
                elapsed = stats.get("elapsed_sec")
            if isinstance(elapsed, str):
                try:
                    elapsed = float(elapsed)
                except ValueError:
                    elapsed = None
            time_display = "-" if elapsed in (None, "") else f"{float(elapsed):.2f}"
            outputs_present = bool(session_dir and method_output_exists(session_dir, code))
            failed_state = bool(session_dir and method_failed_last_run(session_dir, code))
            if outputs_present:
                status_text = "Selected"
            elif failed_state:
                status_text = "Failed"
            else:
                status_text = "Not selected"
            run_disabled = not session_ready or outputs_present
            delete_disabled = not outputs_present
            status_cell = html.Td(
                dcc.Loading(
                    html.Span(status_text, id={"type": "method-status-label", "code": code}),
                    type="default",
                    parent_className="be-cell-loading",
                    className="be-cell-loading",
                ),
                className="text-center",
                style=_LABEL_COLUMN_WIDTH,
            )
            run_button = dbc.Button(
                "Run Correction",
                id={"type": "method-run-button", "code": code},
                color=button_color(run_disabled),
                size="sm",
                style={"width": "200px"},
                disabled=run_disabled,
                n_clicks=0,
            )
            run_cell = html.Td(
                dcc.Loading(
                    html.Div(run_button, className="d-flex justify-content-center"),
                    type="default",
                    parent_className="be-cell-loading",
                    className="be-cell-loading",
                ),
                className="text-center be-run-cell",
                style=_ACTION_COLUMN_WIDTH,
            )
            delete_button = dbc.Button(
                "Delete",
                id={"type": "method-delete-button", "code": code},
                color=button_color(delete_disabled),
                size="sm",
                style={"width": "200px"},
                disabled=delete_disabled,
                n_clicks=0,
            )
            delete_cell = html.Td(
                dcc.Loading(
                    html.Div(delete_button, className="d-flex justify-content-center"),
                    type="default",
                    parent_className="be-cell-loading",
                    className="be-cell-loading",
                ),
                className="text-center be-delete-cell",
                style=_ACTION_COLUMN_WIDTH,
            )
            metadata = METHOD_REFERENCE_BY_CODE.get(code, {})
            package_url = (metadata.get("package") or "").strip()
            citation_text = (metadata.get("citation") or "").strip()
            citation_url = (metadata.get("url") or "").strip()
            method_display = (
                html.A(
                    display,
                    href=package_url,
                    target="_blank",
                    rel="noopener noreferrer",
                )
                if package_url
                else display
            )
            citation_content: object
            citation_children = _parse_italic_text(citation_text) if citation_text else "-"
            if citation_text and citation_url:
                citation_content = html.A(
                    citation_children,
                    href=citation_url,
                    target="_blank",
                    rel="noopener noreferrer",
                )
            elif citation_text:
                citation_content = citation_children
            else:
                citation_content = "-"
            citation_cell = html.Td(citation_content)
            toggle_button = dbc.Button(
                "Show",
                id={"type": "method-config-toggle", "code": code},
                color=button_color(False),
                size="sm",
                className="w-100",
                n_clicks=0,
            )
            toggle_cell = html.Td(
                html.Div(toggle_button, className="d-flex"),
                className="text-center",
                style=_CONFIG_COLUMN_WIDTH,
            )
            row = html.Tr(
                [
                    toggle_cell,
                    html.Td(method_display, className="text-center", style=_LABEL_COLUMN_WIDTH),
                    html.Td(time_display, className="text-center", style=_LABEL_COLUMN_WIDTH),
                    status_cell,
                    run_cell,
                    delete_cell,
                    citation_cell,
                ]
            )
            body_rows.append(row)
            param_layout = _build_parameter_layout(code)
            if param_layout is not None:
                body_rows.append(
                    html.Tr(
                        html.Td(
                            dbc.Collapse(
                                param_layout,
                                id={"type": "method-config-collapse", "code": code},
                                is_open=False,
                            ),
                            colSpan=7,
                            className="p-0",
                        )
                    )
                )
            row_extras.append(
                dcc.Store(id={"type": "method-operation-result", "code": code}, data=None)
            )
        table = dbc.Table(
            [header, html.Tbody(body_rows)],
            bordered=True,
            hover=True,
            responsive=True,
            striped=True,
            className="align-middle",
        )
        children: List[object] = [table]
        if row_extras:
            children.extend(row_extras)
        table_wrapper = html.Div(children, className="be-method-table-wrapper")
        return table_wrapper

    @app.callback(
        Output({"type": "method-config-collapse", "code": MATCH}, "is_open"),
        Output({"type": "method-config-toggle", "code": MATCH}, "children"),
        Input({"type": "method-config-toggle", "code": MATCH}, "n_clicks"),
        State({"type": "method-config-collapse", "code": MATCH}, "is_open"),
        prevent_initial_call=False,
    )
    def toggle_method_config(n_clicks: int | None, is_open: bool | None):
        if not n_clicks:
            raise dash.exceptions.PreventUpdate
        next_state = not bool(is_open)
        return next_state, ("Hide" if next_state else "Show")

    @app.callback(
        Output({"type": "method-config-input", "code": MATCH, "param": ALL}, "value"),
        Input({"type": "method-config-reset", "code": MATCH}, "n_clicks"),
        State({"type": "method-config-input", "code": MATCH, "param": ALL}, "id"),
        prevent_initial_call=True,
    )
    def reset_method_config(n_clicks: int | None, ids: List[Dict[str, object]] | None):
        if not n_clicks:
            raise dash.exceptions.PreventUpdate
        if not ids:
            raise dash.exceptions.PreventUpdate
        method_code = None
        first_id = ids[0]
        if isinstance(first_id, dict):
            method_code = first_id.get("code")
        specs = _PARAMETER_CONFIG.get(method_code) if method_code else None
        default_lookup = {spec.get("name"): spec.get("default") for spec in specs or []}
        defaults: List[object] = []
        for control_id in ids:
            param_name = control_id.get("param") if isinstance(control_id, dict) else None
            defaults.append(default_lookup.get(param_name, dash.no_update))
        return defaults

    @app.callback(
        Output({"type": "method-status-label", "code": MATCH}, "children", allow_duplicate=True),
        Output({"type": "method-run-button", "code": MATCH}, "disabled", allow_duplicate=True),
        Output({"type": "method-run-button", "code": MATCH}, "color", allow_duplicate=True),
        Output({"type": "method-delete-button", "code": MATCH}, "disabled", allow_duplicate=True),
        Output({"type": "method-delete-button", "code": MATCH}, "color", allow_duplicate=True),
        Output({"type": "method-operation-result", "code": MATCH}, "data", allow_duplicate=True),
        Input({"type": "method-run-button", "code": MATCH}, "n_clicks"),
        State({"type": "method-run-button", "code": MATCH}, "id"),
        State("session-id", "data"),
        State("method-operation-trigger", "data"),
        State({"type": "method-config-input", "code": MATCH, "param": ALL}, "value"),
        State({"type": "method-config-input", "code": MATCH, "param": ALL}, "id"),
        prevent_initial_call=True,
    )
    def run_correction_method(
        n_clicks: int,
        component_id: Dict[str, str],
        session_id: str | None,
        refresh_token: int | None,
        values: List[object] | None,
        ids: List[Dict[str, object]] | None,
    ):
        if not n_clicks:
            raise dash.exceptions.PreventUpdate
        method_code = component_id.get("code") if isinstance(component_id, dict) else None
        if not method_code:
            raise dash.exceptions.PreventUpdate
        params: Dict[str, object] = {}
        if isinstance(values, list) and isinstance(ids, list):
            for value, control_id in zip(values, ids):
                if not isinstance(control_id, dict):
                    continue
                param_name = control_id.get("param")
                if not param_name:
                    continue
                params[str(param_name)] = value
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
                payload,
            )
        log_path = session_dir / "run.log"
        success, _ = run_single_method(session_dir, method_code, log_path=log_path, params=params)
        new_refresh = refresh_value + 1 if success else refresh_value
        if success:
            clear_method_failure(session_dir, method_code)
            status_text = "Selected"
            run_disabled = True
            delete_disabled = False
            message = f"{display_name} correction complete."
        else:
            mark_method_failed(session_dir, method_code)
            _remove_method_from_summary(session_dir, method_code)
            status_text = "Failed"
            run_disabled = False
            delete_disabled = True
            message = f"{display_name} correction failed. Check logs."
        complete_flag = any_method_outputs(session_dir)
        payload = {
            "message": message,
            "complete": bool(complete_flag),
            "refresh": new_refresh,
            "log_path": str(log_path),
            "log_meta": log_file_meta(log_path),
        }
        run_color = "secondary" if run_disabled else "success"
        delete_color = "secondary" if delete_disabled else "success"
        return (
            status_text,
            run_disabled,
            run_color,
            delete_disabled,
            delete_color,
            payload,
        )

    @app.callback(
        Output("correction-status", "children"),
        Output("correction-complete", "data"),
        Output("method-operation-trigger", "data"),
        Output("runlog-path", "data", allow_duplicate=True),
        Output("runlog-file-meta", "data", allow_duplicate=True),
        Output("runlog-modal", "is_open", allow_duplicate=True),
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
        status_message = message if message is not None else dash.no_update
        complete_flag = bool(complete) if isinstance(complete, bool) else complete

        return (
            status_message,
            complete_flag,
            refresh,
            log_path,
            log_meta,
            open_log,
        )

    @app.callback(
        Output({"type": "method-status-label", "code": MATCH}, "children", allow_duplicate=True),
        Output({"type": "method-run-button", "code": MATCH}, "disabled", allow_duplicate=True),
        Output({"type": "method-run-button", "code": MATCH}, "color", allow_duplicate=True),
        Output({"type": "method-delete-button", "code": MATCH}, "disabled", allow_duplicate=True),
        Output({"type": "method-delete-button", "code": MATCH}, "color", allow_duplicate=True),
        Output({"type": "method-operation-result", "code": MATCH}, "data", allow_duplicate=True),
        Input({"type": "method-delete-button", "code": MATCH}, "n_clicks"),
        State({"type": "method-delete-button", "code": MATCH}, "id"),
        State("session-id", "data"),
        State("method-operation-trigger", "data"),
        prevent_initial_call=True,
    )
    def delete_correction_outputs(
        n_clicks: int,
        component_id: Dict[str, str],
        session_id: str | None,
        refresh_token: int | None,
    ):
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
                payload,
            )
        session_dir = get_session_dir(session_id)
        removed = delete_method_outputs(session_dir, method_code)
        clear_method_failure(session_dir, method_code)
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
            payload,
        )
