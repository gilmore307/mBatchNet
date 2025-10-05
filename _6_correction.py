# ===============================
# File: _6_correction.py
# ===============================
from typing import Sequence
from dash import dcc, html
from dash.dependencies import Input, Output, State
import dash
import dash_bootstrap_components as dbc

from _1_components import build_navbar
from _2_utils import get_session_dir, run_methods, SUPPORTED_METHODS, DEFAULT_METHODS


def correction_layout(active_path: str):
    checklist_options = [
        {"label": display, "value": code} for code, display in SUPPORTED_METHODS
    ]
    return html.Div(
        [
            build_navbar(active_path),
            dbc.Container(
                dbc.Row(
                    [
                        dbc.Col(
                            [
                                html.H2("Batch Effect Correction"),
                                html.P("Select the correction methods to run, then start the pipeline."),
                                html.H4("Select correction methods"),
                                dcc.Checklist(
                                    id="method-selection",
                                    options=checklist_options,
                                    value=list(DEFAULT_METHODS),
                                    inputStyle={"margin-right": "0.5rem"},
                                    labelStyle={"display": "block", "margin-bottom": "0.4rem"},
                                ),
                                dcc.Loading([
                                    dbc.Button(
                                        "Run correction",
                                        id="run-correction",
                                        color="secondary",
                                        className="mb-2",
                                        disabled=True,
                                        size="sm",
                                        style={"width": "250px"},
                                    ),
                                    html.Div(id="correction-status", className="text-muted mb-2"),
                                ], type="default"),
                            ],
                            xs=12,
                            md=8,
                            lg=9,
                        ),
                    ],
                    align="start",
                ),
                fluid=True,
            ),
        ]
    )


def register_correction_callbacks(app):
    @app.callback(Output("selected-methods", "data"), Input("method-selection", "value"), prevent_initial_call=True)
    def sync_method_selection(selected):
        return selected or []

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
