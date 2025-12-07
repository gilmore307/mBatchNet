# ===============================
# File: _0_main.py
# ===============================
from pathlib import Path
import os
import uuid
import shutil
import zipfile
import tempfile
import time

import dash
from dash import Dash, dcc, html
from dash.dependencies import Input, Output, State, ALL
import dash_bootstrap_components as dbc
from flask import request
from flask_sock import Sock

# Local modules
from _1_components import build_navbar, NAV_LINKS, NAV_ID_MAP
from _2_utils import (
    cleanup_old_sessions,
    get_session_dir,
    OUTPUT_ROOT,
)
from _3_welcome import welcome_layout
from _4_upload import upload_layout, register_upload_callbacks
from _5_assessment import assessment_layout, register_pre_post_callbacks
from _6_correction import correction_layout, register_correction_callbacks
from _7_description import HELP_MODAL_SECTIONS, HELP_SECTION_TOC


app: Dash = dash.Dash(
    __name__,
    external_stylesheets=[dbc.themes.FLATLY],
    suppress_callback_exceptions=True,
)
app.title = "mBatchNet"
server = app.server
sock = Sock(server)


# ---- Layout factory ----
PAGE_FACTORY = {
    "/": welcome_layout,
    "/upload": upload_layout,
    "/pre": lambda active: assessment_layout(active, stage="pre"),
    "/correction": correction_layout,
    "/post": lambda active: assessment_layout(active, stage="post"),
}


def serve_layout() -> html.Div:
    session_id = str(uuid.uuid4())
    cleanup_old_sessions()
    return html.Div(
        [
            dcc.Location(id="page-url", refresh=False),

            # Session + state stores
            dcc.Store(id="session-id", storage_type="session", data=session_id),
            dcc.Store(id="selected-methods", storage_type="session", data=[]),
            dcc.Store(id="upload-complete", storage_type="session", data=False),
            dcc.Store(id="preprocess-complete", storage_type="session", data=False),
            dcc.Store(id="pre-complete", storage_type="session", data=False),
            dcc.Store(id="pre-started", storage_type="session", data=False),
            dcc.Store(id="correction-complete", storage_type="session", data=False),
            dcc.Store(id="post-complete", storage_type="session", data=False),
            # Upload page session flags (global so callbacks won't fail on other pages)
            dcc.Store(id="example-loaded", storage_type="session", data=False),
            dcc.Store(id="upload-last-session", storage_type="session", data=""),
            dcc.Store(id="help-shown", storage_type="session", data=False),
            dcc.Store(id="runlog-path", storage_type="session", data=""),
            dcc.Store(id="runlog-file-meta", storage_type="memory", data=None),
            dcc.Store(id="runlog-scroll-trigger", storage_type="memory", data=None),
            dcc.Store(id="runlog-ws-status", storage_type="memory", data=None),
            dcc.Store(id="runlog-unread", storage_type="session", data=False),
            dcc.Store(id="runlog-last-seen", storage_type="memory", data=None),
            # Target page for restart confirmation (set when clicking Home/Upload)
            dcc.Store(id="restart-target", storage_type="session", data=""),

            # Download
            dcc.Download(id="download-results"),

            # Page mount (preload Home so navbar buttons exist for callbacks)
            html.Div(id="page-content", children=welcome_layout("/")),

            # Confirm restart modal (shown when clicking Upload from other pages)
            dbc.Modal(
                [
                    dbc.ModalHeader(dbc.ModalTitle("Restart session?")),
                    dbc.ModalBody(
                        "Starting a new session clears your current uploads and results. Continue?"
                    ),
                    dbc.ModalFooter(
                        [
                            dbc.Button("Cancel", id="confirm-restart-no", color="secondary", className="me-2"),
                            dbc.Button("Restart", id="confirm-restart-yes", color="danger"),
                        ]
                    ),
                ],
                id="confirm-restart-modal",
                is_open=False,
                backdrop="static",
                keyboard=False,
                centered=True,
            ),

            # Help modal (placeholder)
            dbc.Modal(
                [
                    dbc.ModalHeader(dbc.ModalTitle("Help")),
                    dbc.ModalBody(
                        dbc.Row(
                            [
                                dbc.Col(
                                    [
                                        html.H6("Jump to section", className="mb-3"),
                                        html.Ul(
                                            [
                                                html.Li(
                                                    dcc.Link(
                                                        toc_item["title"],
                                                        href=f"#{toc_item['id']}",
                                                        className="text-decoration-none",
                                                    ),
                                                    className="mb-2",
                                                )
                                                for toc_item in HELP_SECTION_TOC
                                            ],
                                            className="list-unstyled mb-0",
                                        ),
                                    ],
                                    width=3,
                                    className="pe-3",
                                    style={"position": "sticky", "top": 0},
                                ),
                                dbc.Col(
                                    html.Div(HELP_MODAL_SECTIONS, id="help-modal-content"),
                                    width=9,
                                ),
                            ],
                            className="g-4",
                        ),
                        style={"maxHeight": "70vh", "overflowY": "auto"},
                    ),
                    dbc.ModalFooter(
                        dbc.Button("Close", id="help-close", color="secondary")
                    ),
                ],
                id="help-modal",
                is_open=False,
                centered=True,
                size="xl",
                scrollable=True,
            ),

            # Run log modal
            dbc.Modal(
                [
                    dbc.ModalHeader(dbc.ModalTitle("Run Log")),
                    dbc.ModalBody(
                        html.Pre(
                            id="runlog-content",
                            children="Log will appear here...",
                            style={
                                "whiteSpace": "pre-wrap",
                                "fontFamily": "monospace",
                                "maxHeight": "70vh",
                                "overflowY": "auto",
                                "marginBottom": 0,
                            },
                        )
                    ),
                    dbc.ModalFooter(
                        dbc.Button("Close", id="runlog-close", color="secondary")
                    ),
                ],
                id="runlog-modal",
                is_open=False,
                centered=True,
                size="xl",
                scrollable=True,
            ),

        ]
    )


app.layout = serve_layout


app.validation_layout = html.Div(
    [
        serve_layout(),
        upload_layout("/upload"),
        assessment_layout("/pre", stage="pre"),
        correction_layout("/correction"),
        assessment_layout("/post", stage="post"),
    ]
)


# ---- Page routing ----
@app.callback(Output("page-content", "children"), Input("page-url", "pathname"))
def render_page(pathname: str):
    layout_factory = PAGE_FACTORY.get(pathname, welcome_layout)
    return layout_factory(pathname)


# ---- Enable/disable navigation buttons based on stage completion ----
@app.callback(
    Output(NAV_ID_MAP["/"], "disabled"),          # Home (never disabled)
    Output(NAV_ID_MAP["/upload"], "disabled"),    # Upload
    Output(NAV_ID_MAP["/pre"], "disabled"),       # Pre
    Output(NAV_ID_MAP["/correction"], "disabled"),# Correction
    Output(NAV_ID_MAP["/post"], "disabled"),      # Post
    Input("preprocess-complete", "data"),
    Input("pre-started", "data"),
    Input("pre-complete", "data"),
    Input("correction-complete", "data"),
    Input("post-complete", "data"),
)
def gate_nav_buttons(preprocess_done, pre_started, pre_done, correction_done, post_done):
    preprocess_done = bool(preprocess_done)
    pre_started = bool(pre_started)
    pre_done = bool(pre_done)
    correction_done = bool(correction_done)
    post_done = bool(post_done)
    return (
        False,                 # Welcome
        False,                 # Upload always enabled
        not preprocess_done,   # Pre requires preprocess
        not preprocess_done,   # Correction now available after preprocess
        not correction_done,   # Post requires correction
    )


# ---- Intercept Home/Upload nav to confirm restart ----
@app.callback(
    Output("confirm-restart-modal", "is_open"),
    Output("restart-target", "data"),
    Input(NAV_ID_MAP["/upload"], "n_clicks"),
    Input(NAV_ID_MAP["/"], "n_clicks"),
    State("page-url", "pathname"),
    prevent_initial_call=True,
)
def open_restart_modal(upload_clicks: int, home_clicks: int, pathname: str):
    ctx = getattr(dash, "ctx", dash.callback_context)
    if not getattr(ctx, "triggered", None):
        raise dash.exceptions.PreventUpdate
    # Only react to real user clicks; ignore re-mount resets where n_clicks is 0/None
    triggered = ctx.triggered[0]
    trigger_id = getattr(ctx, "triggered_id", None) or triggered["prop_id"].split(".")[0]
    trigger_val = triggered.get("value", None)
    if not trigger_val:
        raise dash.exceptions.PreventUpdate
    current = (pathname or "/").split("?", 1)[0] or "/"

    if trigger_id == NAV_ID_MAP["/upload"]:
        # 允许从 Home -> Upload（或已在 Upload）直接跳转，不弹窗；其它页面 -> Upload 才弹窗
        if current in ("/", "/upload"):
            return False, dash.no_update
        return True, "/upload"

    if trigger_id == NAV_ID_MAP["/"]:
        # 已在 Home 不弹窗；其它页面 -> Home 才弹窗
        if current == "/":
            return False, dash.no_update
        return True, "/"

    raise dash.exceptions.PreventUpdate

@app.callback(
    Output("session-id", "data"),
    Output("selected-methods", "data", allow_duplicate=True),
    Output("upload-complete", "data", allow_duplicate=True),
    Output("preprocess-complete", "data", allow_duplicate=True),
    Output("pre-started", "data", allow_duplicate=True),
    Output("pre-complete", "data", allow_duplicate=True),
    Output("correction-complete", "data", allow_duplicate=True),
    Output("post-complete", "data", allow_duplicate=True),
    Output("page-url", "pathname"),
    Output("confirm-restart-modal", "is_open", allow_duplicate=True),
    Input("confirm-restart-yes", "n_clicks"),
    Input("confirm-restart-no", "n_clicks"),
    State("session-id", "data"),
    State("restart-target", "data"),
    prevent_initial_call=True,
)
def handle_restart(confirm_yes: int, confirm_no: int, current_session: str, restart_target: str):
    ctx = dash.callback_context
    if not ctx.triggered:
        raise dash.exceptions.PreventUpdate
    trigger_id = ctx.triggered[0]["prop_id"].split(".")[0]
    if trigger_id == "confirm-restart-yes":
        # Delete previous session folder if present
        try:
            if current_session:
                old_dir = get_session_dir(current_session)
                if old_dir.exists():
                    shutil.rmtree(old_dir, ignore_errors=True)
        except Exception:
            # Ignore cleanup errors; proceed with new session
            pass
        new_session = str(uuid.uuid4())
        nav_path = restart_target or "/upload"
        return (
            new_session,  # session-id
            [],           # selected-methods
            False,        # upload-complete
            False,        # preprocess-complete
            False,        # pre-started
            False,        # pre-complete
            False,        # correction-complete
            False,        # post-complete
            nav_path,     # navigate to chosen target (Home or Upload)
            False,        # close modal
        )
    else:
        # Cancel: keep everything, just close modal
        return (
            dash.no_update,
            dash.no_update,
            dash.no_update,
            dash.no_update,  # preprocess-complete
            dash.no_update,  # pre-started
            dash.no_update,
            dash.no_update,
            dash.no_update,
            dash.no_update,
            False,
        )

# ---- Download results (ZIP session dir) ----
@app.callback(
    Output("download-results", "data"),
    Input("download-results-btn", "n_clicks"),
    State("session-id", "data"),
    prevent_initial_call=True,
)
def download_results(n_clicks: int, session_id: str):
    if not n_clicks:
        raise dash.exceptions.PreventUpdate
    # Only proceed if the session directory already exists
    if not session_id:
        raise dash.exceptions.PreventUpdate
    session_dir = OUTPUT_ROOT / session_id
    if not session_dir.exists() or not session_dir.is_dir():
        raise dash.exceptions.PreventUpdate
    # Build a zip that excludes any existing results.zip, then save it into the session folder
    target_zip = session_dir / "results.zip"
    # Create zip in a temp location to avoid including it while walking the session dir
    with tempfile.NamedTemporaryFile(delete=False, suffix=".zip", dir=str(OUTPUT_ROOT)) as tmpf:
        tmp_zip_path = Path(tmpf.name)
    try:
        with zipfile.ZipFile(tmp_zip_path, mode="w", compression=zipfile.ZIP_DEFLATED) as zf:
            for p in session_dir.rglob("*"):
                if p.is_file():
                    # Exclude previous results.zip if present
                    if p.name.lower() == "results.zip":
                        continue
                    arcname = p.relative_to(session_dir)
                    zf.write(p, arcname)
        # Move into the session directory (overwrite existing if necessary)
        try:
            if target_zip.exists():
                target_zip.unlink(missing_ok=True)
        except Exception:
            pass
        shutil.move(str(tmp_zip_path), str(target_zip))
    finally:
        # Clean up temp if move failed
        if tmp_zip_path.exists() and not target_zip.exists():
            try:
                tmp_zip_path.unlink()
            except Exception:
                pass
    return dcc.send_file(str(target_zip))


# ---- Register page-specific callbacks ----
register_upload_callbacks(app)
register_pre_post_callbacks(app)
register_correction_callbacks(app)

# Enable Download button whenever the session folder exists
@app.callback(
    Output("download-results-btn", "disabled"),
    Input("session-id", "data"),
    Input("upload-complete", "data"),
    Input("preprocess-complete", "data"),
    Input("pre-started", "data"),
    Input("pre-complete", "data"),
    Input("correction-complete", "data"),
    Input("post-complete", "data"),
)
def enable_download(session_id: str, *_stage_flags) -> bool:
    # Disabled if no session or folder doesn't exist yet
    if not session_id:
        return True
    session_dir = OUTPUT_ROOT / session_id
    return not (session_dir.exists() and session_dir.is_dir())


@app.callback(
    Output("help-modal", "is_open"),
    Output("help-shown", "data"),
    Input({"type": "help-open-trigger", "source": ALL}, "n_clicks"),
    Input("help-close", "n_clicks"),
    State("help-shown", "data"),
    prevent_initial_call=True,
)
def toggle_help_modal(open_clicks_list, close_clicks, help_shown):
    # Only open/close on explicit user clicks; block initial/layout triggers.
    ctx = dash.callback_context
    if not ctx.triggered:
        raise dash.exceptions.PreventUpdate

    trigger_id = ctx.triggered_id
    if isinstance(trigger_id, dict) and trigger_id.get("type") == "help-open-trigger":
        if any(click or 0 for click in open_clicks_list):
            return True, True
    if trigger_id == "help-close" and close_clicks:
        return False, dash.no_update
    raise dash.exceptions.PreventUpdate


# Highlight next-step nav button when user can proceed
@app.callback(
    Output(NAV_ID_MAP["/"], "color"), Output(NAV_ID_MAP["/"], "outline"),
    Output(NAV_ID_MAP["/upload"], "color"), Output(NAV_ID_MAP["/upload"], "outline"),
    Output(NAV_ID_MAP["/pre"], "color"), Output(NAV_ID_MAP["/pre"], "outline"),
    Output(NAV_ID_MAP["/correction"], "color"), Output(NAV_ID_MAP["/correction"], "outline"),
    Output(NAV_ID_MAP["/post"], "color"), Output(NAV_ID_MAP["/post"], "outline"),
    Input("page-url", "pathname"),
    Input("upload-complete", "data"),
    Input("preprocess-complete", "data"),
    Input("pre-started", "data"),
    Input("pre-complete", "data"),
    Input("correction-complete", "data"),
    Input("post-complete", "data"),
)
def highlight_next(
    pathname,
    upload_done,
    preprocess_done,
    pre_started,
    pre_done,
    correction_done,
    post_done,
):
    upload_done = bool(upload_done)
    preprocess_done = bool(preprocess_done)
    pre_started = bool(pre_started)
    pre_done = bool(pre_done)
    correction_done = bool(correction_done)
    post_done = bool(post_done)

    # Determine next step user can take
    if not upload_done:
        next_step = "/upload"
    elif not preprocess_done:
        next_step = "/pre"
    elif not correction_done:
        next_step = "/correction"
    elif not post_done:
        next_step = "/post"
    else:
        next_step = None

    def style_for(path):
        if path == pathname:
            return ("light", False)
        if path == next_step:
            return ("success", True)  # highlight next step as available
        return ("light", True)

    home = style_for("/")
    upload = style_for("/upload")
    pre = style_for("/pre")
    corr = style_for("/correction")
    post = style_for("/post")
    return (
        home[0], home[1],
        upload[0], upload[1],
        pre[0], pre[1],
        corr[0], corr[1],
        post[0], post[1],
    )


# ---- Run log modal handlers ----
@app.callback(
    Output("runlog-modal", "is_open", allow_duplicate=True),
    Output("runlog-file-meta", "data", allow_duplicate=True),
    Input("log-open", "n_clicks"),
    Input("runlog-close", "n_clicks"),
    State("runlog-modal", "is_open"),
    prevent_initial_call=True,
)
def toggle_runlog_modal(open_clicks, close_clicks, is_open):
    # Only user-driven open/close here; job callbacks will also open it explicitly.
    ctx = dash.callback_context
    if not ctx.triggered:
        raise dash.exceptions.PreventUpdate
    trig = ctx.triggered[0]["prop_id"].split(".")[0]
    if trig == "log-open" and open_clicks:
        return True, None
    if trig == "runlog-close" and close_clicks:
        return False, dash.no_update
    return is_open, dash.no_update


@app.callback(
    Output("runlog-unread", "data"),
    Output("runlog-last-seen", "data"),
    Input("runlog-file-meta", "data"),
    Input("runlog-modal", "is_open"),
    State("runlog-last-seen", "data"),
    State("runlog-unread", "data"),
)
def sync_runlog_unread_state(log_meta, modal_open, last_seen, unread):
    meta = log_meta if isinstance(log_meta, dict) else None
    last_seen_meta = last_seen if isinstance(last_seen, dict) else None
    unread_flag = bool(unread)

    if modal_open:
        # Mark everything as read when the modal is opened.
        return False, meta or last_seen_meta

    if meta and meta != last_seen_meta:
        return True, last_seen_meta

    return unread_flag, last_seen_meta


@app.callback(
    Output("log-open", "color"),
    Output("log-open", "outline"),
    Input("runlog-unread", "data"),
)
def style_log_button(unread):
    if unread:
        return "warning", False
    return "light", True


def _resolve_log_path(raw_path: str | None) -> Path | None:
    if not raw_path:
        return None
    try:
        path = Path(raw_path).expanduser().resolve()
    except Exception:
        return None
    try:
        root = OUTPUT_ROOT.resolve()
    except Exception:
        root = OUTPUT_ROOT
    try:
        if path == root or root in path.parents:
            return path
    except Exception:
        return None
    return None


@sock.route("/ws/runlog")
def stream_runlog(ws):
    raw_path = request.args.get("path")
    log_path = _resolve_log_path(raw_path)
    if not log_path:
        try:
            ws.send("Invalid log path.\n")
        finally:
            return

    try:
        ws.send("__runlog_reset__")
    except Exception:
        return

    last_size = 0
    missing_sent = False

    while True:
        try:
            if getattr(ws, "closed", False):
                break

            if not log_path.exists():
                if not missing_sent:
                    ws.send("Waiting for log file...\n")
                    missing_sent = True
                time.sleep(0.6)
                continue

            missing_sent = False
            current_size = log_path.stat().st_size
            if current_size < last_size:
                last_size = 0
                ws.send("__runlog_reset__")

            if current_size > last_size:
                with log_path.open("r", encoding="utf-8", errors="replace") as fh:
                    fh.seek(last_size)
                    chunk = fh.read()
                    last_size = fh.tell()
                if chunk:
                    ws.send(chunk)

            time.sleep(0.4)
        except Exception as exc:  # pragma: no cover - runtime guard
            try:
                ws.send(f"[stream error] {exc}\n")
            except Exception:
                pass
            break


app.clientside_callback(
    """
    function(logPath, isOpen) {
        if (!window.handleRunlogSocket) {
            return window.dash_clientside.no_update;
        }
        return window.handleRunlogSocket(logPath, isOpen);
    }
    """,
    Output("runlog-ws-status", "data"),
    Input("runlog-path", "data"),
    Input("runlog-modal", "is_open"),
)



app.clientside_callback(
    """
    function(content, isOpen) {
        var ctx = window.dash_clientside.callback_context;
        var el = document.getElementById('runlog-content');
        if (!el) {
            return window.dash_clientside.no_update;
        }
        var TOLERANCE = 40;
        if (!el.dataset.listenerAttached) {
            el.addEventListener('scroll', function() {
                var atBottom = (el.scrollHeight - el.scrollTop - el.clientHeight) <= TOLERANCE;
                el.dataset.autoScroll = atBottom ? "1" : "0";
            });
            el.dataset.listenerAttached = "1";
        }
        if (typeof el.dataset.autoScroll === "undefined") {
            el.dataset.autoScroll = "1";
        }
        if (!isOpen) {
            return null;
        }
        var triggered = (ctx.triggered && ctx.triggered[0]) ? ctx.triggered[0].prop_id : "";
        if (triggered === "runlog-modal.is_open") {
            el.dataset.autoScroll = "1";
            window.requestAnimationFrame(function() {
                el.scrollTop = el.scrollHeight;
            });
            return null;
        }
        if (el.dataset.autoScroll !== "0") {
            window.requestAnimationFrame(function() {
                el.scrollTop = el.scrollHeight;
            });
        }
        return null;
    }
    """,
    Output("runlog-scroll-trigger", "data"),
    Input("runlog-content", "children"),
    Input("runlog-modal", "is_open"),
)


if __name__ == "__main__":
    # Production-friendly defaults. Override with env: DASH_DEBUG, HOST, PORT
    debug = (os.getenv("DASH_DEBUG", "1") == "1")
    # On Windows, binding to 0.0.0.0 can be blocked by firewall policies and yield
    # "以一种访问权限不允许的方式做了一个访问套接字的尝试" (WSAEACCES 10013).
    # Use 127.0.0.1 by default on Windows; allow override via HOST env.
    default_host = "127.0.0.1" if os.name == "nt" else "0.0.0.0"
    host = os.getenv("HOST", default_host)
    try:
        port = int(os.getenv("PORT", "8050"))
    except Exception:
        port = 8050
    # Disable hot reload so writing large files (e.g. results.zip) won't refresh the page
    app.run_server(
        debug=debug,
        host=host,
        port=port,
        use_reloader=False,
    )

