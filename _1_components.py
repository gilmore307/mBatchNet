# ===============================
# File: _1_components.py
# ===============================
from dash import html
import dash_bootstrap_components as dbc

# Nav items (path, label)
NAV_LINKS = (
    ("/", "Home"),
    ("/upload", "Upload Files"),
    ("/pre", "Pre-correction Assessment"),
    ("/correction", "Batch Effect Correction"),
    ("/post", "Post-correction Assessment"),
)

# Stable IDs for navbar buttons
NAV_ID_MAP = {
    "/": "nav-welcome-btn",
    "/upload": "nav-upload-btn",
    "/pre": "nav-pre-btn",
    "/correction": "nav-correction-btn",
    "/post": "nav-post-btn",
}


def logger_box(log_id: str, title: str = "Progress log") -> dbc.Col:
    return dbc.Col(
        dbc.Card(
            [
                dbc.CardHeader(html.Strong(title)),
                dbc.CardBody(
                    dbc.Alert(
                        id=log_id,
                        color="secondary",
                        is_open=False,
                        style={
                            "whiteSpace": "pre-wrap",
                            "fontFamily": "monospace",
                            "maxHeight": "70vh",
                            "overflowY": "auto",
                            "marginBottom": 0,
                        },
                    )
                ),
            ],
            className="h-100",
        ),
        xs=12,
        md=4,
        lg=3,
        className="mb-4",
    )


def build_navbar(active_path: str) -> dbc.Navbar:
    # Row 1: Left (Home, GitHub, Report) | Right (Help, Logs, Download) all same style
    is_home = active_path == "/"
    home_href = "/" if is_home else None  # Intercept when not on Home to confirm restart

    left_group = [
        dbc.Button(
            [html.I(className="bi bi-house me-1"), "Home"],
            id=NAV_ID_MAP["/"],
            href=home_href,
            color="light",
            outline=True,
            className="mb-2 me-2",
            size="sm",
            style={"width": "150px"},
        ),
        dbc.Button(
            [html.I(className="bi bi-github me-1"), "GitHub"],
            color="light",
            outline=True,
            className="mb-2 me-2",
            size="sm",
            href="https://github.com/gilmore307/Batch-Effect-Correction",
            target="_blank",
            style={"width": "150px"},
        ),
        dbc.Button(
            [html.I(className="bi bi-bug me-1"), "Report issue"],
            color="light",
            outline=True,
            className="mb-2 me-2",
            size="sm",
            href="https://github.com/gilmore307/Batch-Effect-Correction/issues",
            target="_blank",
            style={"width": "150px"},
        ),
    ]

    right_group = [
        dbc.Button(
            [html.I(className="bi bi-question-circle me-1"), "Help"],
            id="help-open",
            color="light",
            outline=True,
            className="mb-2 ms-2",
            size="sm",
            style={"width": "150px"},
        ),
        dbc.Button(
            [html.I(className="bi bi-terminal me-1"), "Logs"],
            id="log-open",
            color="light",
            outline=True,
            className="mb-2 ms-2",
            size="sm",
            style={"width": "150px"},
        ),
        dbc.Button(
            [html.I(className="bi bi-download me-1"), "Download results"],
            id="download-results-btn",
            color="light",
            outline=True,
            className="mb-2 ms-2",
            size="sm",
            disabled=True,
            style={"width": "150px"},
        ),
    ]

    # First row layout: left group anchored left, right group anchored right,
    # with a bit of horizontal padding on both sides.
    top_row = html.Div(
        [
            html.Div(left_group, className="d-flex align-items-center flex-wrap"),
            html.Div(right_group, className="d-flex align-items-center flex-wrap ms-auto"),
        ],
        className="d-flex align-items-center flex-wrap justify-content-between w-100 py-1 px-3",
    )

    # Row 2: Pipeline navigation (full-width tab-like steps with arrows)
    bottom_items = []
    for path, label in [x for x in NAV_LINKS if x[0] != "/"]:
        is_active = path == active_path
        # Upload: direct href only when on Home; otherwise intercept for restart modal
        if path == "/upload":
            href = "/upload" if is_home else None
        else:
            href = path
        bottom_items.append(
            dbc.Button(
                label,
                id=NAV_ID_MAP[path],
                href=href,
                color="light",
                outline=not is_active,
                className="be-pipeline-step text-truncate",
                size="sm",
                disabled=False,
            )
        )

    bottom_row = html.Div(
        bottom_items,
        className="be-pipeline-container d-flex align-items-stretch flex-nowrap w-100 py-1",
    )

    return dbc.Navbar(
        dbc.Container([top_row, bottom_row], fluid=True, className="py-1 d-flex flex-column"),
        color="primary",  # brighter background for better contrast
        dark=True,
        className="mb-4 be-navbar",
        sticky="top",
    )


