# ===============================
# File: _3_welcome.py
# ===============================
from dash import html
import dash_bootstrap_components as dbc
from _1_components import build_navbar


def welcome_layout(active_path: str):
    return html.Div(
        [
            build_navbar(active_path),
            dbc.Container(
                [
                    dbc.Row(
                        dbc.Col(
                            dbc.Card(
                                dbc.CardBody(
                                    [
                                        dbc.Badge("Welcome", color="primary", className="text-uppercase mb-2"),
                                        html.H1("Batch-Effect Explorer", className="display-5 mb-2"),
                                        html.P(
                                            "Assess, correct, and visualise technical variation across omics datasets "
                                            "in a streamlined, browser-based workspace.",
                                            className="lead text-muted",
                                        ),
                                        html.Div(
                                            [
                                                dbc.Button(
                                                    [html.I(className="bi bi-upload me-2"), "Start with your data"],
                                                    color="primary",
                                                    href="/upload",
                                                    size="lg",
                                                    className="me-2",
                                                ),
                                                dbc.Button(
                                                    [html.I(className="bi bi-collection-play me-2"), "Try sample workflow"],
                                                    color="secondary",
                                                    outline=True,
                                                    href="/upload?tab=example",
                                                    size="lg",
                                                ),
                                            ],
                                            className="d-flex flex-wrap align-items-center mt-3",
                                        ),
                                    ]
                                ),
                                className="shadow-sm border-0 bg-light",
                            ),
                            md=10,
                            lg=9,
                        ),
                        className="justify-content-center mb-4",
                    ),
                    dbc.Row(
                        [
                            dbc.Col(
                                dbc.Card(
                                    [
                                        dbc.CardHeader([html.I(className="bi bi-rocket-takeoff me-2"), "Quick start"]),
                                        dbc.CardBody(
                                            html.Ol(
                                                [
                                                    html.Li("Upload count matrices and metadata (or load the example dataset)."),
                                                    html.Li("Review pre-correction QC plots to spot batch effects early."),
                                                    html.Li("Run automated correction workflows with sensible defaults."),
                                                    html.Li("Compare post-correction diagnostics and download results."),
                                                ]
                                            )
                                        ),
                                    ],
                                    className="h-100 shadow-sm",
                                ),
                                md=6,
                                lg=4,
                                className="mb-4",
                            ),
                            dbc.Col(
                                dbc.Card(
                                    [
                                        dbc.CardHeader([html.I(className="bi bi-stars me-2"), "Why researchers use it"]),
                                        dbc.CardBody(
                                            html.Ul(
                                                [
                                                    html.Li("No installation required; everything runs in the browser."),
                                                    html.Li("Interactive figures with shareable links for collaboration."),
                                                    html.Li("Multiple correction methods with transparent defaults."),
                                                    html.Li("Session privacy: uploads stay within your session and are cleared when you finish."),
                                                ]
                                            )
                                        ),
                                    ],
                                    className="h-100 shadow-sm",
                                ),
                                md=6,
                                lg=4,
                                className="mb-4",
                            ),
                            dbc.Col(
                                dbc.Card(
                                    [
                                        dbc.CardHeader([html.I(className="bi bi-shield-check me-2"), "Best practices"]),
                                        dbc.CardBody(
                                            [
                                                html.P(
                                                    "We follow NAR Web Server guidelines: HTTPS access, example data, "
                                                    "and rich outputs at every step."
                                                ),
                                                html.P(
                                                    "We keep sessions self-contained: data, figures, and logs live only "
                                                    "while you are working and are discarded when the session ends."
                                                ),
                                            ],
                                        ),
                                    ],
                                    className="h-100 shadow-sm",
                                ),
                                md=12,
                                lg=4,
                                className="mb-4",
                            ),
                        ],
                        className="mb-2",
                    ),
                    dbc.Row(
                        dbc.Col(
                            dbc.Card(
                                [
                                    dbc.CardHeader([html.I(className="bi bi-diagram-3 me-2"), "Workflow overview"]),
                                    dbc.CardBody(
                                        dbc.Row(
                                            [
                                                dbc.Col(
                                                    [
                                                        html.H5("Upload"),
                                                        html.P(
                                                            "Bring count matrices and metadata in common tabular formats. "
                                                            "Use the provided example to explore the interface first."
                                                        ),
                                                    ],
                                                    md=6,
                                                    lg=3,
                                                ),
                                                dbc.Col(
                                                    [
                                                        html.H5("Pre-correction"),
                                                        html.P(
                                                            "Visualise quality metrics, PCA/UMAP embeddings, and batch labels "
                                                            "to diagnose technical variation."
                                                        ),
                                                    ],
                                                    md=6,
                                                    lg=3,
                                                ),
                                                dbc.Col(
                                                    [
                                                        html.H5("Correction"),
                                                        html.P(
                                                            "Run established methods with curated defaults, or tweak parameters "
                                                            "to match your study design."
                                                        ),
                                                    ],
                                                    md=6,
                                                    lg=3,
                                                ),
                                                dbc.Col(
                                                    [
                                                        html.H5("Assessment"),
                                                        html.P(
                                                            "Compare before/after diagnostics, export plots, and download "
                                                            "processed matrices for downstream use."
                                                        ),
                                                    ],
                                                    md=6,
                                                    lg=3,
                                                ),
                                            ],
                                            className="g-3",
                                        )
                                    ),
                                ],
                                className="shadow-sm mb-4",
                            ),
                            lg=12,
                        )
                    ),
                    dbc.Row(
                        [
                            dbc.Col(
                                dbc.Card(
                                    [
                                        dbc.CardHeader([html.I(className="bi bi-info-circle me-2"), "Helpful resources"]),
                                        dbc.CardBody(
                                            html.Ul(
                                                [
                                                    html.Li("Built-in tooltips explain parameters inline as you work."),
                                                    html.Li("Downloadable tables accompany each plot for transparent reporting."),
                                                    html.Li("Session-specific logs track progress for reproducibility."),
                                                ]
                                            )
                                        ),
                                    ],
                                    className="h-100 shadow-sm",
                                ),
                                md=6,
                                className="mb-4",
                            ),
                            dbc.Col(
                                dbc.Card(
                                    [
                                        dbc.CardHeader([html.I(className="bi bi-lock me-2"), "Data handling"]),
                                        dbc.CardBody(
                                            [
                                                html.P(
                                                    "All uploads remain private within your browser session and are not "
                                                    "shared or retained once your work is complete."
                                                ),
                                                html.P(
                                                    "When you finish, use the Download results button in the navigation bar "
                                                    "to save corrected outputs and logs."
                                                ),
                                            ],
                                        ),
                                    ],
                                    className="h-100 shadow-sm",
                                ),
                                md=6,
                                className="mb-4",
                            ),
                        ],
                        className="mb-2",
                    ),
                ],
                fluid=True,
                className="pb-4",
            ),
            html.Footer(
                dbc.Container(
                    [
                        html.P(
                            "Batch-Effect Explorer is an open-source tool designed to help researchers "
                            "assess, correct, and interpret batch effects across omics datasets. All "
                            "resources and tools on this website are freely accessible to the public.",
                            className="mb-1",
                        ),
                        html.P(
                            [
                                "For questions, please contact us at ",
                                html.A(
                                    "yuxuan.du@utsa.edu",
                                    href="mailto:yuxuan.du@utsa.edu",
                                    className="text-decoration-none",
                                ),
                                ".",
                            ],
                            className="mb-0",
                        ),
                    ],
                    className="text-center text-muted small py-3",
                    fluid=True,
                ),
                className="mt-4 border-top",
            ),
        ]
    )
