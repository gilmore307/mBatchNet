# ===============================
# File: _3_welcome.py
# ===============================
from dash import html
import dash_bootstrap_components as dbc
from _1_components import build_navbar


def _feature_card(icon, title, text):
    return dbc.Card(
        dbc.CardBody(
            [
                html.Div(
                    html.I(className=f"bi {icon}"),
                    className="d-inline-flex align-items-center justify-content-center rounded-circle mb-3",
                    style={
                        "width": "56px",
                        "height": "56px",
                        "backgroundColor": "rgba(31, 191, 168, 0.10)",
                        "color": "#1fbfa8",
                        "fontSize": "1.35rem",
                    },
                ),
                html.H5(title, className="fw-semibold mb-2", style={"color": "#223242"}),
                html.P(text, className="mb-0", style={"color": "#5f7285", "lineHeight": "1.7"}),
            ]
        ),
        className="h-100 border-0 rounded-4",
        style={
            "backgroundColor": "#ffffff",
            "boxShadow": "0 8px 20px rgba(31, 45, 61, 0.06)",
            "border": "1px solid #e8eef3",
        },
    )


def _step_card(step, title, text, icon):
    return dbc.Card(
        dbc.CardBody(
            [
                html.Div(
                    [
                        html.Span(
                            step,
                            className="fw-bold",
                            style={
                                "fontSize": "0.9rem",
                                "letterSpacing": "0.04em",
                                "color": "#1fbfa8",
                            },
                        ),
                        html.I(
                            className=f"bi {icon}",
                            style={
                                "fontSize": "1.1rem",
                                "color": "#1fbfa8",
                            },
                        ),
                    ],
                    className="d-flex align-items-center justify-content-between mb-3",
                ),
                html.H5(title, className="fw-semibold mb-2", style={"color": "#223242"}),
                html.P(text, className="mb-0", style={"color": "#5f7285", "lineHeight": "1.7"}),
            ]
        ),
        className="h-100 border-0 rounded-4",
        style={
            "backgroundColor": "#ffffff",
            "boxShadow": "0 8px 20px rgba(31, 45, 61, 0.06)",
            "border": "1px solid #e8eef3",
        },
    )


def _resource_card(icon, title, text, button):
    return dbc.Card(
        dbc.CardBody(
            [
                html.Div(
                    html.I(className=f"bi {icon}"),
                    className="d-inline-flex align-items-center justify-content-center rounded-circle mb-3",
                    style={
                        "width": "52px",
                        "height": "52px",
                        "backgroundColor": "rgba(31, 191, 168, 0.10)",
                        "color": "#1fbfa8",
                        "fontSize": "1.2rem",
                    },
                ),
                html.H5(title, className="fw-semibold mb-2", style={"color": "#223242"}),
                html.P(text, className="mb-4", style={"color": "#5f7285", "lineHeight": "1.7"}),
                button,
            ],
            className="d-flex flex-column h-100",
        ),
        className="h-100 border-0 rounded-4",
        style={
            "backgroundColor": "#ffffff",
            "boxShadow": "0 8px 20px rgba(31, 45, 61, 0.06)",
            "border": "1px solid #e8eef3",
        },
    )


def welcome_layout(active_path: str):
    return html.Div(
        [
            build_navbar(active_path),
            html.Div(
                [
                    # =====================================
                    # HERO SECTION
                    # =====================================
                    dbc.Container(
                        [
                            dbc.Row(
                                [
                                    dbc.Col(
                                        [
                                            dbc.Badge(
                                                "Microbiome batch-effect analysis",
                                                className="px-3 py-2 mb-3 rounded-pill fw-semibold",
                                            ),
                                            html.H1(
                                                "mBatchNet",
                                                className="fw-bold text-white mb-3",
                                                style={
                                                    "fontSize": "4rem",
                                                    "letterSpacing": "-0.03em",
                                                },
                                            ),
                                            html.P(
                                                "Assess, correct, and compare batch effects across microbiome datasets "
                                                "in a streamlined browser-based workspace.",
                                                className="mb-3",
                                                style={
                                                    "fontSize": "1.35rem",
                                                    "color": "rgba(255,255,255,0.88)",
                                                    "maxWidth": "840px",
                                                    "lineHeight": "1.6",
                                                },
                                            ),
                                            html.P(
                                                "Upload your data, inspect pre-correction structure, run batch-effect "
                                                "correction workflows, and evaluate results before downstream analysis.",
                                                className="mb-4",
                                                style={
                                                    "fontSize": "1.03rem",
                                                    "color": "rgba(255,255,255,0.68)",
                                                    "maxWidth": "840px",
                                                    "lineHeight": "1.9",
                                                },
                                            ),
                                            html.Div(
                                                [
                                                    dbc.Button(
                                                        [html.I(className="bi bi-upload me-2"), "Start with your data"],
                                                        href="/upload",
                                                        size="lg",
                                                        className="me-3 mb-2 rounded-pill px-4 fw-semibold border-0",
                                                        style={
                                                            "backgroundColor": "#1fbfa8",
                                                            "color": "#ffffff",
                                                            "minWidth": "220px",
                                                        },
                                                    ),
                                                    dbc.Button(
                                                        [html.I(className="bi bi-collection-play me-2"), "Try sample workflow"],
                                                        href="/upload?tab=example",
                                                        size="lg",
                                                        className="mb-2 rounded-pill px-4 fw-semibold",
                                                        style={
                                                            "backgroundColor": "transparent",
                                                            "color": "#dffaf5",
                                                            "border": "1px solid rgba(223, 250, 245, 0.35)",
                                                            "minWidth": "220px",
                                                        },
                                                    ),
                                                ],
                                                className="d-flex flex-wrap",
                                            ),
                                        ],
                                        lg=9,
                                        className="py-4 py-lg-5",
                                    ),
                                ],
                                className="align-items-center justify-content-center",
                            ),
                        ],
                        fluid=True,
                        className="rounded-4 px-4 px-lg-5 py-4 mb-5",
                        style={
                            "background": "linear-gradient(135deg, #31475e 0%, #2b3f52 55%, #243545 100%)",
                            "boxShadow": "0 14px 34px rgba(24, 35, 48, 0.20)",
                            "border": "1px solid rgba(255,255,255,0.05)",
                        },
                    ),

                    # =====================================
                    # MAIN CONTENT
                    # =====================================
                    dbc.Container(
                        [
                            # Core capabilities
                            dbc.Row(
                                dbc.Col(
                                    html.Div(
                                        [
                                            dbc.Badge(
                                                "Core capabilities",
                                                className="mb-2 px-3 py-2 rounded-pill",
                                            ),
                                            html.H2(
                                                "Everything needed for practical batch-effect evaluation",
                                                className="fw-bold mb-2",
                                                style={"color": "#223242"},
                                            ),
                                            html.P(
                                                "Designed for microbiome workflows that require clear diagnostics, "
                                                "usable defaults, and exportable outputs.",
                                                className="mb-0",
                                                style={"color": "#617487"},
                                            ),
                                        ],
                                        className="text-center mb-4",
                                    ),
                                    width=12,
                                )
                            ),
                            dbc.Row(
                                [
                                    dbc.Col(
                                        _feature_card(
                                            "bi-cloud-arrow-up",
                                            "Flexible data intake",
                                            "Upload microbiome feature matrices and metadata in common tabular formats, or use the example dataset to explore the interface before running your own analysis.",
                                        ),
                                        md=6,
                                        lg=4,
                                        className="mb-4",
                                    ),
                                    dbc.Col(
                                        _feature_card(
                                            "bi-bar-chart-line",
                                            "Diagnostic visualisation",
                                            "Review pre-correction QC summaries, embeddings, and batch-associated structure so technical variation can be identified before correction is applied.",
                                        ),
                                        md=6,
                                        lg=4,
                                        className="mb-4",
                                    ),
                                    dbc.Col(
                                        _feature_card(
                                            "bi-sliders",
                                            "Correction and comparison",
                                            "Run supported correction workflows with curated defaults, compare before/after outputs, and download processed results for downstream analyses.",
                                        ),
                                        md=6,
                                        lg=4,
                                        className="mb-4",
                                    ),
                                ],
                                className="mb-4",
                            ),

                            # Workflow
                            dbc.Row(
                                dbc.Col(
                                    html.Div(
                                        [
                                            dbc.Badge(
                                                "Workflow",
                                                className="mb-2 px-3 py-2 rounded-pill",
                                            ),
                                            html.H2(
                                                "From upload to post-correction interpretation",
                                                className="fw-bold mb-2",
                                                style={"color": "#223242"},
                                            ),
                                            html.P(
                                                "A clear end-to-end sequence for reviewing, correcting, and assessing batch effects.",
                                                className="mb-0",
                                                style={"color": "#617487"},
                                            ),
                                        ],
                                        className="text-center mb-4",
                                    ),
                                    width=12,
                                )
                            ),
                            dbc.Row(
                                [
                                    dbc.Col(
                                        _step_card(
                                            "01",
                                            "Upload",
                                            "Bring microbiome feature matrices and metadata into the workspace and map the necessary metadata fields.",
                                            "bi-upload",
                                        ),
                                        md=6,
                                        lg=3,
                                        className="mb-4",
                                    ),
                                    dbc.Col(
                                        _step_card(
                                            "02",
                                            "Pre-correction assessment",
                                            "Inspect QC plots, PCA or UMAP structure, and batch labels to determine whether unwanted technical variation is present.",
                                            "bi-search",
                                        ),
                                        md=6,
                                        lg=3,
                                        className="mb-4",
                                    ),
                                    dbc.Col(
                                        _step_card(
                                            "03",
                                            "Correction",
                                            "Apply supported batch-effect correction methods using sensible defaults or adjust parameters for your study design.",
                                            "bi-magic",
                                        ),
                                        md=6,
                                        lg=3,
                                        className="mb-4",
                                    ),
                                    dbc.Col(
                                        _step_card(
                                            "04",
                                            "Post-correction assessment",
                                            "Compare diagnostics before and after correction, export plots, and download processed matrices for subsequent analysis.",
                                            "bi-check2-square",
                                        ),
                                        md=6,
                                        lg=3,
                                        className="mb-4",
                                    ),
                                ],
                                className="mb-4",
                            ),

                            # Resources
                            dbc.Row(
                                dbc.Col(
                                    html.Div(
                                        [
                                            dbc.Badge(
                                                "Resources",
                                                className="mb-2 px-3 py-2 rounded-pill",
                                            ),
                                            html.H2(
                                                "Helpful references and companion materials",
                                                className="fw-bold mb-2",
                                                style={"color": "#223242"},
                                            ),
                                            html.P(
                                                "Documentation and source materials are available directly from the homepage.",
                                                className="mb-0",
                                                style={"color": "#617487"},
                                            ),
                                        ],
                                        className="text-center mb-4",
                                    ),
                                    width=12,
                                )
                            ),
                            dbc.Row(
                                [
                                    dbc.Col(
                                        _resource_card(
                                            "bi-question-circle",
                                            "Help and tutorials",
                                            "Built-in guidance, inline parameter descriptions, and example-based help pages support users through each analysis stage.",
                                            dbc.Button(
                                                [html.I(className="bi bi-life-preserver me-2"), "Open help page"],
                                                id={"type": "help-open-trigger", "source": "resources"},
                                                className="rounded-pill px-4 fw-semibold border-0 mt-auto",
                                                style={
                                                    "backgroundColor": "#1fbfa8",
                                                    "color": "#ffffff",
                                                    "minWidth": "190px",
                                                },
                                            ),
                                        ),
                                        md=6,
                                        className="mb-4",
                                    ),
                                    dbc.Col(
                                        _resource_card(
                                            "bi-github",
                                            "Template script on GitHub",
                                            "Explore the companion template script to understand the underlying workflow, reproduce analyses locally, and review linked method references.",
                                            dbc.Button(
                                                [html.I(className="bi bi-github me-2"), "View on GitHub"],
                                                href="https://github.com/gilmore307/Batch-Effect-Correction",
                                                target="_blank",
                                                className="rounded-pill px-4 fw-semibold border-0 mt-auto",
                                                style={
                                                    "backgroundColor": "#2f4358",
                                                    "color": "#ffffff",
                                                    "minWidth": "190px",
                                                },
                                            ),
                                        ),
                                        md=6,
                                        className="mb-4",
                                    ),
                                ],
                                className="mb-4",
                            ),
                        ],
                        fluid=True,
                        className="pb-4 px-3 px-lg-4",
                    ),

                    # =====================================
                    # FOOTER
                    # =====================================
                    html.Footer(
                        dbc.Container(
                            [
                                html.H5("mBatchNet", className="fw-bold mb-2", style={"color": "#223242"}),
                                html.P(
                                    "An open-source platform for assessing, correcting, and interpreting batch effects in microbiome studies.",
                                    className="mb-2",
                                    style={"color": "#617487"},
                                ),
                                html.P(
                                    "All website resources and tools are free to use for both commercial and non-commercial purposes.",
                                    className="mb-2",
                                    style={"color": "#617487"},
                                ),
                                html.P(
                                    [
                                        "For questions, please contact ",
                                        html.A(
                                            "yuxuan.du@utsa.edu",
                                            href="mailto:yuxuan.du@utsa.edu",
                                            className="text-decoration-none fw-semibold",
                                            style={"color": "#1b8f8d"},
                                        ),
                                        ".",
                                    ],
                                    className="mb-0",
                                    style={"color": "#617487"},
                                ),
                            ],
                            className="text-center py-4",
                            fluid=True,
                        ),
                        className="mt-4 border-top",
                        style={"backgroundColor": "#ffffff", "borderColor": "#e8eef3"},
                    ),
                ],
                style={
                    "backgroundColor": "#f4f6f8",
                    "minHeight": "100vh",
                },
            ),
        ]
    )