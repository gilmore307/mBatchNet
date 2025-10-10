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
                    html.H2("Batch-Effect Explorer"),
                    html.P(
                        "Batch-Effect Explorer is a free, browser-based workspace for assessing, correcting, "
                        "and visualising technical variation across omics datasets. Upload your own count matrices and "
                        "metadata, explore quality control diagnostics, and launch correction workflows without installing "
                        "local software."
                    ),
                    html.P(
                        "We apply the NAR Web Server guidelines: the platform is accessible over HTTPS, provides sample "
                        "datasets, and keeps user uploads private to each session. Rich interactive output, including "
                        "plots and downloadable tables, is generated for every analysis step."
                    ),
                    html.P(
                        "To help us improve the service, you may optionally allow us to retain anonymised analysis results. "
                        "This choice is presented when you first open the site, and declining will not disable any features."
                    ),
                    html.Hr(),
                    html.H4("Highlights"),
                    html.Ul(
                        [
                            html.Li("One-click example data for quick demonstrations."),
                            html.Li("Interactive pre- and post-correction diagnostics with shareable links."),
                            html.Li("Support for automated correction workflows spanning multiple established methods."),
                        ]
                    ),
                ],
                fluid=True,
            ),
        ]
    )
