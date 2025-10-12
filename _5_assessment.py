# ===============================
# File: _5_assessment.py
# ===============================
from dash import html, dcc
from dash.dependencies import Input, Output, State
import dash
import dash_bootstrap_components as dbc

from _1_components import build_navbar
from _2_utils import (
    get_session_dir,
    run_r_scripts,
    PRE_SCRIPTS,
    POST_SCRIPTS,
    PRE_FIGURES,
    POST_FIGURES,
    render_assessment_tabs,
    render_group_tabset,
    build_group_subtab_content,
    build_ranking_tab,
    build_raw_assessments_tab,
    build_overall_div,
)

MULTI_PANEL_KEYS = {"pca", "pcoa", "nmds", "dissimilarity"}


def _param_controls(stage: str, key: str):
    """Return a list of parameter input components (with tooltips) for a group.
    IDs follow pattern: f"{stage}-{key}-param-<name>".
    """
    def num_input(pid, label, value, step=None, min_=None, max_=None, tooltip=""):
        comp = dbc.Col([
            dbc.Label(label, html_for=pid, className="mb-1 d-block"),
            dbc.Input(id=pid, type="number", value=value, step=step, min=min_, max=max_, size="sm", className="w-100 be-param-input"),
            dbc.Tooltip(tooltip, target=pid, placement="right")
        ], xs=12, sm=6, md=3, lg=2, className="mb-2 be-param-col")
        return comp

    def text_input(pid, label, value, tooltip=""):
        comp = dbc.Col([
            dbc.Label(label, html_for=pid, className="mb-1 d-block"),
            dbc.Input(id=pid, type="text", value=value, size="sm", className="w-100 be-param-input"),
            dbc.Tooltip(tooltip, target=pid, placement="right")
        ], xs=12, sm=12, md=6, lg=4, className="mb-2 be-param-col")
        return comp

    def dropdown(pid, label, options, value, tooltip=""):
        comp = dbc.Col([
            dbc.Label(label, html_for=pid, className="mb-1 d-block"),
            dcc.Dropdown(id=pid, options=[{"label": o, "value": o} for o in options], value=value, clearable=False, style={"width": "100%"}),
            dbc.Tooltip(tooltip, target=pid, placement="right")
        ], xs=12, sm=6, md=3, lg=2, className="mb-2 be-param-col")
        return comp

    def checklist(pid, label, options, value, tooltip=""):
        comp = dbc.Col([
            dbc.Label(label, className="mb-1 d-block"),
            dbc.Checklist(id=pid, options=options, value=value, inline=True, switch=True),
            dbc.Tooltip(tooltip, target=pid, placement="right")
        ], xs=12, sm=12, md=6, lg=4, className="mb-2 be-param-col")
        return comp

    controls = []
    sid = f"{stage}-{key}"

    if key == "auc":
        # AUC.R parameters
        controls = [
            num_input(f"{sid}-param-cv-folds", "CV_FOLDS", 5, step=1, min_=2,
                      tooltip="Number of cross-validation folds (repeated CV)."),
            num_input(f"{sid}-param-cv-reps", "CV_REPS", 5, step=1, min_=1,
                      tooltip="Number of repetitions for repeated cross-validation."),
        ]
    elif key == "alignment":
        controls = [
            num_input(f"{sid}-param-k", "K_NEIGHBORS", 10, step=1, min_=1,
                      tooltip="k for k-NN graph in PCA space."),
            num_input(f"{sid}-param-var-prop", "VAR_PROP_MIN", 0.95, step=0.01, min_=0.1, max_=1.0,
                      tooltip="Min cumulative variance for PCA retention (0–1)."),
            num_input(f"{sid}-param-max-pcs", "MAX_PCS", 10, step=1, min_=2,
                      tooltip="Maximum number of principal components to use."),
        ]
    elif key == "ebm":
        controls = [
            num_input(f"{sid}-param-umap-nn", "UMAP_NEIGHB", 15, step=1, min_=2,
                      tooltip="UMAP: Number of neighbors (local connectivity)."),
            num_input(f"{sid}-param-umap-min-dist", "UMAP_MIN_DIST", 0.3, step=0.05, min_=0.0, max_=1.0,
                      tooltip="UMAP: Minimum distance between points in embedding."),
            dropdown(f"{sid}-param-umap-metric", "UMAP_METRIC", ["euclidean", "cosine"], "euclidean",
                     tooltip="UMAP distance metric (CLR uses Euclidean)."),
            num_input(f"{sid}-param-knn-k", "KNN_K", 50, step=1, min_=1,
                      tooltip="k for entropy mixing (neighbors per anchor)."),
            num_input(f"{sid}-param-knn-pools", "KNN_POOLS", 50, step=1, min_=1,
                      tooltip="Number of anchor pools to average entropy over."),
            num_input(f"{sid}-param-knn-per-label", "KNN_PER_LABEL", 100, step=1, min_=1,
                      tooltip="Anchors sampled per batch label per pool."),
        ]
    elif key == "lisi":
        controls = [
            num_input(f"{sid}-param-k", "k (neighbors)", 30, step=1, min_=1,
                      tooltip="k for k-NN graph when computing LISI."),
            num_input(f"{sid}-param-npcs", "nPCs (PCA)", 50, step=1, min_=2,
                      tooltip="Number of PCs for LISI (set via coords=CLR to use none)."),
            dropdown(f"{sid}-param-coords", "coords", ["pca", "clr"], "pca",
                     tooltip="Coordinate space for LISI: PCA or CLR."),
        ]
    elif key == "silhouette":
        controls = [
            num_input(f"{sid}-param-umap-nn", "UMAP_NEIGHB", 15, step=1, min_=2,
                      tooltip="UMAP: Number of neighbors (local connectivity)."),
            num_input(f"{sid}-param-umap-min-dist", "UMAP_MIN_DIST", 0.3, step=0.05, min_=0.0, max_=1.0,
                      tooltip="UMAP: Minimum distance between points in embedding."),
            dropdown(f"{sid}-param-umap-metric", "UMAP_METRIC", ["euclidean", "cosine"], "euclidean",
                     tooltip="UMAP distance metric (CLR uses Euclidean)."),
        ]

    controls += [
        num_input(
            f"{sid}-param-width-px",
            "Width (px)",
            None,
            step=100,
            min_=1,
            tooltip="Optional: override export width in pixels (converted with dpi=300).",
        ),
        num_input(
            f"{sid}-param-height-px",
            "Height (px)",
            None,
            step=100,
            min_=1,
            tooltip="Optional: override export height in pixels (converted with dpi=300).",
        ),
    ]

    if key in MULTI_PANEL_KEYS:
        controls.append(
            num_input(
                f"{sid}-param-subplots-per-row",
                "Subplots / row",
                None,
                step=1,
                min_=1,
                tooltip="Optional: override panels per row. Leave blank for automatic layout.",
            )
        )

    if not controls:
        return []

    return [
        dbc.Row(
            controls,
            className="g-2 align-items-end",
            style={"marginLeft": "5px"},
        ),
        html.Hr(className="my-2", style={"marginLeft": "5px"}),
    ]


def assessment_layout(active_path: str, stage: str):
    # Unified tab styles (keep size and position consistent)
    TOP_TAB_STYLE = {
        "borderBottom": "1px solid #d6d6d6",
        "padding": "6px",
        "fontWeight": "bold",
        "width": "12vw",
        "minWidth": "12vw",
        "maxWidth": "12vw",
        "height": "60px",
        "minHeight": "60px",
        "maxHeight": "60px",
        "display": "flex",
        "alignItems": "center",
        "justifyContent": "center",
        "marginRight": "1vw",
    }
    TOP_TAB_SELECTED_STYLE = {
        "borderTop": "1px solid #d6d6d6",
        "borderBottom": "1px solid #d6d6d6",
        "backgroundColor": "#f8f9fa",
        "color": "#0d6efd",
        "padding": "6px",
        "width": "12vw",
        "minWidth": "12vw",
        "maxWidth": "12vw",
        "height": "60px",
        "minHeight": "60px",
        "maxHeight": "60px",
        "display": "flex",
        "alignItems": "center",
        "justifyContent": "center",
        "marginRight": "1vw",
    }
    header = "Pre-correction Assessment" if stage == "pre" else "Post-correction Assessment"
    gallery_id = "pre-assessment-gallery" if stage == "pre" else "post-assessment-gallery"

    # Define groups (key, title, script)
    pre_groups = [
        ("pca", "PCA", "pca.R"),
        ("pcoa", "PCoA", "pcoa.R"),
        ("nmds", "NMDS", "NMDS.R"),
        ("dissimilarity", "Dissimilarity heatmaps", "Dissimilarity_Heatmaps.R"),
        ("r2", "R^2", "R2.R"),
        ("prda", "pRDA", "pRDA.R"),
        ("pvca", "PVCA", "pvca.R"),
        ("alignment", "Alignment score", "Alignment_Score.R"),
        ("auc", "AUC", "AUC.R"),
    ]
    post_extra = [
        ("lisi", "LISI", "LISI.R"),
        ("ebm", "Entropy score", "Entropy_Score.R"),
        ("silhouette", "Silhouette score", "Silhouette.R"),
    ]
    # Extend post assessment with additional post-only metrics
    if stage == "post":
        groups = pre_groups + post_extra
    else:
        groups = pre_groups

    # Build preset tabs with a Run button per group and placeholder until run
    tab_items = []
    for key, title, _ in groups:
        run_id = f"run-{stage}-{key}"
        content_id = f"{stage}-{key}-content"
        placeholder = html.Div("Click Run to generate results.")
        controls = _param_controls(stage, key)
        tab_items.append(
            dcc.Tab(
                label=title,
                value=f"tab-{key}",
                style=TOP_TAB_STYLE,
                selected_style=TOP_TAB_SELECTED_STYLE,
                children=html.Div(
                    [
                        *(controls or []),
                        dcc.Loading([
                            dbc.Button(
                                "Run",
                                id=run_id,
                                size="sm",
                                color="success",
                                className="mb-2",
                                style={"width": "250px", "marginLeft": "5px"},
                            ),
                            html.Div(
                                id=content_id,
                                children=placeholder,
                                style={
                                    "width": "85vw",
                                    "minWidth": "85vw",
                                    "maxWidth": "85vw",
                                    "marginLeft": "0",
                                },
                            ),
                        ], type="default"),
                    ],
                    # Ensure the tab pane provides full width so Bootstrap grid works
                    className="w-100",
                    style={
                        "width": "85vw",
                        "minWidth": "85vw",
                        "maxWidth": "85vw",
                        "display": "block",
                        "marginLeft": "0",
                    },
                ),
            )
        )

    # Overall tab only for post stage
    if stage == "post":
        tab_items.append(
            dcc.Tab(
                label="Overall",
                value="tab-overall",
                style=TOP_TAB_STYLE,
                selected_style=TOP_TAB_SELECTED_STYLE,
                children=html.Div(id=f"{stage}-overall-content", children=html.Div("No results yet.")),
            )
        )

    tabs = dcc.Tabs(
        children=tab_items,
        value=(tab_items[0].value if tab_items else None),
        vertical=True,
        className="be-results-tabs be-stage-tabs",
    )

    return html.Div(
        [
            build_navbar(active_path),
            dbc.Container(
                [
                    html.H2(header),
                    html.Div(
                        "Click Run inside a tab to compute only that assessment.",
                        className="text-muted mb-3",
                    ),
                    tabs,
                ],
                fluid=True,
            ),
        ]
    )


def register_pre_post_callbacks(app):
    # Group definitions for per-tab runs
    pre_groups = [
        ("pca", "PCA", "pca.R"),
        ("pcoa", "PCoA", "pcoa.R"),
        ("nmds", "NMDS", "NMDS.R"),
        ("dissimilarity", "Dissimilarity heatmaps", "Dissimilarity_Heatmaps.R"),
        ("r2", "R^2", "R2.R"),
        ("prda", "pRDA", "pRDA.R"),
        ("pvca", "PVCA", "pvca.R"),
        ("alignment", "Alignment score", "Alignment_Score.R"),
        ("auc", "AUC", "AUC.R"),
    ]
    post_extra = [
        ("lisi", "LISI", "LISI.R"),
        ("ebm", "Entropy score", "Entropy_Score.R"),
        ("silhouette", "Silhouette score", "Silhouette.R"),
    ]

    def _register_group(stage: str, key: str, script_name: str):
        run_id = f"run-{stage}-{key}"
        content_id = f"{stage}-{key}-content"

        outputs = [Output(content_id, "children")]
        if stage == "pre":
            outputs.append(Output("pre-started", "data", allow_duplicate=True))
        if stage == "post":
            outputs.append(Output("post-complete", "data", allow_duplicate=True))
        # Common runlog outputs
        outputs.extend([
            Output("runlog-path", "data", allow_duplicate=True),
            Output("runlog-file-meta", "data", allow_duplicate=True),
            Output("runlog-modal", "is_open", allow_duplicate=True),
            Output("runlog-interval", "disabled", allow_duplicate=True),
        ])
        # Only post stage updates an Overall tab
        if stage == "post":
            outputs.append(Output(f"{stage}-overall-content", "children", allow_duplicate=True))

        # Parameter States by group
        sid = f"{stage}-{key}"
        states: list = [State("session-id", "data")]
        if key == "auc":
            states += [
                State(f"{sid}-param-cv-folds", "value"),
                State(f"{sid}-param-cv-reps", "value"),
            ]
        elif key == "alignment":
            states += [
                State(f"{sid}-param-k", "value"),
                State(f"{sid}-param-var-prop", "value"),
                State(f"{sid}-param-max-pcs", "value"),
            ]
        elif key == "ebm":
            states += [
                State(f"{sid}-param-umap-nn", "value"),
                State(f"{sid}-param-umap-min-dist", "value"),
                State(f"{sid}-param-umap-metric", "value"),
                State(f"{sid}-param-knn-k", "value"),
                State(f"{sid}-param-knn-pools", "value"),
                State(f"{sid}-param-knn-per-label", "value"),
            ]
        elif key == "lisi":
            states += [
                State(f"{sid}-param-k", "value"),
                State(f"{sid}-param-npcs", "value"),
                State(f"{sid}-param-coords", "value"),
            ]
        elif key == "silhouette":
            states += [
                State(f"{sid}-param-umap-nn", "value"),
                State(f"{sid}-param-umap-min-dist", "value"),
                State(f"{sid}-param-umap-metric", "value"),
            ]
        states += [
            State(f"{sid}-param-width-px", "value"),
            State(f"{sid}-param-height-px", "value"),
        ]
        if key in MULTI_PANEL_KEYS:
            states.append(State(f"{sid}-param-subplots-per-row", "value"))

        state_ids = [s.component_id for s in states]

        @app.callback(*outputs, Input(run_id, "n_clicks"), *states, prevent_initial_call=True)
        def _run_one(n_clicks: int, *values, _stage=stage, _key=key, _script=script_name, _state_ids=state_ids):
            if not n_clicks:
                raise dash.exceptions.PreventUpdate
            # unpack session id
            if not values:
                raise dash.exceptions.PreventUpdate
            session_id = values[0]
            param_map = {
                cid: val for cid, val in zip(_state_ids[1:], values[1:])
            }
            if not session_id:
                if _stage == "pre":
                    return html.Div("Session not initialised."), True, dash.no_update, dash.no_update, dash.no_update, dash.no_update
                else:
                    return html.Div("Session not initialised."), True, dash.no_update, dash.no_update, dash.no_update, dash.no_update, dash.no_update
            session_dir = get_session_dir(session_id)
            if not (session_dir / "raw.csv").exists() or not (session_dir / "metadata.csv").exists():
                if _stage == "pre":
                    return html.Div("Upload both raw.csv and metadata.csv first."), True, dash.no_update, dash.no_update, dash.no_update, dash.no_update
                else:
                    return html.Div("Upload both raw.csv and metadata.csv first."), True, dash.no_update, dash.no_update, dash.no_update, dash.no_update, dash.no_update

            # Append all analysis logs into a single session-wide log file
            log_path = session_dir / "run.log"
            # Build CLI flags from parameters
            flags = []
            def _add(flag, val, cast=str):
                if val is None or val == "":
                    return
                try:
                    sval = cast(val)
                except Exception:
                    sval = val
                flags.append(f"--{flag}={sval}")

            if _key == "auc":
                # order: cv-folds, cv-reps
                _add("cv_folds", param_map.get(f"{_stage}-{_key}-param-cv-folds"), int)
                _add("cv_reps", param_map.get(f"{_stage}-{_key}-param-cv-reps"), int)
            elif _key == "alignment":
                _add("k", param_map.get(f"{_stage}-{_key}-param-k"), int)
                _add("var_prop_min", param_map.get(f"{_stage}-{_key}-param-var-prop"), float)
                _add("max_pcs", param_map.get(f"{_stage}-{_key}-param-max-pcs"), int)
            elif _key == "ebm":
                _add("umap_neighbors", param_map.get(f"{_stage}-{_key}-param-umap-nn"), int)
                _add("umap_min_dist", param_map.get(f"{_stage}-{_key}-param-umap-min-dist"), float)
                _add("umap_metric", param_map.get(f"{_stage}-{_key}-param-umap-metric"), str)
                _add("knn_k", param_map.get(f"{_stage}-{_key}-param-knn-k"), int)
                _add("knn_pools", param_map.get(f"{_stage}-{_key}-param-knn-pools"), int)
                _add("knn_per_label", param_map.get(f"{_stage}-{_key}-param-knn-per-label"), int)
            elif _key == "lisi":
                _add("k", param_map.get(f"{_stage}-{_key}-param-k"), int)
                _add("npcs", param_map.get(f"{_stage}-{_key}-param-npcs"), int)
                _add("coords", param_map.get(f"{_stage}-{_key}-param-coords"), str)
            elif _key == "silhouette":
                _add("umap_neighbors", param_map.get(f"{_stage}-{_key}-param-umap-nn"), int)
                _add("umap_min_dist", param_map.get(f"{_stage}-{_key}-param-umap-min-dist"), float)
                _add("umap_metric", param_map.get(f"{_stage}-{_key}-param-umap-metric"), str)

            _add("width_px", param_map.get(f"{_stage}-{_key}-param-width-px"), int)
            _add("height_px", param_map.get(f"{_stage}-{_key}-param-height-px"), int)
            if _key in MULTI_PANEL_KEYS:
                _add(
                    "subplots_per_row",
                    param_map.get(f"{_stage}-{_key}-param-subplots-per-row"),
                    int,
                )

            success, _ = run_r_scripts((_script,), session_dir, log_path=log_path, extra_args=flags)
            content = render_group_tabset(session_dir, _stage, _key)
            if _stage == "pre":
                # pre: no Overall tab to update
                return content, True, str(log_path), None, dash.no_update, dash.no_update
            else:
                # post: update Overall
                overall = build_overall_div(session_dir, _stage)
                return content, True, str(log_path), None, dash.no_update, dash.no_update, overall

        # Update subtab content when user clicks a subtab (after results are rendered)
        @app.callback(
            Output(f"{sid}-subtab-content", "children"),
            Input(f"{sid}-subtabs", "value"),
            State("session-id", "data"),
            prevent_initial_call=True,
        )
        def _switch_subtab(selected_value, session_id, _stage=stage, _key=key):
            if not selected_value or not session_id:
                raise dash.exceptions.PreventUpdate
            session_dir = get_session_dir(session_id)
            return build_group_subtab_content(session_dir, _stage, _key, selected_value)

    # Register all group callbacks
    for key, _, script in pre_groups:
        _register_group("pre", key, script)
    # Register post-stage groups (baseline pre groups plus post-only extras)
    for key, _, script in pre_groups + post_extra:
        _register_group("post", key, script)

