# ===============================
# File: _5_assessment.py
# ===============================
from dash import html, dcc
from dash.dependencies import Input, Output, State
import dash
import dash_bootstrap_components as dbc
from typing import List

from _7_description import (
    ASSESSMENT_PARAM_TOOLTIPS,
    FIGURE_DIMENSION_OVERRIDE_TOOLTIP,
    FIGURE_DPI_TOOLTIP,
    FIGURE_SUBPLOTS_TOOLTIP,
)

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


FIGURE_DEFAULTS = {
    "pca": {"width": 2850, "height": 1800, "dpi": 300, "ncol": 2},
    "pcoa": {"width": 2850, "height": 1800, "dpi": 300, "ncol": 2},
    "nmds": {"width": 2850, "height": 1800, "dpi": 300, "ncol": 2},
    "dissimilarity": {"width": 2550, "height": 1800, "dpi": 300, "ncol": 2},
    "r2": {"width": 3000, "height": 1560, "dpi": 300},
    "prda": {"width": 2280, "height": 2070, "dpi": 300},
    "pvca": {"width": 2160, "height": 2040, "dpi": 300},
    "alignment": {"width": 1950, "height": 1380, "dpi": 300},
    "auc": {"width": 2640, "height": 1860, "dpi": 300},
    "lisi": {"width": 2700, "height": 2400, "dpi": 300},
    "ebm": {"width": 2550, "height": 1560, "dpi": 300},
    "silhouette": {"width": 2550, "height": 1560, "dpi": 300},
}


def _param_controls(stage: str, key: str):
    """Return a list of parameter input components (with tooltips) for a group.
    IDs follow pattern: f"{stage}-{key}-param-<name>".
    """
    def num_input(
        pid,
        label,
        value,
        step=None,
        min_=None,
        max_=None,
        tooltip="",
        placeholder=None,
    ):
        comp = dbc.Col([
            dbc.Label(label, html_for=pid, className="mb-1 d-block"),
            dbc.Input(
                id=pid,
                type="number",
                value=value,
                step=step,
                min=min_,
                max=max_,
                size="sm",
                className="w-100 be-param-input",
                placeholder=placeholder,
            ),
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
    tooltips = ASSESSMENT_PARAM_TOOLTIPS.get(key, {})

    if key == "auc":
        # AUC.R parameters
        controls = [
            num_input(
                f"{sid}-param-cv-folds",
                "CV_FOLDS",
                5,
                step=1,
                min_=2,
                tooltip=tooltips.get("cv_folds", ""),
            ),
            num_input(
                f"{sid}-param-cv-reps",
                "CV_REPS",
                5,
                step=1,
                min_=1,
                tooltip=tooltips.get("cv_reps", ""),
            ),
        ]
    elif key == "alignment":
        controls = [
            num_input(
                f"{sid}-param-k",
                "K_NEIGHBORS",
                10,
                step=1,
                min_=1,
                tooltip=tooltips.get("k_neighbors", ""),
            ),
            num_input(
                f"{sid}-param-var-prop",
                "VAR_PROP_MIN",
                0.95,
                step=0.01,
                min_=0.1,
                max_=1.0,
                tooltip=tooltips.get("var_prop_min", ""),
            ),
            num_input(
                f"{sid}-param-max-pcs",
                "MAX_PCS",
                10,
                step=1,
                min_=2,
                tooltip=tooltips.get("max_pcs", ""),
            ),
        ]
    elif key == "ebm":
        controls = [
            num_input(
                f"{sid}-param-umap-nn",
                "UMAP_NEIGHB",
                15,
                step=1,
                min_=2,
                tooltip=tooltips.get("umap_neighbors", ""),
            ),
            num_input(
                f"{sid}-param-umap-min-dist",
                "UMAP_MIN_DIST",
                0.3,
                step=0.05,
                min_=0.0,
                max_=1.0,
                tooltip=tooltips.get("umap_min_dist", ""),
            ),
            dropdown(
                f"{sid}-param-umap-metric",
                "UMAP_METRIC",
                ["euclidean", "cosine"],
                "euclidean",
                tooltip=tooltips.get("umap_metric", ""),
            ),
            num_input(
                f"{sid}-param-knn-k",
                "KNN_K",
                50,
                step=1,
                min_=1,
                tooltip=tooltips.get("knn_k", ""),
            ),
            num_input(
                f"{sid}-param-knn-pools",
                "KNN_POOLS",
                50,
                step=1,
                min_=1,
                tooltip=tooltips.get("knn_pools", ""),
            ),
            num_input(
                f"{sid}-param-knn-per-label",
                "KNN_PER_LABEL",
                100,
                step=1,
                min_=1,
                tooltip=tooltips.get("knn_per_label", ""),
            ),
        ]
    elif key == "lisi":
        controls = [
            num_input(
                f"{sid}-param-k",
                "k (neighbors)",
                30,
                step=1,
                min_=1,
                tooltip=tooltips.get("k_neighbors", ""),
            ),
            num_input(
                f"{sid}-param-npcs",
                "nPCs (PCA)",
                50,
                step=1,
                min_=2,
                tooltip=tooltips.get("n_pcs", ""),
            ),
            dropdown(
                f"{sid}-param-coords",
                "coords",
                ["pca", "clr"],
                "pca",
                tooltip=tooltips.get("coords", ""),
            ),
        ]
    elif key == "silhouette":
        controls = [
            num_input(
                f"{sid}-param-umap-nn",
                "UMAP_NEIGHB",
                15,
                step=1,
                min_=2,
                tooltip=tooltips.get("umap_neighbors", ""),
            ),
            num_input(
                f"{sid}-param-umap-min-dist",
                "UMAP_MIN_DIST",
                0.3,
                step=0.05,
                min_=0.0,
                max_=1.0,
                tooltip=tooltips.get("umap_min_dist", ""),
            ),
            dropdown(
                f"{sid}-param-umap-metric",
                "UMAP_METRIC",
                ["euclidean", "cosine"],
                "euclidean",
                tooltip=tooltips.get("umap_metric", ""),
            ),
        ]
    figure_controls: list = []
    include_fig_controls = stage in {"pre", "post"}
    if include_fig_controls:
        defaults = FIGURE_DEFAULTS.get(key, {})
        figure_controls.extend([
            num_input(
                f"{sid}-param-fig-width",
                "Figure width (px)",
                defaults.get("width"),
                step=50,
                min_=100,
                tooltip=FIGURE_DIMENSION_OVERRIDE_TOOLTIP,
            ),
            num_input(
                f"{sid}-param-fig-height",
                "Figure height (px)",
                defaults.get("height"),
                step=50,
                min_=100,
                tooltip=FIGURE_DIMENSION_OVERRIDE_TOOLTIP,
            ),
            num_input(
                f"{sid}-param-fig-dpi",
                "Figure DPI",
                defaults.get("dpi", 300),
                step=10,
                min_=50,
                tooltip=FIGURE_DPI_TOOLTIP,
            ),
        ])
        if stage == "post" and key in {"pca", "pcoa", "nmds", "dissimilarity"}:
            figure_controls.append(
                num_input(
                    f"{sid}-param-fig-ncol",
                    "Subplots per row",
                    defaults.get("ncol", 2),
                    step=1,
                    min_=1,
                    tooltip=FIGURE_SUBPLOTS_TOOLTIP,
                )
            )

    if not controls and not figure_controls:
        return []

    return [
        dbc.Row(
            controls + figure_controls,
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
        ("r2", "Feature-wise ANOVA R²", "R2.R"),
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
                        dcc.Store(
                            id=f"{stage}-{key}-param-store",
                            storage_type="session",
                        ),
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
                                    "width": "83vw",
                                    "minWidth": "83vw",
                                    "maxWidth": "83vw",
                                    "marginLeft": "0",
                                },
                            ),
                        ], type="default"),
                    ],
                    # Ensure the tab pane provides full width so Bootstrap grid works
                    className="w-100",
                    style={
                        "width": "83vw",
                        "minWidth": "83vw",
                        "maxWidth": "83vw",
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
        ("r2", "Feature-wise ANOVA R²", "R2.R"),
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
        sid = f"{stage}-{key}"
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
        outputs.append(Output(f"{sid}-param-store", "data", allow_duplicate=True))

        # Parameter States by group
        states: list = [State("session-id", "data")]
        param_state_ids: List[str] = []
        if key == "auc":
            param_state_ids.extend([
                f"{sid}-param-cv-folds",
                f"{sid}-param-cv-reps",
            ])
            states += [
                State(f"{sid}-param-cv-folds", "value"),
                State(f"{sid}-param-cv-reps", "value"),
            ]
        elif key == "alignment":
            param_state_ids.extend([
                f"{sid}-param-k",
                f"{sid}-param-var-prop",
                f"{sid}-param-max-pcs",
            ])
            states += [
                State(f"{sid}-param-k", "value"),
                State(f"{sid}-param-var-prop", "value"),
                State(f"{sid}-param-max-pcs", "value"),
            ]
        elif key == "ebm":
            param_state_ids.extend([
                f"{sid}-param-umap-nn",
                f"{sid}-param-umap-min-dist",
                f"{sid}-param-umap-metric",
                f"{sid}-param-knn-k",
                f"{sid}-param-knn-pools",
                f"{sid}-param-knn-per-label",
            ])
            states += [
                State(f"{sid}-param-umap-nn", "value"),
                State(f"{sid}-param-umap-min-dist", "value"),
                State(f"{sid}-param-umap-metric", "value"),
                State(f"{sid}-param-knn-k", "value"),
                State(f"{sid}-param-knn-pools", "value"),
                State(f"{sid}-param-knn-per-label", "value"),
            ]
        elif key == "lisi":
            param_state_ids.extend([
                f"{sid}-param-k",
                f"{sid}-param-npcs",
                f"{sid}-param-coords",
            ])
            states += [
                State(f"{sid}-param-k", "value"),
                State(f"{sid}-param-npcs", "value"),
                State(f"{sid}-param-coords", "value"),
            ]
        elif key == "silhouette":
            param_state_ids.extend([
                f"{sid}-param-umap-nn",
                f"{sid}-param-umap-min-dist",
                f"{sid}-param-umap-metric",
            ])
            states += [
                State(f"{sid}-param-umap-nn", "value"),
                State(f"{sid}-param-umap-min-dist", "value"),
                State(f"{sid}-param-umap-metric", "value"),
            ]

        has_ncol_param = False
        if stage in {"pre", "post"}:
            param_state_ids.extend([
                f"{sid}-param-fig-width",
                f"{sid}-param-fig-height",
                f"{sid}-param-fig-dpi",
            ])
            states += [
                State(f"{sid}-param-fig-width", "value"),
                State(f"{sid}-param-fig-height", "value"),
                State(f"{sid}-param-fig-dpi", "value"),
            ]
            if stage == "post" and key in {"pca", "pcoa", "nmds", "dissimilarity"}:
                param_state_ids.append(f"{sid}-param-fig-ncol")
                states.append(State(f"{sid}-param-fig-ncol", "value"))
                has_ncol_param = True

        state_ids_tuple = tuple(param_state_ids)

        @app.callback(
            *outputs,
            Input(run_id, "n_clicks"),
            *states,
            prevent_initial_call=True,
        )
        def _run_one(
            n_clicks: int,
            *values,
            _stage=stage,
            _key=key,
            _script=script_name,
            _has_ncol=has_ncol_param,
            _state_ids=state_ids_tuple,
        ):
            if not n_clicks:
                raise dash.exceptions.PreventUpdate
            # unpack session id
            if not values:
                raise dash.exceptions.PreventUpdate
            session_id = values[0]
            param_vals = list(values[1:])
            persisted_payload = dash.no_update
            if _state_ids:
                persisted_payload = {"values": {pid: val for pid, val in zip(_state_ids, param_vals)}}
            if not session_id:
                if _stage == "pre":
                    return (
                        html.Div("Session not initialised."),
                        True,
                        dash.no_update,
                        dash.no_update,
                        dash.no_update,
                        dash.no_update,
                        dash.no_update,
                    )
                else:
                    return (
                        html.Div("Session not initialised."),
                        True,
                        dash.no_update,
                        dash.no_update,
                        dash.no_update,
                        dash.no_update,
                        dash.no_update,
                        dash.no_update,
                    )
            session_dir = get_session_dir(session_id)
            if not (session_dir / "raw.csv").exists() or not (session_dir / "metadata.csv").exists():
                if _stage == "pre":
                    return (
                        html.Div("Upload both raw.csv and metadata.csv first."),
                        True,
                        dash.no_update,
                        dash.no_update,
                        dash.no_update,
                        dash.no_update,
                        dash.no_update,
                    )
                else:
                    return (
                        html.Div("Upload both raw.csv and metadata.csv first."),
                        True,
                        dash.no_update,
                        dash.no_update,
                        dash.no_update,
                        dash.no_update,
                        dash.no_update,
                        dash.no_update,
                    )

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

            pv = param_vals
            idx = 0
            if _key == "auc":
                # order: cv-folds, cv-reps
                _add("cv_folds", pv[idx], int)
                idx += 1
                _add("cv_reps", pv[idx], int)
                idx += 1
            elif _key == "alignment":
                _add("k", pv[idx], int)
                idx += 1
                _add("var_prop_min", pv[idx], float)
                idx += 1
                _add("max_pcs", pv[idx], int)
                idx += 1
            elif _key == "ebm":
                _add("umap_neighbors", pv[idx], int)
                idx += 1
                _add("umap_min_dist", pv[idx], float)
                idx += 1
                _add("umap_metric", pv[idx], str)
                idx += 1
                _add("knn_k", pv[idx], int)
                idx += 1
                _add("knn_pools", pv[idx], int)
                idx += 1
                _add("knn_per_label", pv[idx], int)
                idx += 1
            elif _key == "lisi":
                _add("k", pv[idx], int)
                idx += 1
                _add("npcs", pv[idx], int)
                idx += 1
                _add("coords", pv[idx], str)
                idx += 1
            elif _key == "silhouette":
                _add("umap_neighbors", pv[idx], int)
                idx += 1
                _add("umap_min_dist", pv[idx], float)
                idx += 1
                _add("umap_metric", pv[idx], str)
                idx += 1

            if _stage in {"pre", "post"}:
                fig_width = pv[idx] if idx < len(pv) else None
                idx += 1
                fig_height = pv[idx] if idx < len(pv) else None
                idx += 1
                fig_dpi = pv[idx] if idx < len(pv) else None
                idx += 1
                fig_ncol = pv[idx] if _has_ncol and idx < len(pv) else None
                if _has_ncol:
                    idx += 1
                _add("fig-width-px", fig_width, int)
                _add("fig-height-px", fig_height, int)
                _add("fig-dpi", fig_dpi, int)
                if _has_ncol and fig_ncol is not None:
                    _add("fig-ncol", fig_ncol, int)
                if _stage == "post" and _has_ncol:
                    flags.append("--fig-per-panel=true")

            success, _ = run_r_scripts((_script,), session_dir, log_path=log_path, extra_args=flags)
            content = render_group_tabset(session_dir, _stage, _key)
            if _stage == "pre":
                # pre: no Overall tab to update
                return (
                    content,
                    True,
                    str(log_path),
                    None,
                    dash.no_update,
                    dash.no_update,
                    persisted_payload,
                )
            else:
                # post: update Overall
                overall = build_overall_div(session_dir, _stage)
                return (
                    content,
                    True,
                    str(log_path),
                    None,
                    dash.no_update,
                    dash.no_update,
                    overall,
                    persisted_payload,
                )

        if state_ids_tuple:

            @app.callback(
                *(Output(pid, "value") for pid in state_ids_tuple),
                Input(f"{sid}-param-store", "data"),
            )
            def _restore_params(store_data, _ids=state_ids_tuple):
                if not store_data:
                    return [dash.no_update] * len(_ids)
                values_map = store_data.get("values", {}) if isinstance(store_data, dict) else {}
                restored = []
                for pid in _ids:
                    if pid in values_map:
                        restored.append(values_map[pid])
                    else:
                        restored.append(dash.no_update)
                return restored

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

