# ===============================
# File: _5_assessment.py
# ===============================
import threading
from pathlib import Path

from dash import html, dcc
from dash.dependencies import Input, Output, State
import dash
import dash_bootstrap_components as dbc
from typing import List, Optional, Sequence

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
)


FIGURE_DEFAULTS = {
    "alignment": {"width": 2800, "height": 1800, "dpi": 300},
    "pca": {"width": 2800, "height": 1800, "dpi": 300, "ncol": 3},
    "pcoa": {"width": 2800, "height": 1800, "dpi": 300, "ncol": 3},
    "nmds": {"width": 2800, "height": 1800, "dpi": 300, "ncol": 3},
    "dissimilarity": {"width": 2800, "height": 1800, "dpi": 300, "ncol": 3},
    "permanova": {"width": 2800, "height": 1800, "dpi": 300},
    "r2": {"width": 4800, "height": 1200, "dpi": 300},
    "prda": {"width": 3000, "height": 1500, "dpi": 300},
    "pvca": {"width": 3000, "height": 1500, "dpi": 300},
    "ebm": {"width": 2550, "height": 1560, "dpi": 300},
    "silhouette": {"width": 2800, "height": 1800, "dpi": 300},
}


def _expected_figure_files(stage: str, key: str) -> List[str]:
    """Return expected output filenames for a given assessment group."""

    figures: Sequence = PRE_FIGURES if stage == "pre" else POST_FIGURES
    key = key.lower()
    expected: List[Optional[str]] = []

    def add_if(condition: bool, filename: Optional[str]):
        if condition and filename:
            expected.append(filename)

    for spec in figures:
        low = spec.filename.lower()
        stem = Path(low).stem
        if key == "pcoa":
            add_if(low.startswith("pcoa_aitchison_batch"), spec.filename)
            add_if(low.startswith("pcoa_aitchison_target"), spec.filename)
            add_if(low.startswith("pcoa_braycurtis_batch"), spec.filename)
            add_if(low.startswith("pcoa_braycurtis_target"), spec.filename)
        elif key == "nmds":
            add_if(low.startswith("nmds_aitchison_batch"), spec.filename)
            add_if(low.startswith("nmds_aitchison_target"), spec.filename)
            add_if(low.startswith("nmds_braycurtis_batch"), spec.filename)
            add_if(low.startswith("nmds_braycurtis_target"), spec.filename)
        elif key == "dissimilarity":
            add_if(low.startswith("dissimilarity_heatmaps_aitchison"), spec.filename)
            add_if(low.startswith("dissimilarity_heatmaps_braycurtis"), spec.filename)
        elif key == "permanova":
            add_if(low.startswith("permanova_aitchison"), spec.filename)
            add_if(low.startswith("permanova_braycurtis"), spec.filename)
            add_if(low.startswith("permanova"), spec.filename)
        elif key == "r2":
            add_if(low.startswith("anova_aitchison"), spec.filename)
            add_if(low.startswith("anova_braycurtis"), spec.filename)
        elif key == "prda":
            add_if(low.startswith("prda_aitchison"), spec.filename)
            add_if(low.startswith("prda_braycurtis"), spec.filename)
        elif key == "alignment" and stem in {"alignment_score", "alignment"}:
            add_if(True, spec.filename)
        elif key == "pca":
            add_if(stem == "pca_batch", spec.filename)
            add_if(stem == "pca_target", spec.filename)
        elif key == "pvca" and stem == "pvca":
            add_if(True, spec.filename)
        elif key == "ebm" and stem == "ebm":
            add_if(True, spec.filename)
        elif key == "silhouette" and stem == "silhouette":
            add_if(True, spec.filename)

    # Preserve order but drop None/duplicates
    seen = set()
    unique: List[str] = []
    for fname in expected:
        if fname and fname not in seen:
            unique.append(fname)
            seen.add(fname)
    return unique


def _assessment_outputs_ready(session_dir: Path, expected_files: Sequence[str]) -> bool:
    if not expected_files:
        return False

    def _exists_with_fallback(name: str) -> bool:
        path = session_dir / name
        if path.exists():
            return True
        if path.suffix.lower() in {".tif", ".tiff"}:
            alt = path.with_suffix(".png")
            return alt.exists()
        return False

    return all(_exists_with_fallback(name) for name in expected_files)


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

    if key == "ebm":
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
    elif key == "alignment":
        controls = [
            num_input(
                f"{sid}-param-k",
                "k (neighbors)",
                10,
                step=1,
                min_=1,
                tooltip=tooltips.get("k_neighbors", ""),
            ),
            num_input(
                f"{sid}-param-var-prop-min",
                "Variance proportion",
                0.95,
                step=0.01,
                min_=0.1,
                max_=1.0,
                tooltip=tooltips.get("var_prop_min", ""),
            ),
            num_input(
                f"{sid}-param-max-pcs",
                "Max PCs",
                10,
                step=1,
                min_=2,
                tooltip=tooltips.get("max_pcs", ""),
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
        ("permanova", "PERMANOVA R²", "PERMANOVA.R"),
        ("r2", "Feature-wise ANOVA R²", "ANOVA.R"),
        ("prda", "pRDA", "pRDA.R"),
        ("pvca", "PVCA", "pvca.R"),
    ]
    post_extra = [
        ("alignment", "Alignment score", "Alignment_Score.R"),
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
                        dcc.Store(id=f"{stage}-{key}-run-state", storage_type="session"),
                        dcc.Interval(
                            id=f"{stage}-{key}-poll-interval",
                            interval=2000,
                            n_intervals=0,
                            disabled=True,
                        ),
                        *(controls or []),
                        html.Div(
                            [
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
                            ]
                        ),
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
                        [
                            html.Div(
                                "Click Run inside a tab to compute only that assessment.",
                            ),
                            html.Div(
                                "PNG previews are compressed to reduce page load; download TIFF files for full-resolution images.",
                            ),
                        ],
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
        ("permanova", "PERMANOVA R²", "PERMANOVA.R"),
        ("r2", "Feature-wise ANOVA R²", "ANOVA.R"),
        ("prda", "pRDA", "pRDA.R"),
        ("pvca", "PVCA", "pvca.R"),
    ]
    post_extra = [
        ("alignment", "Alignment score", "Alignment_Score.R"),
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
        outputs.append(Output(f"{sid}-param-store", "data", allow_duplicate=True))
        outputs.extend(
            [
                Output(f"{sid}-poll-interval", "disabled", allow_duplicate=True),
                Output(f"{sid}-poll-interval", "n_intervals", allow_duplicate=True),
                Output(f"{sid}-run-state", "data", allow_duplicate=True),
            ]
        )

        # Parameter States by group
        states: list = [State("session-id", "data")]
        param_state_ids: List[str] = []
        if key == "alignment":
            param_state_ids.extend([
                f"{sid}-param-k",
                f"{sid}-param-var-prop-min",
                f"{sid}-param-max-pcs",
            ])
            states += [
                State(f"{sid}-param-k", "value"),
                State(f"{sid}-param-var-prop-min", "value"),
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
            Input(f"{sid}-poll-interval", "n_intervals"),
            *states,
            State(f"{sid}-run-state", "data"),
            prevent_initial_call=True,
        )
        def _run_one(
            n_clicks: int,
            poll_ticks: int,
            *values,
            _stage=stage,
            _key=key,
            _script=script_name,
            _has_ncol=has_ncol_param,
            _state_ids=state_ids_tuple,
        ):
            ctx = dash.callback_context
            if not ctx.triggered:
                raise dash.exceptions.PreventUpdate
            trigger_id = ctx.triggered[0]["prop_id"].split(".")[0]

            # unpack session id
            if not values:
                raise dash.exceptions.PreventUpdate
            session_id = values[0]
            param_vals = list(values[1:-1])
            run_state = values[-1] if values else None
            persisted_payload = dash.no_update
            if _state_ids:
                persisted_payload = {"values": {pid: val for pid, val in zip(_state_ids, param_vals)}}

            def _output(
                content,
                stage_flag,
                log_path_value=dash.no_update,
                log_meta=dash.no_update,
                modal_open=dash.no_update,
                log_interval_disabled=dash.no_update,
                param_store=persisted_payload,
                poll_disabled=True,
                poll_count=dash.no_update,
                run_state_value=dash.no_update,
            ):
                return (
                    content,
                    stage_flag,
                    log_path_value,
                    log_meta,
                    modal_open,
                    log_interval_disabled,
                    param_store,
                    poll_disabled,
                    poll_count,
                    run_state_value,
                )
            if not session_id:
                message = html.Div("Session not initialised.")
                if _stage == "pre":
                    return _output(message, True, poll_disabled=True)
                return _output(message, dash.no_update, poll_disabled=True)

            session_dir = get_session_dir(session_id)
            if not (session_dir / "raw.csv").exists() or not (session_dir / "metadata.csv").exists():
                message = html.Div("Upload both raw.csv and metadata.csv first.")
                if _stage == "pre":
                    return _output(message, True, poll_disabled=True)
                return _output(message, dash.no_update, poll_disabled=True)

            expected_files = _expected_figure_files(_stage, _key)
            log_path = session_dir / "run.log"

            if trigger_id == run_id:
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
                if _key == "alignment":
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

                def _worker():
                    run_r_scripts((_script,), session_dir, log_path=log_path, extra_args=flags)

                threading.Thread(target=_worker, daemon=True).start()
                run_state_payload = {
                    "session": session_id,
                    "expected": expected_files,
                    "stage": _stage,
                    "key": _key,
                }
                stage_flag = True if _stage == "pre" else dash.no_update
                spinner = dbc.Spinner(
                    html.Div("Running..."),
                    color="primary",
                    type="border",
                    size="md",
                    fullscreen=False,
                )
                return _output(
                    spinner,
                    stage_flag,
                    log_path_value=str(log_path),
                    log_meta=None,
                    modal_open=dash.no_update,
                    log_interval_disabled=False,
                    poll_disabled=False,
                    poll_count=0,
                    run_state_value=run_state_payload,
                )

            # Polling branch
            run_state_valid = isinstance(run_state, dict) and run_state.get("session") == session_id
            if isinstance(run_state, dict) and not run_state_valid:
                raise dash.exceptions.PreventUpdate

            expected = run_state.get("expected") if run_state_valid else expected_files
            files_ready = _assessment_outputs_ready(session_dir, expected or expected_files)

            if not run_state_valid:
                if files_ready:
                    content = render_group_tabset(session_dir, _stage, _key)
                    run_state_payload = {
                        "session": session_id,
                        "expected": expected or expected_files,
                        "stage": _stage,
                        "key": _key,
                        "complete": True,
                    }
                    stage_flag = True if _stage == "pre" else dash.no_update
                    return _output(
                        content,
                        stage_flag,
                        log_path_value=str(log_path),
                        log_meta=None,
                        modal_open=dash.no_update,
                        log_interval_disabled=dash.no_update,
                        param_store=persisted_payload,
                        poll_disabled=True,
                        poll_count=poll_ticks,
                        run_state_value=run_state_payload,
                    )

                placeholder = html.Div("Click Run to generate results.")
                return _output(
                    placeholder,
                    dash.no_update,
                    param_store=persisted_payload,
                    poll_disabled=True,
                    poll_count=poll_ticks,
                    run_state_value=None,
                )

            if not files_ready:
                placeholder = dbc.Spinner(
                    html.Div("Waiting for output files..."),
                    color="primary",
                    type="border",
                    size="md",
                    fullscreen=False,
                )
                return _output(
                    placeholder,
                    dash.no_update,
                    param_store=persisted_payload,
                    poll_disabled=False,
                    poll_count=poll_ticks,
                    run_state_value=run_state,
                )

            content = render_group_tabset(session_dir, _stage, _key)
            run_state_payload = dict(run_state) if isinstance(run_state, dict) else {}
            run_state_payload.setdefault("session", session_id)
            run_state_payload.setdefault("expected", expected)
            run_state_payload["complete"] = True
            stage_flag = True if _stage == "pre" else dash.no_update
            return _output(
                content,
                stage_flag,
                log_path_value=str(log_path),
                log_meta=None,
                modal_open=dash.no_update,
                log_interval_disabled=dash.no_update,
                param_store=persisted_payload,
                poll_disabled=True,
                poll_count=poll_ticks,
                run_state_value=run_state_payload,
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

