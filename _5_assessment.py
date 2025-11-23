# ===============================
# File: _5_assessment.py
# ===============================
import shutil
import threading
from pathlib import Path

from dash import html, dcc
from dash.dependencies import Input, Output, State
import dash
import dash_bootstrap_components as dbc
from typing import List, Optional, Sequence, Set, Tuple

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
    session_data_dir,
    session_preview_dir,
    session_results_dir,
    reorganize_session_outputs,
    PRE_SCRIPTS,
    POST_SCRIPTS,
    PRE_FIGURES,
    POST_FIGURES,
    render_assessment_tabs,
    render_group_tabset,
    build_group_subtab_content,
    build_ranking_tab,
    build_raw_assessments_tab,
    append_run_log,
    ensure_preview_images,
    any_method_outputs,
)


FIGURE_DEFAULTS = {
    "alignment": {"width": 2800, "height": 1800, "dpi": 300},
    "pca": {"width": 1800, "height": 1200, "dpi": 300, "ncol": 3},
    "pcoa": {"width": 1800, "height": 1200, "dpi": 300, "ncol": 3},
    "nmds": {"width": 1800, "height": 1200, "dpi": 300, "ncol": 3},
    "dissimilarity": {"width": 1800, "height": 1200, "dpi": 300, "ncol": 3},
    "permanova": {"width": 2800, "height": 1800, "dpi": 300},
    "r2": {"width": 4800, "height": 1200, "dpi": 300},
    "prda": {"width": 3000, "height": 1500, "dpi": 300},
    "pvca": {"width": 3000, "height": 1500, "dpi": 300},
    "ebm": {"width": 2550, "height": 1560, "dpi": 300},
    "silhouette": {"width": 2800, "height": 1800, "dpi": 300},
}


_FOUND_OUTPUT_LOGS: Set[str] = set()


def _output_path_for(session_dir: Path, fname: str) -> Path:
    target = Path(fname)
    suffix = target.suffix.lower()
    if suffix == ".png":
        return session_preview_dir(session_dir) / target.name
    if suffix in {".csv", ".tif", ".tiff"}:
        return session_results_dir(session_dir) / target.name
    return session_results_dir(session_dir) / target.name


def _prepare_results_inputs(session_dir: Path) -> None:
    data_dir = session_data_dir(session_dir)
    results_dir = session_results_dir(session_dir)

    def _mirror(src: Path, dest: Path) -> None:
        try:
            if dest.exists() or dest.is_symlink():
                dest.unlink()
            dest.symlink_to(src)
        except OSError:
            try:
                shutil.copy2(src, dest)
            except OSError:
                pass

    for csv_path in data_dir.glob("*.csv"):
        _mirror(csv_path, results_dir / csv_path.name)

    cfg_path = session_dir / "session_config.json"
    if cfg_path.exists():
        _mirror(cfg_path, results_dir / cfg_path.name)


def _reset_found_logs(log_path: Optional[Path]) -> None:
    """Drop cached discovery entries for the given run log so re-runs re-log finds."""

    if not log_path:
        return
    prefix = f"{log_path.resolve()}::"
    stale = {entry for entry in _FOUND_OUTPUT_LOGS if entry.startswith(prefix)}
    if stale:
        _FOUND_OUTPUT_LOGS.difference_update(stale)


def _clear_outputs(session_dir: Path, expected_files: Sequence[str]) -> None:
    """Remove prior output files (figures and tables) before starting a run."""

    for fname in expected_files:
        path = _output_path_for(session_dir, fname)
        stem = path.with_suffix("")
        for target in (path, stem.with_suffix(".png"), stem.with_suffix(".tif"), stem.with_suffix(".tiff")):
            try:
                target.unlink(missing_ok=True)
            except OSError:
                pass


def _expected_figure_files(stage: str, key: str) -> List[str]:
    """Return expected output filenames (figures + assessment tables) for a group."""

    figures: Sequence = PRE_FIGURES if stage == "pre" else POST_FIGURES
    key = key.lower()
    expected: List[Optional[str]] = []

    def add_if(condition: bool, filename: Optional[str]):
        if condition and filename:
            expected.append(filename)

    def add_assessment_tables(
        base_name: Optional[str],
        multi_geometry: bool = False,
        include_combined: bool = True,
    ):
        if not base_name:
            return
        suffix = "pre" if stage == "pre" else "post"
        if include_combined:
            expected.append(f"{base_name}_raw_assessment_{suffix}.csv")
        if multi_geometry:
            expected.append(f"{base_name}_aitchison_raw_assessment_{suffix}.csv")
            expected.append(f"{base_name}_braycurtis_raw_assessment_{suffix}.csv")

    for spec in figures:
        low = spec.filename.lower()
        stem = Path(low).stem
        if key == "pcoa":
            add_if(low.startswith("pcoa_aitchison_batch"), spec.filename)
            add_if(low.startswith("pcoa_aitchison_target"), spec.filename)
            add_if(low.startswith("pcoa_braycurtis_batch"), spec.filename)
            add_if(low.startswith("pcoa_braycurtis_target"), spec.filename)
            add_assessment_tables(
                "pcoa", multi_geometry=True, include_combined=False
            )
        elif key == "nmds":
            add_if(low.startswith("nmds_aitchison_batch"), spec.filename)
            add_if(low.startswith("nmds_aitchison_target"), spec.filename)
            add_if(low.startswith("nmds_braycurtis_batch"), spec.filename)
            add_if(low.startswith("nmds_braycurtis_target"), spec.filename)
            add_assessment_tables(
                "nmds", multi_geometry=True, include_combined=False
            )
        elif key == "dissimilarity":
            add_if(low.startswith("dissimilarity_heatmaps_aitchison"), spec.filename)
            add_if(low.startswith("dissimilarity_heatmaps_braycurtis"), spec.filename)
            add_assessment_tables(
                "dissimilarity", multi_geometry=True, include_combined=False
            )
        elif key == "permanova":
            add_if(low.startswith("permanova_aitchison"), spec.filename)
            add_if(low.startswith("permanova_braycurtis"), spec.filename)
            add_if(low.startswith("permanova"), spec.filename)
            add_assessment_tables(
                "permanova", multi_geometry=True, include_combined=False
            )
        elif key == "r2":
            add_if(low.startswith("anova_aitchison"), spec.filename)
            add_if(low.startswith("anova_braycurtis"), spec.filename)
            add_assessment_tables("anova")
        elif key == "prda":
            add_if(low.startswith("prda_aitchison"), spec.filename)
            add_if(low.startswith("prda_braycurtis"), spec.filename)
            add_assessment_tables("pRDA")
        elif key == "alignment" and stem in {"alignment_score", "alignment"}:
            add_if(True, spec.filename)
            add_assessment_tables("alignment_score")
        elif key == "pca":
            add_if(stem == "pca_batch", spec.filename)
            add_if(stem == "pca_target", spec.filename)
            add_assessment_tables("pca")
        elif key == "pvca" and stem == "pvca":
            add_if(True, spec.filename)
            add_assessment_tables("PVCA")
        elif key == "ebm" and stem == "ebm":
            add_if(True, spec.filename)
            add_assessment_tables("ebm")
        elif key == "silhouette" and stem == "silhouette":
            add_if(True, spec.filename)
            add_assessment_tables("silhouette")

    # Preserve order but drop None/duplicates
    seen = set()
    unique: List[str] = []
    for fname in expected:
        if fname and fname not in seen:
            unique.append(fname)
            seen.add(fname)
    return unique


def _assessment_outputs_status(
    session_dir: Path, expected_files: Sequence[str], log_path: Optional[Path] = None
) -> Tuple[bool, int, int]:
    """Return readiness plus how many expected files are available."""

    if not expected_files:
        return False, 0, 0

    def _exists(name: str) -> bool:
        target = _output_path_for(session_dir, name)

        def _log_hit(hit: Path) -> None:
            if log_path is None:
                return
            log_key = f"{log_path.resolve()}::{hit.resolve()}"
            if log_key in _FOUND_OUTPUT_LOGS:
                return
            _FOUND_OUTPUT_LOGS.add(log_key)
            append_run_log(log_path, f"Found expected output: {hit}", icon="📄")

        if target.exists():
            _log_hit(target)
            return True

        # On some platforms the filesystem can surface unexpected casing; fall back to a
        # case-insensitive search so detected files always advance progress.
        lowered = target.name.lower()
        for candidate in target.parent.glob("*"):
            if candidate.name.lower() == lowered:
                _log_hit(candidate)
                return True
        return False

    ready = 0
    for fname in expected_files:
        if _exists(fname):
            ready += 1

    total = len(expected_files)
    if ready == total and log_path is not None:
        all_key = f"{log_path.resolve()}::ALL_READY"
        if all_key not in _FOUND_OUTPUT_LOGS:
            _FOUND_OUTPUT_LOGS.add(all_key)
            append_run_log(log_path, "All expected outputs found.", icon="✅")

    return ready == total, ready, total


def _assessment_inputs_ready(session_dir: Optional[Path], stage: str) -> bool:
    """Check whether required inputs are present for the given stage."""

    if session_dir is None:
        return False

    data_dir = session_data_dir(session_dir)
    raw = (data_dir / "raw.csv").exists()
    meta = (data_dir / "metadata.csv").exists()
    if not (raw and meta):
        return False

    if stage == "post":
        return any_method_outputs(session_dir)

    return True


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
        placeholder = html.Div(
            [
                html.Div("Click Run to generate results."),
                html.Div(
                    [
                        dcc.Tabs(id=f"{stage}-{key}-subtabs", value=None, children=[]),
                        html.Div(id=f"{stage}-{key}-subtab-content"),
                    ],
                    style={"display": "none"},
                ),
            ]
        )
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
                                    className="mb-2 be-run-button",
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
            dcc.Interval(id="assessment-input-check", interval=3000, n_intervals=0),
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

    def _register_group(stage: str, key: str, script_name: str, title: str):
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
        outputs.append(Output(run_id, "disabled", allow_duplicate=True))
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
            _title=title,
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
            run_state_value = values[-1] if len(values) > 1 else None
            param_vals = list(values[1:-1])
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
                run_button_disabled=dash.no_update,
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
                    run_button_disabled,
                )
            if not session_id:
                message = html.Div("Session not initialised.")
                if _stage == "pre":
                    return _output(
                        message,
                        True,
                        poll_disabled=True,
                        run_state_value="idle",
                        run_button_disabled=False,
                    )
                return _output(
                    message,
                    dash.no_update,
                    poll_disabled=True,
                    run_state_value="idle",
                    run_button_disabled=False,
                )

            session_dir = get_session_dir(session_id)
            data_dir = session_data_dir(session_dir)
            results_dir = session_results_dir(session_dir)
            if not (data_dir / "raw.csv").exists() or not (data_dir / "metadata.csv").exists():
                message = html.Div("Upload both raw.csv and metadata.csv first.")
                if _stage == "pre":
                    return _output(
                        message,
                        True,
                        poll_disabled=True,
                        run_state_value="idle",
                        run_button_disabled=False,
                    )
                return _output(
                    message,
                    dash.no_update,
                    poll_disabled=True,
                    run_state_value="idle",
                    run_button_disabled=False,
                )

            expected_files = _expected_figure_files(_stage, _key)
            log_path = session_dir / "run.log"

            if trigger_id == run_id:
                # Build CLI flags from parameters
                flags = []

                _reset_found_logs(log_path)
                _clear_outputs(session_dir, expected_files)

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
                    _prepare_results_inputs(session_dir)
                    run_r_scripts(
                        (_script,),
                        results_dir,
                        log_path=log_path,
                        extra_args=flags,
                    )
                    reorganize_session_outputs(session_dir)

                threading.Thread(target=_worker, daemon=True).start()
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
                    run_state_value="running",
                    run_button_disabled=True,
                )

            # Polling branch
            if run_state_value != "running":
                raise dash.exceptions.PreventUpdate

            files_ready, ready_count, total_expected = _assessment_outputs_status(
                session_dir, expected_files, log_path=log_path
            )

            if not files_ready:
                progress = (
                    f"Waiting for output files ({ready_count}/{total_expected})..."
                    if total_expected
                    else "Waiting for output files..."
                )
                placeholder = dbc.Spinner(
                    html.Div(progress),
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
                    run_state_value="running",
                    run_button_disabled=True,
                )

            complete_key = f"{log_path.resolve()}::COMPLETE::{_title}"
            if complete_key not in _FOUND_OUTPUT_LOGS:
                _FOUND_OUTPUT_LOGS.add(complete_key)
                append_run_log(
                    log_path,
                    f"All expected outputs found for {_title}.",
                    icon="✅",
                )

            ensure_preview_images(session_dir, expected_files, log_path=log_path)

            content = render_group_tabset(session_dir, _stage, _key)
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
                poll_count=0,
                run_state_value="idle",
                run_button_disabled=False,
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

        @app.callback(
            Output(run_id, "disabled", allow_duplicate=True),
            Input("assessment-input-check", "n_intervals"),
            State("session-id", "data"),
            State(f"{sid}-poll-interval", "disabled"),
            prevent_initial_call="initial_duplicate",
        )
        def _toggle_run_enabled(_, session_id, poll_disabled, _stage=stage):
            if session_id is None:
                return True
            session_dir = get_session_dir(session_id)
            if poll_disabled is False:
                return True
            return not _assessment_inputs_ready(session_dir, _stage)

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
    for key, title, script in pre_groups:
        _register_group("pre", key, script, title)
    # Register post-stage groups (baseline pre groups plus post-only extras)
    for key, title, script in pre_groups + post_extra:
        _register_group("post", key, script, title)

