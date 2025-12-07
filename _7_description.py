"""Centralised text content for help documentation and tooltips."""
from typing import Dict, List

from dash import html


HELP_SECTION_TOC: List[Dict[str, str]] = [
    {"id": "help-overview", "title": "Overview"},
    {"id": "help-upload", "title": "Upload Files"},
    {"id": "help-correction", "title": "Batch Effect Correction"},
    {"id": "help-assessment", "title": "Assessment (Pre & Post)"},
]


HELP_MODAL_SECTIONS: List = [
    html.Div(
        [
            html.H4("Overview"),
            html.P(
                "The navigation bar mirrors the workflow: upload your inputs, preview pre-correction diagnostics, launch batch-effect correction methods, and then compare the post-correction assessments before downloading the session bundle. "
                "Use the Logs button at any time to inspect long-running jobs."
            ),
        ],
        className="mb-4",
        id="help-overview",
    ),
    html.Div(
        [
            html.H4("Upload Files"),
            html.P(
                "Upload a raw count/abundance table and a matching metadata table, or pick one of the curated examples."
            ),
            html.Div(
                [
                    html.H5("Manual upload", className="mb-2 mt-3"),
                    html.Ul(
                        [
                            html.Li(
                                "Manual uploads accept CSV files."
                            ),
                            html.Li(
                                "The matrix must use features as rows, samples as columns, and omit row/column names. Use consistent feature order between the matrix and metadata to avoid mismatches."
                            ),
                            html.Li(
                                "Metadata CSVs must include: a Batch column (batch IDs), a target column (target label such as treatment/phenotype), and optional covariance columns (keep the count modest to avoid slow runs)."
                            ),
                            html.Li(
                                "Use the dropdowns below each table preview to map the batch, target, and covariance columns."
                            ),
                        ]
                    ),
                ]
            ),
            html.Div(
                [
                    html.H5("Example dataset", className="mb-2 mt-4"),
                    html.Ul(
                        [
                            html.Li(
                                "Example datasets auto-populate every mapping field with the recommended Batch, target, and covariance selections."
                            ),
                            html.Li(
                                "Choose from six curated datasets that have different batch sizes, feature counts, and batch/target distributions."
                            ),
                        ]
                    ),
                ]
            ),
            html.Div(
                [
                    html.H5("Mosaic plot", className="mb-2 mt-4"),
                    html.P(
                        "The mosaic plot cross-tabulates the Batch and target mappings so you "
                        "can confirm how many samples fall into each batch/label combination "
                        "before running corrections. It acts as a quick imbalance diagnostic "
                        "before investing time in lengthy methods."
                    ),
                ]
            ),
        ],
        className="mb-4",
        id="help-upload",
    ),
    html.Div(
        [
            html.H4("Batch Effect Correction"),
            html.P(
                "Select one or more correction strategies to run on the uploaded data."
            ),
            html.Ul(
                [
                    html.Li(
                        [
                            "Table columns:",
                            html.Ul(
                                [
                                    html.Li(
                                        "Methods lists each available algorithm so you can scan the "
                                        "options alongside their references. Expand the accordion on "
                                        "each method to review parameter presets before launching."
                                    ),
                                    html.Li(
                                        "Time (s) shows how long each method took to run in the current session "
                                        "so you can monitor performance without relying on prior sessions."
                                    ),
                                    html.Li(
                                        "Status shows whether that method already has outputs stored for "
                                        "the current session."
                                    ),
                                    html.Li(
                                        "Run Correction launches the job. The button disables itself after results are saved."
                                    ),
                                    html.Li(
                                        "Delete removes the stored outputs so you can re-run the method "
                                        "with different settings. This action only affects the selected "
                                        "method, while other results remain intact."
                                    ),
                                    html.Li(
                                        "Citation links to the publication or documentation for the "
                                        "selected method."
                                    ),
                                ],
                                className="mt-2",
                            ),
                        ]
                    ),
                    html.Li(
                        "Jobs run sequentially and stream status updates to the run log. You can "
                        "re-open the log modal while methods are running to watch the process in real time."
                    ),
                    html.Li(
                        "Once a method completes, its corrected matrix is stored for the "
                        "post-correction assessments and final download bundle. Use \"Download "
                        "results\" to export intermediate corrections at any time."
                    ),
                ]
            ),
        ],
        className="mb-4",
        id="help-correction",
    ),
    html.Div(
        [
            html.H4("Assessment (Pre & Post)"),
            html.P(
                "Assessment pages share the same controls. Pre-correction runs compute a "
                "baseline using the uploaded data, while post-correction runs let you compare "
                "each method's output."
            ),
            html.Ul(
                [
                    html.Li(
                        "Choose which scoring families to run (alignment, ordination, PVCA, etc.) "
                        "and configure their parameters via the tooltips. For reproducibility, "
                        "note any customised settings in the session log."
                    ),
                    html.Li(
                        "Trigger a run to queue the selected analyses. Progress appears in the "
                        "log modal and each finished figure is embedded on the page. Figures "
                        "update automatically when you re-run assessments after new corrections."
                    ),
                    html.Li(
                        "Use the detail table to compare pre vs. post performance and identify which "
                        "method best balances batch removal with phenotype separation."
                    ),
                    html.Li(
                        "Click any detail table column title to filter, sort, and re-rank methods "
                        "on the fly."
                    ),
                    html.Li(
                        "When you are satisfied, click \"Download results\" from the navbar to "
                        "export every corrected matrix, assessment figure, and log file."
                    ),
                ]
            ),
            html.Div(
                [
                    html.H5("Alignment score", className="mb-2 mt-4"),
                    html.Ul(
                        [
                            html.Li(
                                "Purpose: Quantifies how well batches mix in k-NN space after "
                                "correction (higher = stronger batch mixing). Use it to confirm "
                                "whether corrected embeddings blend batches without erasing the "
                                "target label."
                            ),
                            html.Li(
                                "Alignment Score column: Lists the numeric values used for ranking. "
                                "Scores close to 1 indicate that most neighbours originate from "
                                "different batches, confirming strong mixing."
                            ),
                        ]
                    ),
                ]
            ),
            html.Div(
                [
                    html.H5("PCA ordination", className="mb-2 mt-4"),
                    html.Ul(
                        [
                            html.Li(
                                "Purpose: Visualises global variance structure to check whether "
                                "batch dominates the first two PCs or if the target label remains "
                                "separable."
                            ),
                            html.Li(
                                "Figure: Each panel plots PC1 vs PC2 with ellipses around batch and "
                                "target groups. Balanced overlap and short batch-to-batch centroid "
                                "distances indicate effective correction. Watch whether ellipses "
                                "retain the target label ordering even after batches overlap."
                            ),
                            html.Li(
                                "Centroid Distance: Captures between-group spacing; smaller batch "
                                "distances imply tighter batch mixing while larger target distances "
                                "signal preserved biology."
                            ),
                            html.Li(
                                "Ellipse Overlap: Summarises how much batch or target clusters "
                                "overlap. More overlap for batches and less overlap for targets "
                                "indicate good correction."
                            ),
                            html.Li(
                                "Target vs Batch Centroid Delta: Highlights how far apart target "
                                "centroids remain relative to batch centroids so you can prioritise "
                                "methods that preserve target structure."
                            ),
                        ]
                    ),
                ]
            ),
            html.Div(
                [
                    html.H5("PCoA ordination", className="mb-2 mt-4"),
                    html.Ul(
                        [
                            html.Li(
                                "Purpose: Performs Principal Coordinates Analysis in CLR and TSS "
                                "geometries to reveal geometry-specific batch structure, especially "
                                "when Euclidean PCA misses compositional trends."
                            ),
                            html.Li(
                                "Figure: Use the Aitchison vs Bray-Curtis sub-tabs to see how "
                                "batches overlap on each geometry; tighter batch ellipses and clear "
                                "target separation imply successful corrections. Pay attention to "
                                "axes that flip order when corrections change the distance rankings."
                            ),
                            html.Li(
                                "Centroid Distance: Interpreted the same way as PCA but applied to "
                                "CLR or TSS geometries."
                            ),
                            html.Li(
                                "Ellipse Overlap: Reports geometry-specific overlap so you can "
                                "compare how each correction behaves under CLR vs. TSS distances."
                            ),
                            html.Li(
                                "Target vs Batch Centroid Delta: Measures the relative spacing of "
                                "target and batch centroids per geometry to catch geometry-specific "
                                "trade-offs."
                            ),
                        ]
                    ),
                ]
            ),
            html.Div(
                [
                    html.H5("NMDS ordination", className="mb-2 mt-4"),
                    html.Ul(
                        [
                            html.Li(
                                "Purpose: Uses non-metric multidimensional scaling on CLR and TSS "
                                "dissimilarities to examine nonlinear structure, highlighting "
                                "curved manifolds that PCA/PCoA flatten."
                            ),
                            html.Li(
                                "Figure: Inspect NMDS1 vs NMDS2 scatterplots and ensure stress "
                                "values remain below ~0.3 while batches overlap and targets stay "
                                "separable. Runs with diverging stress between geometries often "
                                "signal that a correction only helped a subset of compositions."
                            ),
                            html.Li(
                                "NMDS Stress: Reports fit quality; lower stress indicates that the "
                                "NMDS embedding faithfully represents the dissimilarities."
                            ),
                            html.Li(
                                "Centroid Distance: Uses NMDS coordinates to capture between-group "
                                "spacing with the same interpretation as PCA/PCoA."
                            ),
                            html.Li(
                                "Ellipse Overlap: Summarises NMDS cluster overlap so you can spot "
                                "batches that still dominate the manifold."
                            ),
                            html.Li(
                                "Target vs Batch Centroid Delta: Emphasises methods with high "
                                "target-vs-batch separation ratios while keeping stress low."
                            ),
                        ]
                    ),
                ]
            ),
            html.Div(
                [
                    html.H5("Dissimilarity heatmaps", className="mb-2 mt-4"),
                    html.Ul(
                        [
                            html.Li(
                                "Purpose: Compares pairwise distance matrices (CLR & TSS) to spot "
                                "batches that remain isolated after correction. Use it when you need "
                                "to confirm that distance shrinkage is uniform across all batches."
                            ),
                            html.Li(
                                "Figure: Heatmaps display within- vs between-batch distances. Cool "
                                "tones along off-diagonals imply reduced cross-batch separation."
                            ),
                            html.Li(
                                "ANOSIM R: Quantifies whether within-batch distances are smaller than "
                                "between-batch distances. Values near 0 mean little batch-driven "
                                "structure, while values above ~0.5 flag strong residual batch "
                                "effects. Use the companion p-value column to confirm significance."
                            ),
                        ]
                    ),
                ]
            ),
            html.Div(
                [
                    html.H5("PERMANOVA", className="mb-2 mt-4"),
                    html.Ul(
                        [
                            html.Li(
                                "Purpose: Tests whether batch explains a significant portion of the "
                                "distance matrix variance in each geometry."
                            ),
                            html.Li(
                                "Figure: Box or bar plots show the distributions per method; "
                                "smaller R² implies weaker batch association."
                            ),
                            html.Li(
                                "PERMANOVA R²: Records the proportion of variance explained by batch. "
                                "Values below ~0.05 typically indicate minimal batch impact."
                            ),
                        ]
                    ),
                ]
            ),
            html.Div(
                [
                    html.H5("Per-feature ANOVA (Median R²)", className="mb-2 mt-4"),
                    html.Ul(
                        [
                            html.Li(
                                "Purpose: Summarises how much of each feature's variance is explained "
                                "by batch vs. treatment using per-feature ANOVA."
                            ),
                            html.Li(
                                "Figure: Dual bars show the competing batch and treatment medians per "
                                "method so you can aim for high treatment signal and low batch signal."
                            ),
                            html.Li(
                                "Median R² (Batch): Should shrink after correction to show that batch "
                                "explains less feature-level variance."
                            ),
                            html.Li(
                                "Median R² (Treatment): Higher values indicate preserved biological "
                                "signal across features."
                            ),
                        ]
                    ),
                ]
            ),
            html.Div(
                [
                    html.H5("Partial RDA", className="mb-2 mt-4"),
                    html.Ul(
                        [
                            html.Li(
                                "Purpose: Decomposes constrained variance into treatment, batch, and "
                                "shared components to check whether correction prioritises the target "
                                "label. The analysis partials out covariates so you can attribute "
                                "changes to the chosen label."
                            ),
                            html.Li(
                                "Figure: Stacked bars show how much partial RDA variance is assigned "
                                "to each component—seek taller treatment segments and shrunken batch "
                                "segments."
                            ),
                            html.Li(
                                "Treatment variance: Shows the fraction of constrained variance "
                                "attributed to the target label—higher bars are preferred."
                            ),
                            html.Li(
                                "Batch variance: Reports how much variance batch still explains. "
                                "Lower fractions suggest better correction."
                            ),
                            html.Li(
                                "Intersection variance: Captures overlap between batch and treatment "
                                "effects; large intersections imply residual confounding."
                            ),
                            html.Li(
                                "Residual variance: Reflects unexplained variance so you can assess "
                                "model fit when treatment and batch effects are small."
                            ),
                        ]
                    ),
                ]
            ),
            html.Div(
                [
                    html.H5("PVCA", className="mb-2 mt-4"),
                    html.Ul(
                        [
                            html.Li(
                                "Purpose: Applies Principal Variance Component Analysis to apportion "
                                "variance across batch, treatment, and residual sources."
                            ),
                            html.Li(
                                "Figure: Similar to partial RDA, stacked bars reveal whether batch "
                                "variance shrinks relative to treatment across corrected matrices."
                            ),
                            html.Li(
                                "Treatment variance: PVCA-estimated fraction attributed to treatment."
                            ),
                            html.Li(
                                "Batch variance: PVCA-estimated batch contribution that should drop "
                                "after correction."
                            ),
                            html.Li(
                                "Intersection variance: PVCA overlap between batch and treatment "
                                "effects, highlighting confounding."
                            ),
                            html.Li(
                                "Residual variance: PVCA residual component that signals unmodelled "
                                "noise or covariates."
                            ),
                        ]
                    ),
                ]
            ),
            html.Div(
                [
                    html.H5("Entropy batch mixing (EBM)", className="mb-2 mt-4"),
                    html.Ul(
                        [
                            html.Li(
                                "Purpose: Measures k-NN batch entropy within UMAP embeddings (higher "
                                "entropy = better batch mixing)."
                            ),
                            html.Li(
                                "EBM column: Exposes the average entropy value across anchor pools "
                                "used for ranking. Values above 0.8 suggest near-perfect mixing."
                            ),
                        ]
                    ),
                ]
            ),
            html.Div(
                [
                    html.H5("UMAP silhouette", className="mb-2 mt-4"),
                    html.Ul(
                        [
                            html.Li(
                                "Purpose: Evaluates how well target-label clusters remain compact and "
                                "separated after correction, ensuring biological signal survives the "
                                "batch removal."
                            ),
                            html.Li(
                                "Silhouette column: Reports the rescaled mean silhouette width so you "
                                "can audit the precise values. Scores near 0.5 signal modest "
                                "separation, while >0.7 reflects well-separated targets."
                            ),
                        ]
                    ),
                ]
            ),
        ],
        className="mb-0",
        id="help-assessment",
    ),
]

# Parameter tooltips for assessment configuration controls.
ASSESSMENT_PARAM_TOOLTIPS: Dict[str, Dict[str, str]] = {
    "alignment": {
        "k_neighbors": "k for k-NN graph when computing Alignment Score.",
        "var_prop_min": "Minimum cumulative variance proportion to retain when selecting PCs.",
        "max_pcs": "Maximum number of PCs considered when computing Alignment Score.",
    },
    "ebm": {
        "umap_neighbors": "UMAP: Number of neighbors (local connectivity).",
        "umap_min_dist": "UMAP: Minimum distance between points in embedding.",
        "umap_metric": "UMAP distance metric (CLR uses Euclidean).",
        "knn_k": "k for entropy mixing (neighbors per anchor).",
        "knn_pools": "Number of anchor pools to average entropy over.",
        "knn_per_label": "Anchors sampled per batch label per pool.",
    },
    "silhouette": {
        "umap_neighbors": "UMAP: Number of neighbors (local connectivity).",
        "umap_min_dist": "UMAP: Minimum distance between points in embedding.",
        "umap_metric": "UMAP distance metric (CLR uses Euclidean).",
    },
}

FIGURE_DIMENSION_OVERRIDE_TOOLTIP = (
    "Override exported figure dimensions (pixels converted using DPI). "
    "Leave blank to keep script defaults."
)
FIGURE_DPI_TOOLTIP = "Dots per inch used when saving figures."
FIGURE_SUBPLOTS_TOOLTIP = "Number of method panels per row (where applicable)."

RANKING_SCORE_DESCRIPTIONS: Dict[str, str] = {
    "alignment": (
        "**Score formula:** $S = \\overline{1 - p_{\\text{same-batch}}}$ where $p_{\\text{same-batch}}$ is the fraction of each sample's k-NN drawn from its own batch.\\n\\n"
        "**Interpretation:** Higher scores indicate stronger batch mixing after correction, as fewer neighbours belong to the original batch."
    ),
    "pca": (
        "**Score formula:** $S = c_{1:2} \\times \\frac{w_{\\text{batch}}}{w_{\\text{batch}} + d_{\\text{batch}}}$\n\n"
        "**Symbols:** $c_{1:2}$ = variance coverage of PC1 and PC2; $w_{\\text{batch}}$ = mean within-batch dispersion on PC1-2; "
        "$d_{\\text{batch}}$ = mean Euclidean distance between batch centroids.\n\nHigher scores reflect strong variance retention "
        "and low between-batch separation relative to within-batch spread."
    ),
    "pcoa": (
        "**Score formula:** $S = \\big(S_{\\text{CLR}} S_{\\text{TSS}}\\big)^{1/2}$ with $S_{\\text{geom}} = c_{1:2} \\times \\frac{w_{\\text{geom}}}{w_{\\text{geom}} + d_{\\text{geom}}}$\n\n"
        "**Symbols:** $S_{\\text{CLR}}, S_{\\text{TSS}}$ = geometry-specific PCoA scores; $c_{1:2}$ = variance coverage of the first two PCoA axes; "
        "$w_{\\text{geom}}$ = mean within-batch dispersion on that geometry's first two axes; $d_{\\text{geom}}$ = mean distance between batch centroids in that geometry.\n\n"
        "Balances CLR and TSS PCoA via a geometric mean while contrasting between- vs within-batch separation."
    ),
    "nmds": (
        "**Score formula:** $S = \\big(S_{\\text{CLR}} S_{\\text{TSS}}\\big)^{1/2}$, $S_{\\text{geom}} = \\sqrt{\\frac{w_{\\text{geom}}}{w_{\\text{geom}} + d_{\\text{geom}}} \\times \\Big(1 - \\frac{\\min(\\text{stress}, 0.30)}{0.30}\\Big)}$\n\n"
        "**Symbols:** $S_{\\text{CLR}}, S_{\\text{TSS}}$ = geometry-specific NMDS scores; $w_{\\text{geom}}$ = mean within-batch dispersion on NMDS1-2; "
        "$d_{\\text{geom}}$ = mean distance between batch centroids; $\\text{stress}$ = Kruskal stress of the NMDS fit capped at $0.30$.\n\nEmphasises low between-batch separation relative to within-batch spread while keeping NMDS stress low."
    ),
    "dissimilarity": (
        "**Score formula:** $S = S_{\\text{CLR}}^{w_a} S_{\\text{TSS}}^{w_b}$ (with $w_a + w_b = 1$), $S_{\\text{geom}} = \\frac{1}{1 + \\overline{\\text{RMSE}}_{\\text{between}}}$\n\n"
        "**Symbols:** $S_{\\text{CLR}}, S_{\\text{TSS}}$ = geometry-specific dissimilarity scores; $w_a, w_b$ = weights for CLR and TSS contributions; "
        "$\\overline{\\text{RMSE}}_{\\text{between}}$ = mean between-batch RMSE of the distance matrix.\n\nPenalises large between-batch dissimilarity in CLR and TSS geometries."
    ),
    "r2": (
        "**Score formula:** $S = \\big(S_{\\text{CLR}} S_{\\text{TSS}}\\big)^{1/2}$, $S_{\\text{geom}} = \\tilde{R}^2_{\\text{treat}} \\times \\big(1 - \\tilde{R}^2_{\\text{batch}}\\big)$\n\n"
        "**Symbols:** $\\tilde{R}^2_{\\text{treat}}$ = median per-feature ANOVA $R^2$ for the treatment effect; $\\tilde{R}^2_{\\text{batch}}$ = median $R^2$ for the batch effect.\n\n"
        "Uses median per-feature ANOVA $R^2$ to encourage treatment signal and suppress batch signal."
    ),
    "prda": (
        "**Score formula:** $S = \\big(S_{\\text{CLR}} S_{\\text{TSS}}\\big)^{1/2}$, $S_{\\text{geom}} = \\frac{T}{T + B}$\n\n"
        "**Symbols:** $T$ = fraction of variance attributed to treatment; $B$ = fraction attributed to batch in the partial RDA decomposition.\n\n"
        "Compares treatment vs batch variance fractions from partial RDA."
    ),
    "pvca": (
        "**Score formula:** $S = \\big(S_{\\text{CLR}} S_{\\text{TSS}}\\big)^{1/2}$, $S_{\\text{geom}} = \\frac{T}{T + B}$\n\n"
        "**Symbols:** $T$ = PVCA-estimated treatment variance fraction; $B$ = PVCA-estimated batch variance fraction.\n\nLeverages PVCA variance components to favour low batch contribution."
    ),
    "ebm": (
        "**Score formula:** $S = \\frac{1}{P} \\sum_{p=1}^{P} H_{\\text{batch}}(p)$\n\n"
        "**Symbols:** $P$ = number of anchor pools; $H_{\\text{batch}}(p)$ = entropy of batch labels in pool $p$'s neighborhood.\n\n"
        "Averages k-NN batch entropies across UMAP anchor pools."
    ),
    "silhouette": (
        "**Score formula:** $S = \\tfrac{1}{2}(1 + \\bar{s})$\n\n"
        "**Symbols:** $\\bar{s}$ = mean silhouette width of samples in UMAP space.\n\nRescales the mean UMAP silhouette width so tighter phenotype clusters score higher."
    ),
}
