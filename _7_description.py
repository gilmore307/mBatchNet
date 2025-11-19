"""Centralised text content for help documentation and tooltips."""
from typing import Dict, List

from dash import html


HELP_MODAL_SECTIONS: List = [
    html.Div(
        [
            html.H4("Overview"),
            html.P(
                "The navigation bar mirrors the workflow: upload your inputs, preview pre-"
                "correction diagnostics, launch batch-effect correction methods, and then "
                "compare the post-correction assessments before downloading the session "
                "bundle. Use the Logs button at any time to inspect long-running jobs."
            ),
        ],
        className="mb-4",
    ),
    html.Div(
        [
            html.H4("Upload Files"),
            html.P(
                "Upload a raw count/abundance table and a matching metadata table, or pick "
                "one of the curated examples."
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
                                "The matrix must use features as rows, samples as columns, and "
                                "omit row/column names."
                            ),
                            html.Li(
                                "Metadata CSVs must include: a Batch column (batch IDs), a target "
                                "column (target label such as treatment/phenotype), and optional "
                                "covariance columns (keep the count modest to avoid slow runs)."
                            ),
                            html.Li(
                                "Use the dropdowns below each table preview to map the batch, "
                                "target, and covariance columns. These mappings are remembered in "
                                "the session."
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
                        "before running corrections."
                    ),
                    html.Ul(
                        [
                            html.Li(
                                "Click \"Generate mosaic plot\" after both files (or an example) "
                                "are loaded to render the batch-by-target grid."
                            ),
                            html.Li(
                                "Tile area encodes the sample count for that combination: evenly "
                                "sized tiles imply balanced coverage, while thin or missing tiles "
                                "highlight under-represented groups."
                            ),
                            html.Li(
                                "Hover to see exact counts and percentages so you can decide "
                                "whether to rebalance or drop sparse categories before proceeding."
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
                                "Example datasets auto-populate every mapping field with the "
                                "recommended Batch, target, and covariance selections."
                            ),
                            html.Li(
                                "Review the previews to learn the expected formatting before "
                                "switching back to manual uploads."
                            ),
                        ]
                    ),
                ]
            ),
        ],
        className="mb-4",
    ),
    html.Div(
        [
            html.H4("Batch Effect Correction"),
            html.P(
                "Select one or more correction strategies, tune their optional parameters, "
                "and launch them as a batch job."
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
                                        "options alongside their references."
                                    ),
                                    html.Li(
                                        "Avg Time (s) displays the median runtime gathered from prior "
                                        "sessions to help gauge how long a run may take."
                                    ),
                                    html.Li(
                                        "Status shows whether that method already has outputs stored for "
                                        "the current session."
                                    ),
                                    html.Li(
                                        "Run Correction launches the job once both uploads are mapped. The "
                                        "button disables itself after results are saved."
                                    ),
                                    html.Li(
                                        "Delete removes the stored outputs so you can re-run the method "
                                        "with different settings."
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
                        "The sidebar keeps track of which methods have finished and which are "
                        "still queued."
                    ),
                    html.Li(
                        "Jobs run sequentially and stream status updates to the run log. You can "
                        "re-open the log modal while methods are running."
                    ),
                    html.Li(
                        "Once a method completes, its corrected matrix is stored for the "
                        "post-correction assessments and final download bundle."
                    ),
                ]
            ),
        ],
        className="mb-4",
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
                        "and configure their parameters via the tooltips."
                    ),
                    html.Li(
                        "Trigger a run to queue the selected analyses. Progress appears in the "
                        "log modal and each finished figure is embedded on the page."
                    ),
                    html.Li(
                        "Use the ranking table to compare pre vs. post scores and identify which "
                        "method best balances batch removal with phenotype separation."
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
                                "correction (higher = stronger batch mixing)."
                            ),
                            html.Li(
                                "Figure: Bars show the alignment score for each method across the "
                                "selected geometries so you can instantly spot which runs draw "
                                "neighbours from multiple batches."
                            ),
                            html.Li(
                                "Details: Lists the Alignment Score ↑ column so you can compare the "
                                "exact numeric values that feed the ranking table."
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
                                "distances indicate effective correction."
                            ),
                            html.Li(
                                "Details: Centroid Distance (Batch ↓ / Target ↑) captures between-"
                                "group spacing, Ellipse Overlap (Batch ↑ / Target ↓) summarises "
                                "cluster overlap, and Target vs Batch Centroid Delta ↑ highlights "
                                "phenotype separation relative to batch."
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
                                "geometries to reveal geometry-specific batch structure."
                            ),
                            html.Li(
                                "Figure: Use the Aitchison vs Bray-Curtis sub-tabs to see how "
                                "batches overlap on each geometry; tighter batch ellipses and clear "
                                "target separation imply successful corrections."
                            ),
                            html.Li(
                                "Details: Shares the same Centroid Distance, Ellipse Overlap, and "
                                "Target vs Batch Centroid Delta metrics as PCA so you can compare "
                                "directions across geometries."
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
                                "dissimilarities to examine nonlinear structure."
                            ),
                            html.Li(
                                "Figure: Inspect NMDS1 vs NMDS2 scatterplots and ensure stress "
                                "values remain below ~0.3 while batches overlap and targets stay "
                                "separable."
                            ),
                            html.Li(
                                "Details: NMDS Stress ↓ reports fit quality, while Centroid Distance, "
                                "Ellipse Overlap, and Target vs Batch Centroid Delta mirror the "
                                "PCA/PCoA diagnostics."
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
                                "batches that remain isolated after correction."
                            ),
                            html.Li(
                                "Figure: Heatmaps display within- vs between-batch distances. Cool "
                                "tones along off-diagonals imply reduced cross-batch separation."
                            ),
                            html.Li(
                                "Details: ANOSIM R ↓ indicates how batch labels explain the distance "
                                "matrix—values near 0 mean little batch-driven structure."
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
                                "Figure: Box or bar plots show the pseudo-F distributions per method; "
                                "smaller R² implies weaker batch association."
                            ),
                            html.Li(
                                "Details: The R² ↓ column records the proportion of variance explained "
                                "by batch so you can track how each method suppresses it."
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
                                "Details: Median R² (Batch ↓) should shrink after correction, while "
                                "Median R² (Treatment ↑) indicates preserved biological signal."
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
                                "label."
                            ),
                            html.Li(
                                "Figure: Stacked bars show how much partial RDA variance is assigned "
                                "to each component—seek taller treatment segments and shrunken batch "
                                "segments."
                            ),
                            html.Li(
                                "Details: Treatment ↑, Batch ↓, Intersection ↓, and Residuals ↓ "
                                "columns spell out the precise variance fractions per geometry."
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
                                "Details: Treatment ↑, Batch ↓, Intersection ↓, and Residuals ↓ mirror "
                                "the PRDA table but derive from PVCA components."
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
                                "Figure: Line or bar plots summarise the mean EBM value per method so "
                                "you can prioritise corrections that fully intermingle batches."
                            ),
                            html.Li(
                                "Details: The EBM ↑ column exposes the average entropy value across "
                                "anchor pools that feeds the ranking score."
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
                                "separated after correction."
                            ),
                            html.Li(
                                "Figure: Bars or ridgelines show the mean silhouette width derived "
                                "from the UMAP embedding—higher values mean cleaner target clusters."
                            ),
                            html.Li(
                                "Details: The Silhouette ↑ column reports the rescaled mean silhouette "
                                "width so you can audit the precise values."
                            ),
                        ]
                    ),
                ]
            ),
        ],
        className="mb-0",
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
