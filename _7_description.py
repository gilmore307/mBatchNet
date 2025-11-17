"""Centralised text content for help documentation and tooltips."""
from typing import Dict, List

# Placeholder for future help modal sections. Populate this list with Dash components
# when help documentation is ready to be displayed in the modal.
HELP_MODAL_SECTIONS: List = []

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
