"""Centralised text content for help documentation and tooltips."""
from typing import Dict, List

# Placeholder for future help modal sections. Populate this list with Dash components
# when help documentation is ready to be displayed in the modal.
HELP_MODAL_SECTIONS: List = []

# Parameter tooltips for assessment configuration controls.
ASSESSMENT_PARAM_TOOLTIPS: Dict[str, Dict[str, str]] = {
    "auc": {
        "cv_folds": "Number of cross-validation folds (repeated CV).",
        "cv_reps": "Number of repetitions for repeated cross-validation.",
    },
    "alignment": {
        "k_neighbors": "k for k-NN graph in PCA space.",
        "var_prop_min": "Min cumulative variance for PCA retention (0–1).",
        "max_pcs": "Maximum number of principal components to use.",
    },
    "ebm": {
        "umap_neighbors": "UMAP: Number of neighbors (local connectivity).",
        "umap_min_dist": "UMAP: Minimum distance between points in embedding.",
        "umap_metric": "UMAP distance metric (CLR uses Euclidean).",
        "knn_k": "k for entropy mixing (neighbors per anchor).",
        "knn_pools": "Number of anchor pools to average entropy over.",
        "knn_per_label": "Anchors sampled per batch label per pool.",
    },
    "lisi": {
        "k_neighbors": "k for k-NN graph when computing LISI.",
        "n_pcs": "Number of PCs for LISI (set via coords=CLR to use none).",
        "coords": "Coordinate space for LISI: PCA or CLR.",
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
