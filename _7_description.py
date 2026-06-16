"""Centralised text content for help documentation and tooltips."""
from typing import Dict, List

from dash import html

from _2_utils import CODE_TO_DISPLAY, METHOD_REFERENCE_BY_CODE, SUPPORTED_METHODS
from _6_correction import (
    _PARAMETER_CONFIG,
)


def _format_default_value(value: object) -> str:
    if value is True:
        return "True"
    if value is False:
        return "False"
    if value is None:
        return "not set"
    return str(value)


def _method_help_cards() -> List:
    cards: List = []
    for code, display in SUPPORTED_METHODS:
        metadata = METHOD_REFERENCE_BY_CODE.get(code, {})
        description = (metadata.get("description") or "").strip()
        package_url = (metadata.get("package") or "").strip()
        reference_url = (metadata.get("url") or "").strip()
        params = _PARAMETER_CONFIG.get(code, [])
        parameter_items = []
        if params:
            for spec in params:
                parameter_items.append(
                    html.Li(
                        [
                            html.Strong(str(spec.get("name") or "parameter")),
                            f" (default: {_format_default_value(spec.get('default'))}) - ",
                            str(spec.get("description") or "No parameter description available."),
                        ]
                    )
                )
        else:
            parameter_items.append(
                html.Li(
                    "No method-specific parameters are exposed for this method. The run uses the uploaded matrix, metadata mapping, additional metadata covariates, and study settings from the session."
                )
            )

        links = []
        if package_url:
            links.append(
                html.A(
                    "Package/source",
                    href=package_url,
                    target="_blank",
                    rel="noopener noreferrer",
                    className="me-3",
                )
            )
        if reference_url:
            links.append(
                html.A(
                    "Reference",
                    href=reference_url,
                    target="_blank",
                    rel="noopener noreferrer",
                )
            )
        cards.append(
            html.Details(
                [
                    html.Summary(CODE_TO_DISPLAY.get(code, display), className="fw-semibold"),
                    html.P(description or "No method description available.", className="mt-2 mb-2"),
                    html.Div(links, className="mb-2") if links else html.Div(),
                    html.Div("Exposed parameters", className="fw-semibold mb-1"),
                    html.Ul(parameter_items, className="mb-0"),
                ],
                className="border rounded p-3 mb-2",
            )
        )
    return cards


HELP_SECTION_TOC: List[Dict[str, str]] = [
    {"id": "help-overview", "title": "Overview"},
    {
        "id": "help-upload",
        "title": "Upload Files",
        "children": [
            {"id": "help-upload-manual", "title": "Manual upload"},
            {"id": "help-upload-validation", "title": "Preprocess validation"},
            {"id": "help-upload-example", "title": "Example dataset"},
            {"id": "help-upload-repro", "title": "Repro bundle"},
            {"id": "help-upload-mosaic", "title": "Mosaic plot"},
        ],
    },
    {
        "id": "help-correction",
        "title": "Batch Effect Correction",
        "children": [
            {"id": "help-correction-table", "title": "Table columns"},
            {"id": "help-correction-run", "title": "Running corrections"},
            {"id": "help-correction-methods", "title": "Methods and parameters"},
        ],
    },
    {
        "id": "help-assessment",
        "title": "Assessment (Pre & Post)",
        "children": [
            {"id": "help-assessment-alignment", "title": "Alignment score"},
            {"id": "help-assessment-pca", "title": "PCA ordination"},
            {"id": "help-assessment-pcoa", "title": "PCoA ordination"},
            {"id": "help-assessment-nmds", "title": "NMDS ordination"},
            {"id": "help-assessment-heatmap", "title": "Dissimilarity heatmaps"},
            {"id": "help-assessment-permanova", "title": "PERMANOVA"},
            {"id": "help-assessment-anova", "title": "Per-feature ANOVA"},
            {"id": "help-assessment-rda", "title": "Partial RDA"},
            {"id": "help-assessment-pvca", "title": "PVCA"},
            {"id": "help-assessment-ebm", "title": "Entropy batch mixing"},
            {"id": "help-assessment-silhouette", "title": "UMAP silhouette"},
        ],
    },
]


HELP_MODAL_SECTIONS: List = [
    html.Div(
        [
            html.H4("Overview"),
            html.P(
                "The navigation bar mirrors the workflow: upload or restore inputs, preview pre-correction diagnostics, launch batch-effect correction methods, and then compare the post-correction assessments before downloading outputs or a Repro bundle. "
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
                "Upload a processed microbiome feature table with a matching metadata table, pick one of the curated examples, or restore a session from a Repro bundle."
            ),
            html.Div(
                [
                    html.H5("Manual upload", className="mb-2 mt-3", id="help-upload-manual"),
                    html.Ul(
                        [
                            html.Li(
                                "Manual uploads accept CSV files."
                            ),
                            html.Li(
                                "The matrix must use samples as rows and profiled features as columns. Metadata must have one row per sample in the same order."
                            ),
                            html.Li(
                                "Raw sequencing files such as FASTQ are not accepted. Upload a processed sample-by-feature numeric table, such as a 16S-derived OTU/ASV table or a shotgun-derived taxonomic/functional profile after upstream profiling."
                            ),
                            html.Li(
                                "Metadata CSVs must include: a Batch column (batch IDs), a target column (target label such as phenotype/group), and optional covariate columns (keep the count modest to avoid slow runs)."
                            ),
                        ]
                    ),
                ]
            ),
            html.Div(
                [
                    html.H5("Preprocess validation", className="mb-2 mt-4", id="help-upload-validation"),
                    html.P(
                        "Before preprocessing, mBatchNet validates the upload contract and writes validation_report.json for the session."
                    ),
                    html.Ul(
                        [
                            html.Li(
                                "File and shape checks: required raw.csv and metadata_origin.csv files, public-server file-size limits, sample/feature/cell limits, at least two samples and two features, and matching metadata row count."
                            ),
                            html.Li(
                                "Hard blocks: missing required files, oversize files, invalid matrix shape, blank/NA/NaN/Inf/non-numeric matrix cells, all-zero sample rows, metadata row-count mismatch, missing selected metadata columns, invalid batch/target levels, or using the same column for batch and target."
                            ),
                            html.Li(
                                "Warnings: all-zero feature columns, high sparsity, negative/transformed-looking values, large files or matrices, strong batch-target association, outliers, and method-specific availability limits."
                            ),
                            html.Li(
                                "Method availability: count-based methods ConQuR, RUV-III-NB, ComBat-seq, and DEBIAS-M require nonnegative integer count input. Continuous, transformed, negative, or otherwise non-count matrices disable these methods in the Correction table while leaving continuous-compatible methods available."
                            ),
                            html.Li(
                                "FAbatch availability is checked after low-variance filtering; FAbatch is disabled when retained features are not greater than the largest batch size."
                            ),
                            html.Li(
                                "Study-design warnings: strong batch-target association is reported when Cramer's V >= 0.60."
                            ),
                            html.Li(
                                [
                                    "Outlier screening uses Scanpy's ",
                                    html.A(
                                        "sc.pp.calculate_qc_metrics",
                                        href="https://scanpy.readthedocs.io/en/stable/api/scanpy.pp.calculate_qc_metrics.html",
                                        target="_blank",
                                        rel="noopener noreferrer",
                                    ),
                                    " to compute sample-level QC totals, then mBatchNet applies a 5x MAD screening rule to sample totals and matrix values. This produces an advisory warning only; it does not delete samples or alter the matrix.",
                                ]
                            ),
                        ]
                    ),
                ]
            ),
            html.Div(
                [
                    html.H5("Example dataset", className="mb-2 mt-4", id="help-upload-example"),
                    html.Ul(
                        [
                            html.Li(
                                "Example datasets auto-populate every mapping field with that dataset's Batch, target, and covariate selections."
                            ),
                            html.Li(
                                "Use the Example Dataset tab to preview rows and columns before loading the included AD example into the session."
                            ),
                        ]
                    ),
                ]
            ),
            html.Div(
                [
                    html.H5("Repro bundle", className="mb-2 mt-4", id="help-upload-repro"),
                    html.Ul(
                        [
                            html.Li(
                                "Upload a reproducibility_bundle.zip exported from mBatchNet."
                            ),
                            html.Li(
                                "A restored bundle loads the saved input files, mapping settings, parameters, manifests, and run log into the current session."
                            ),
                            html.Li(
                                "If preprocessed files are present, the restored session can continue into correction and assessment without re-running preprocessing first."
                            ),
                        ]
                    ),
                ]
            ),
            html.Div(
                [
                    html.H5("Mosaic plot", className="mb-2 mt-4", id="help-upload-mosaic"),
                    html.P(
                        "The mosaic plot cross-tabulates the Batch and target mappings after metadata "
                        "mapping and study-setting confirmation, showing how many samples fall into "
                        "each batch/label combination before running corrections. It acts as a quick "
                        "imbalance diagnostic before lengthy methods."
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
            html.Div(
                [
                    html.H5("Table columns", className="mb-2 mt-3", id="help-correction-table"),
                    html.Ul(
                        [
                            html.Li(
                                "Config opens method-specific parameter controls. Saved values are "
                                "recorded in parameter_manifest.json."
                            ),
                            html.Li(
                                "Explanation opens the citation-derived method description, package "
                                "or source link, citation, and reference link."
                            ),
                            html.Li(
                                "Methods lists each available algorithm and opens its package or source reference when one is available."
                            ),
                            html.Li(
                                "Expected time (s) shows reference elapsed seconds from prior successful runs on this server, excluding the current session's completed run."
                            ),
                            html.Li(
                                "Time (s) shows how long each method took to run in the current session "
                                "from the run log or session_summary.json."
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
                        ],
                        className="mt-2",
                    ),
                ]
            ),
            html.Div(
                [
                    html.H5("Running corrections", className="mb-2 mt-4", id="help-correction-run"),
                    html.Ul(
                        [
                            html.Li(
                                "Jobs run sequentially and write status updates to the run log. "
                                "When a method finishes, open the log modal to review the process."
                            ),
                            html.Li(
                                "Once a method completes, its corrected matrix is stored for the "
                                "post-correction assessments. Use Download outputs to export corrected "
                                "matrices, figures, logs, and manifests. Use Repro bundle to export "
                                "the files needed to restore the session later."
                            ),
                        ]
                    ),
                ]
            ),
            html.Div(
                [
                    html.H5("Methods and parameters", className="mb-2 mt-4", id="help-correction-methods"),
                    html.P(
                        "These entries mirror the method descriptions and exposed parameter controls in the Correction table. Method descriptions are drawn from the official method files, linked package/source documentation, and citation records used by mBatchNet; parameter descriptions reflect the current wrapper arguments exposed by the app."
                    ),
                    html.Div(_method_help_cards()),
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
                        "Use the detail table to inspect the current stage's raw assessment CSV values across methods and geometries."
                    ),
                    html.Li(
                        "Click any detail table column title to filter and sort methods "
                        "on the fly."
                    ),
                    html.Li(
                        "Use Download outputs from the navbar to export corrected matrices, "
                        "assessment figures, logs, and manifests. Use Repro bundle to export the "
                        "session files needed for restore."
                    ),
                ]
            ),
            html.Div(
                [
                    html.H5("Alignment score", className="mb-2 mt-4", id="help-assessment-alignment"),
                    html.Img(
                        src="/assets/Figure/alignment_score.png",
                        style={
                            "maxWidth": "100%",
                            "height": "auto",
                            "display": "block",
                            "margin": "10px 0 15px 0",
                        },
                    ),
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
                    html.H5("PCA ordination", className="mb-2 mt-4", id="help-assessment-pca"),
                    html.Img(
                        src="/assets/Figure/pca_batch.png",
                        style={
                            "maxWidth": "100%",
                            "height": "auto",
                            "display": "block",
                            "margin": "10px 0 15px 0",
                        },
                    ),
                    html.Ul(
                        [
                            html.Li(
                                "Purpose: Visualises global variance structure to check whether "
                                "batch dominates the first two PCs or if the target label remains "
                                "separable."
                            ),
                            html.Li(
                                "Figure: Each panel plots PC1 vs PC2 with ellipses around batch and "
                                "target groups. The panels show batch-to-batch centroid distances, "
                                "ellipse overlap, and target-label ordering after correction."
                            ),
                            html.Li(
                                "Centroid Distance: Captures between-group spacing; smaller batch "
                                "distances imply tighter batch mixing while larger target distances "
                                "signal preserved biology."
                            ),
                            html.Li(
                                "Ellipse Overlap: Summarises how much batch or target clusters "
                                "overlap."
                            ),
                            html.Li(
                                "Target vs Batch Centroid Delta: Highlights how far apart target "
                                "centroids remain relative to batch centroids."
                            ),
                        ]
                    ),
                ]
            ),
            html.Div(
                [
                    html.H5("PCoA ordination", className="mb-2 mt-4", id="help-assessment-pcoa"),
                    html.Img(
                        src="/assets/Figure/pcoa_aitchison_batch.png",
                        style={
                            "maxWidth": "100%",
                            "height": "auto",
                            "display": "block",
                            "margin": "10px 0 15px 0",
                        },
                    ),
                    html.Ul(
                        [
                            html.Li(
                                "Purpose: Performs Principal Coordinates Analysis in CLR and TSS "
                                "geometries to reveal geometry-specific batch structure, especially "
                                "when Euclidean PCA misses compositional trends."
                            ),
                            html.Li(
                                "Figure: Use the Aitchison vs Bray-Curtis sub-tabs to see how "
                                "batches overlap on each geometry. The axes can change order when "
                                "corrections change the distance rankings."
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
                    html.H5("NMDS ordination", className="mb-2 mt-4", id="help-assessment-nmds"),
                    html.Img(
                        src="/assets/Figure/nmds_aitchison_batch.png",
                        style={
                            "maxWidth": "100%",
                            "height": "auto",
                            "display": "block",
                            "margin": "10px 0 15px 0",
                        },
                    ),
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
                    html.H5("Dissimilarity heatmaps", className="mb-2 mt-4", id="help-assessment-heatmap"),
                    html.Img(
                        src="/assets/Figure/dissimilarity_heatmaps_aitchison.png",
                        style={
                            "maxWidth": "100%",
                            "height": "auto",
                            "display": "block",
                            "margin": "10px 0 15px 0",
                        },
                    ),
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
                                "effects. ANOSIM p records the companion permutation p-value in the raw assessment CSV."
                            ),
                        ]
                    ),
                ]
            ),
            html.Div(
                [
                    html.H5("PERMANOVA", className="mb-2 mt-4", id="help-assessment-permanova"),
                    html.Img(
                        src="/assets/Figure/permanova_aitchison.png",
                        style={
                            "maxWidth": "100%",
                            "height": "auto",
                            "display": "block",
                            "margin": "10px 0 15px 0",
                        },
                    ),
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
                                "Values below ~0.05 typically indicate minimal batch impact. The raw assessment CSV also includes a permutation p-value column; invalid or underspecified method/geometry combinations are recorded as NA rather than failing the whole assessment."
                            ),
                        ]
                    ),
                ]
            ),
            html.Div(
                [
                    html.H5("Per-feature ANOVA (Median R²)", className="mb-2 mt-4", id="help-assessment-anova"),
                    html.Img(
                        src="/assets/Figure/anova_aitchison.png",
                        style={
                            "maxWidth": "100%",
                            "height": "auto",
                            "display": "block",
                            "margin": "10px 0 15px 0",
                        },
                    ),
                    html.Ul(
                        [
                            html.Li(
                                "Purpose: Summarises how much of each feature's variance is explained "
                                "by batch vs. target using per-feature ANOVA."
                            ),
                            html.Li(
                                "Figure: Dual bars show the competing batch and target medians per "
                                "method."
                            ),
                            html.Li(
                                "Median R² (Batch): Reports how much feature-level variance is "
                                "explained by batch."
                            ),
                            html.Li(
                                "Median R² (Target): Higher values indicate preserved biological "
                                "signal across features."
                            ),
                        ]
                    ),
                ]
            ),
            html.Div(
                [
                    html.H5("Partial RDA", className="mb-2 mt-4", id="help-assessment-rda"),
                    html.Img(
                        src="/assets/Figure/pRDA_aitchison.png",
                        style={
                            "maxWidth": "100%",
                            "height": "auto",
                            "display": "block",
                            "margin": "10px 0 15px 0",
                        },
                    ),
                    html.Ul(
                        [
                            html.Li(
                                "Purpose: Decomposes constrained variance into target, batch, and "
                                "shared components. The analysis partials out covariates so changes "
                                "can be attributed to the chosen label."
                            ),
                            html.Li(
                                "Figure: Stacked bars show how much partial RDA variance is assigned "
                                "to each component across target, batch, intersection, and residual terms."
                            ),
                            html.Li(
                                "Target variance: Shows the fraction of constrained variance "
                                "attributed to the target label."
                            ),
                            html.Li(
                                "Batch variance: Reports how much variance batch still explains. "
                                "Lower fractions indicate a smaller batch-associated component."
                            ),
                            html.Li(
                                "Intersection variance: Captures overlap between batch and target "
                                "effects; large intersections imply residual confounding."
                            ),
                            html.Li(
                                "Residual variance: Reflects unexplained variance so you can assess "
                                "model fit when target and batch effects are small."
                            ),
                        ]
                    ),
                ]
            ),
            html.Div(
                [
                    html.H5("PVCA", className="mb-2 mt-4", id="help-assessment-pvca"),
                    html.Img(
                        src="/assets/Figure/PVCA.png",
                        style={
                            "maxWidth": "100%",
                            "height": "auto",
                            "display": "block",
                            "margin": "10px 0 15px 0",
                        },
                    ),
                    html.Ul(
                        [
                            html.Li(
                                "Purpose: Applies Principal Variance Component Analysis to apportion "
                                "variance across batch, target, and residual sources."
                            ),
                            html.Li(
                                "Figure: Similar to partial RDA, stacked bars reveal whether batch "
                                "variance shrinks relative to target across corrected matrices."
                            ),
                            html.Li(
                                "Target variance: PVCA-estimated fraction attributed to target."
                            ),
                            html.Li(
                                "Batch variance: PVCA-estimated batch contribution after correction."
                            ),
                            html.Li(
                                "Intersection variance: PVCA overlap between batch and target "
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
                    html.H5("Entropy batch mixing (EBM)", className="mb-2 mt-4", id="help-assessment-ebm"),
                    html.Img(
                        src="/assets/Figure/ebm.png",
                        style={
                            "maxWidth": "100%",
                            "height": "auto",
                            "display": "block",
                            "margin": "10px 0 15px 0",
                        },
                    ),
                    html.Ul(
                        [
                            html.Li(
                                "Purpose: Measures k-NN batch entropy within UMAP embeddings (higher "
                                "entropy = more even local batch composition)."
                            ),
                            html.Li(
                                "EBM column: Exposes the average entropy value across anchor pools "
                                "used for ranking."
                            ),
                        ]
                    ),
                ]
            ),
            html.Div(
                [
                    html.H5("UMAP silhouette", className="mb-2 mt-4", id="help-assessment-silhouette"),
                    html.Img(
                        src="/assets/Figure/silhouette.png",
                        style={
                            "maxWidth": "100%",
                            "height": "auto",
                            "display": "block",
                            "margin": "10px 0 15px 0",
                        },
                    ),
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
        "**Symbols:** $\\tilde{R}^2_{\\text{target}}$ = median per-feature ANOVA $R^2$ for the target effect; $\\tilde{R}^2_{\\text{batch}}$ = median $R^2$ for the batch effect.\n\n"
        "Uses median per-feature ANOVA $R^2$ to encourage target signal and suppress batch signal."
    ),
    "prda": (
        "**Score formula:** $S = \\big(S_{\\text{CLR}} S_{\\text{TSS}}\\big)^{1/2}$, $S_{\\text{geom}} = \\frac{T}{T + B}$\n\n"
        "**Symbols:** $T$ = fraction of variance attributed to target; $B$ = fraction attributed to batch in the partial RDA decomposition.\n\n"
        "Compares target vs batch variance fractions from partial RDA."
    ),
    "pvca": (
        "**Score formula:** $S = \\big(S_{\\text{CLR}} S_{\\text{TSS}}\\big)^{1/2}$, $S_{\\text{geom}} = \\frac{T}{T + B}$\n\n"
        "**Symbols:** $T$ = PVCA-estimated target variance fraction; $B$ = PVCA-estimated batch variance fraction.\n\nLeverages PVCA variance components to favour low batch contribution."
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
