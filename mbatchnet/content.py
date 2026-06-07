from __future__ import annotations

HELP_SECTIONS = [
    {
        "id": "overview",
        "title": "Overview",
        "body": (
            "mBatchNet follows one workflow: upload microbiome inputs, inspect "
            "pre-correction diagnostics, run selected correction methods, compare "
            "post-correction evidence, and download the result or reproducibility bundle."
        ),
        "bullets": [
            "Use the top navigation to move through the analysis stages.",
            "Open Logs during any long-running job to inspect the active command output.",
            "Use Download results for generated matrices, figures, logs, and summaries.",
            "Use Repro bundle for input files and run settings, without generated figures.",
        ],
    },
    {
        "id": "upload",
        "title": "Upload files",
        "body": (
            "Upload a feature-by-sample microbiome table and a matching metadata table, "
            "or load the bundled anaerobic digestion example."
        ),
        "bullets": [
            "Manual uploads accept CSV files.",
            "The count matrix should use samples as rows and microbial features as columns.",
            "Metadata must include a batch column and a target or phenotype column.",
            "Covariates may be supplied when they are part of the study design.",
            "The pre-check reports missing values, nonnumeric matrix entries, sample mismatches, and batch-target confounding warnings.",
        ],
        "image": "Figure/mosaic_plot.png",
        "image_caption": "The mosaic plot summarizes batch by target combinations before correction.",
    },
    {
        "id": "correction",
        "title": "Batch effect correction",
        "body": (
            "The correction page lists all compatible methods, their current session "
            "status, configurable parameters, and source references."
        ),
        "bullets": [
            "Run Correction queues a method for the current session.",
            "Status indicates whether that method already has a corrected matrix saved.",
            "Delete method outputs removes only that method's stored outputs so it can be rerun.",
            "Method-specific parameter controls use conservative defaults and can be adjusted for study-specific needs.",
        ],
    },
    {
        "id": "assessment",
        "title": "Assessment and interpretation",
        "body": (
            "Pre- and post-correction assessment pages share the same diagnostic families. "
            "The goal is to reduce batch-associated structure while preserving meaningful target signal."
        ),
        "bullets": [
            "Ordination diagnostics show global sample structure.",
            "Distance-based tests quantify batch or target association.",
            "Variance decomposition separates batch, target, shared, and residual components.",
            "Neighborhood metrics evaluate local batch mixing and target preservation after correction.",
        ],
    },
]


UPLOAD_GUIDANCE = {
    "manual": [
        "Use clean microbiome count or abundance data. mBatchNet focuses on correction, not full raw-data curation.",
        "Keep sample identifiers consistent between matrix columns and metadata rows.",
        "Do not rely on the server to impute missing study labels. Missing labels are reported as validation problems.",
        "Batch-target confounding is reported as a warning instead of a hard block because some users still need to inspect the trade-off.",
    ],
    "example": [
        "The bundled anaerobic digestion dataset is provided as a fast walkthrough.",
        "Batch, target, and covariate mappings are detected automatically for the example session.",
        "Use the example to inspect the workflow before uploading a study-specific dataset.",
    ],
}


ASSESSMENT_OVERVIEW = [
    "PCA, PCoA, and NMDS reveal whether batch labels dominate global sample structure.",
    "ANOSIM and PERMANOVA summarize distance-based batch association with effect sizes and p-values when available.",
    "Feature-wise ANOVA, pRDA, and PVCA report how much variation is attributed to batch, target, their overlap, and residual structure.",
    "Alignment, entropy batch mixing, and silhouette summaries help compare post-correction local mixing and target preservation.",
]


METRIC_GUIDES = {
    "pca": {
        "title": "PCA ordination",
        "image": "Figure/pca_batch.png",
        "summary": "Principal component analysis visualizes major linear variance directions.",
        "points": [
            "Strong batch separation on early PCs suggests technical structure before correction.",
            "After correction, batch centroids should move closer while target structure should remain interpretable.",
            "Centroid distance, ellipse overlap, and target-vs-batch deltas make the visual comparison explicit.",
        ],
    },
    "pcoa": {
        "title": "PCoA ordination",
        "image": "Figure/pcoa_aitchison_batch.png",
        "summary": "PCoA evaluates sample relationships in Aitchison and Bray-Curtis spaces.",
        "points": [
            "Aitchison distance is Euclidean distance on CLR-transformed compositional profiles.",
            "Bray-Curtis is applied to relative abundance profiles to inspect abundance-weighted dissimilarity.",
            "Comparing both spaces helps detect corrections that work in one geometry but not another.",
        ],
    },
    "nmds": {
        "title": "NMDS ordination",
        "image": "Figure/nmds_aitchison_batch.png",
        "summary": "NMDS uses ranked dissimilarities to inspect nonlinear sample structure.",
        "points": [
            "Lower stress means the two-dimensional embedding better preserves pairwise dissimilarity ranks.",
            "Residual batch islands after correction indicate incomplete technical mixing.",
            "Interpret NMDS together with effect-size tests rather than as a standalone score.",
        ],
    },
    "dissimilarity": {
        "title": "Dissimilarity heatmaps",
        "image": "Figure/dissimilarity_heatmaps_aitchison.png",
        "summary": "Heatmaps expose within- and between-batch distance patterns.",
        "points": [
            "Block-like same-batch structure suggests residual batch effects.",
            "ANOSIM R near zero indicates weak batch-driven distance separation.",
            "Use the p-value when available to distinguish visible structure from noise.",
        ],
    },
    "permanova": {
        "title": "PERMANOVA",
        "image": "Figure/permanova_aitchison.png",
        "summary": "PERMANOVA estimates how much distance-matrix variation is explained by study factors.",
        "points": [
            "Batch R2 should decrease after correction.",
            "Target R2 should not collapse if biological structure is preserved.",
            "P-values are useful context, but effect size is the main comparison target across methods.",
        ],
    },
    "r2": {
        "title": "Feature-wise ANOVA R2",
        "image": "Figure/anova_aitchison.png",
        "summary": "Per-feature ANOVA summarizes batch and target variance across features.",
        "points": [
            "Median batch R2 should shrink after correction.",
            "Median target R2 helps detect overcorrection.",
            "This metric complements ordination because it works feature by feature.",
        ],
    },
    "prda": {
        "title": "Partial RDA",
        "image": "Figure/pRDA_aitchison.png",
        "summary": "Partial RDA decomposes constrained variance into target, batch, shared, and residual components.",
        "points": [
            "Large shared variance can indicate batch-target confounding.",
            "Good correction reduces batch contribution without erasing target contribution.",
            "Residual variance provides context for how much structure remains unexplained.",
        ],
    },
    "pvca": {
        "title": "PVCA",
        "image": "Figure/PVCA.png",
        "summary": "PVCA estimates variance components after principal-component reduction.",
        "points": [
            "Batch contribution should decline after correction.",
            "Target contribution should remain visible when phenotype signal is present.",
            "Intersection terms warn that batch and target cannot be cleanly separated by the design alone.",
        ],
    },
    "alignment": {
        "title": "Alignment score",
        "image": "Figure/alignment_score.png",
        "summary": "Alignment score quantifies local cross-batch mixing in nearest-neighbor space.",
        "points": [
            "Higher scores indicate stronger batch mixing after correction.",
            "Use it with target-preservation metrics to avoid choosing an overcorrected result.",
            "The score is most useful for comparing multiple corrected matrices from the same session.",
        ],
    },
    "ebm": {
        "title": "Entropy batch mixing",
        "image": "Figure/ebm.png",
        "summary": "EBM measures neighborhood-level entropy of batch labels.",
        "points": [
            "Higher entropy means local neighborhoods contain a broader mix of batches.",
            "Low entropy after correction suggests persistent local batch structure.",
            "Interpret with UMAP settings because neighborhood size changes the sensitivity.",
        ],
    },
    "silhouette": {
        "title": "UMAP silhouette",
        "image": "Figure/silhouette.png",
        "summary": "Silhouette summaries help check whether target labels remain coherent after correction.",
        "points": [
            "Higher target silhouette means target groups remain better separated.",
            "A sharp drop can indicate overcorrection or weak phenotype signal.",
            "Use together with batch-mixing scores to balance both goals.",
        ],
    },
}


METHOD_GUIDANCE = {
    "ConQuR": {
        "scenario": "Microbiome count data where distribution-aware quantile regression is appropriate.",
        "notes": [
            "Can be computationally intensive on larger studies.",
            "Most useful when batch effects vary across the abundance distribution.",
        ],
    },
    "MMUPHin": {
        "scenario": "Microbiome meta-analysis or multi-study settings with microbial feature profiles.",
        "notes": [
            "Designed for microbiome-oriented batch adjustment and association workflows.",
            "Works best when metadata design is explicit and sample groups are not severely confounded.",
        ],
    },
    "RUV": {
        "scenario": "Count data where unwanted variation can be estimated with latent factors.",
        "notes": [
            "The k parameter controls the number of unwanted-variation factors.",
            "Too large a k may remove target signal in small or confounded datasets.",
        ],
    },
    "MetaDICT": {
        "scenario": "Microbiome-oriented correction where upper-quartile style normalization and model tuning are desired.",
        "notes": [
            "Parameter choices affect the balance between batch attenuation and phenotype preservation.",
            "Use post-correction diagnostics to compare against simpler methods.",
        ],
    },
    "DEBIAS": {
        "scenario": "Microbiome-specific deep-learning correction when its dependency stack is available.",
        "notes": [
            "Requires the optional DEBIAS-M Python dependency stack.",
            "Use with care on small datasets because model flexibility can exceed available signal.",
        ],
    },
    "PLSDA": {
        "scenario": "Supervised correction where target and batch components are modeled explicitly.",
        "notes": [
            "Component and keepX settings influence how much structure is preserved.",
            "Confounded designs require careful interpretation.",
        ],
    },
    "ComBat": {
        "scenario": "General empirical-Bayes batch correction for continuous transformed profiles.",
        "notes": [
            "Common baseline for transformed abundance matrices.",
            "Not microbiome-specific, so compositional diagnostics remain important.",
        ],
    },
    "ComBatSeq": {
        "scenario": "Count-oriented ComBat variant for sequencing count data.",
        "notes": [
            "Useful when count scale should be retained.",
            "Compare against microbiome-specific methods when zero inflation is prominent.",
        ],
    },
    "limma": {
        "scenario": "General linear-model batch adjustment after suitable transformation.",
        "notes": [
            "Fast and useful as a baseline.",
            "Assumptions are simpler than microbiome-specific correction methods.",
        ],
    },
    "BMC": {
        "scenario": "General correction baseline for matrix-level batch adjustment.",
        "notes": [
            "Useful for comparison when method assumptions are uncertain.",
            "Review post-correction target preservation before selecting it.",
        ],
    },
    "FSQN": {
        "scenario": "Quantile-normalization style correction when feature-wise distribution alignment is desired.",
        "notes": [
            "Can change marginal feature distributions substantially.",
            "Check both batch attenuation and target preservation.",
        ],
    },
    "FAbatch": {
        "scenario": "Factor-analysis based correction for latent batch-associated structure.",
        "notes": [
            "Maximum factor settings affect runtime and correction strength.",
            "Use variance-decomposition diagnostics to detect overcorrection.",
        ],
    },
}
