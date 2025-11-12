# get_PLSDAbatch_data.R
# Outputs:
#   1) raw_plada.csv            -> samples x OTUs, numeric only, NO row/col names
#   2) metadata_PLSDAbatch.csv  -> CSV with headers: batch_id (Batch 1..), phenotype (0/1)

quiet_install_cran <- function(pkgs) {
  missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
}
quiet_install_bioc <- function(pkgs) {
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager", repos = "https://cloud.r-project.org")
  missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing)) BiocManager::install(missing, ask = FALSE, update = FALSE)
}

# Minimal dependencies
quiet_install_bioc(c("PLSDAbatch", "SummarizedExperiment"))

suppressPackageStartupMessages({
  library(PLSDAbatch)
  library(SummarizedExperiment)
})

# --- Load vignette dataset (AD_data) ---
data("AD_data", package = "PLSDAbatch")

# Count matrix and sample metadata
ad.count    <- SummarizedExperiment::assays(AD_data$FullData)[["Count"]]
ad.metadata <- SummarizedExperiment::rowData(AD_data$FullData)

# Build batch and treatment per vignette
ad.batch <- factor(ad.metadata$sequencing_run_date,
                   levels = unique(ad.metadata$sequencing_run_date))
ad.trt   <- as.factor(ad.metadata$initial_phenol_concentration.regroup)

# --- Detect orientation and align sample order ---
sample_ids <- rownames(ad.metadata)
rn <- rownames(ad.count); cn <- colnames(ad.count)

if (!is.null(rn) && all(sample_ids %in% rn)) {
  sample_in_rows <- TRUE
  ad.count <- ad.count[sample_ids, , drop = FALSE]  # reorder rows to metadata order
} else if (!is.null(cn) && all(sample_ids %in% cn)) {
  sample_in_rows <- FALSE
  ad.count <- ad.count[, sample_ids, drop = FALSE]  # reorder cols to metadata order
} else {
  stop("Could not match sample IDs from metadata to either rownames or colnames of the count matrix.")
}

# --- (Optional) vignette-style prefilter to remove low-prevalence OTUs ---
ad.filter.res <- PLSDAbatch::PreFL(data = ad.count)
ad.filter <- ad.filter.res$data.filter  # orientation preserved
# If you prefer raw counts without prefilter, replace 'ad.filter' with 'ad.count' below.

# --- Strict binary phenotype (0/1): two unique values or logical ---
to_binary01 <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  if (is.numeric(x)) { ux <- sort(unique(x)); if (length(ux) == 2L) return(match(x, ux) - 1L) }
  if (is.factor(x) || is.character(x)) { f <- factor(x); if (nlevels(f) == 2L) return(as.integer(f) - 1L) }
  stop("Phenotype must have exactly two unique values (or be logical).")
}
phenotype01 <- to_binary01(ad.trt)

# --- Build matrix: ensure samples x OTUs, numeric only, no headers ---
if (sample_in_rows) {
  M <- as.matrix(ad.filter)       # already samples x OTUs
  row_order <- rownames(M)
} else {
  M <- t(as.matrix(ad.filter))    # transpose to samples x OTUs
  row_order <- rownames(M)
}
storage.mode(M) <- "double"

# --- Recode batch to 'Batch 1..' (any unique mapping is fine) ---
orig_batch_vec <- as.character(ad.batch)
batch_levels   <- unique(orig_batch_vec)                 # order of first appearance
batch_map_vec  <- setNames(paste0("Batch ", seq_along(batch_levels)), batch_levels)
batch_id_vec   <- unname(batch_map_vec[orig_batch_vec])

# --- Build metadata and align to matrix rows ---
metadata <- data.frame(
  batch_id  = batch_id_vec,
  phenotype = phenotype01,
  stringsAsFactors = FALSE
)

idx <- match(row_order, sample_ids)
if (any(is.na(idx))) stop("Row names of the matrix do not match sample IDs from metadata.")
metadata <- metadata[idx, , drop = FALSE]

# Final checks
if (nrow(M) != nrow(metadata)) stop("Matrix rows and metadata rows disagree.")

# --- Write files ---
matrix_path   <- "raw_plada.csv"
metadata_path <- "metadata_PLSDAbatch.csv"

# Matrix: NO headers
write.table(M, file = matrix_path, sep = ",", row.names = FALSE, col.names = FALSE)

# Metadata: with headers
write.csv(metadata, file = metadata_path, row.names = FALSE, quote = TRUE)

cat("Done.\n",
    " - ", matrix_path,   " (samples x OTUs, no headers)\n",
    " - ", metadata_path, " (batch_id=Batch 1.., phenotype=0/1)\n",
    sep = "")
