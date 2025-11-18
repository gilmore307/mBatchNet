# get_PLSDAbatch_data.R
# Outputs:
#   1) raw_plada.csv           -> samples x OTUs, numeric only, NO row/col names
#   2) metadata_PLSDAbatch.csv -> CSV with headers:
#        - batch (Batch 1..)
#        - initial_phenol_concentration.regroup (original phenotype values)
#        - filtered covariates (see rules in filter_covariates)

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

# ---------- Helpers ----------

# Recode batches (character vector) to "Batch 1.."
recode_batches <- function(batch_chr) {
  lv <- unique(batch_chr)
  mp <- setNames(paste0("Batch ", seq_along(lv)), lv)
  unname(mp[batch_chr])
}

# Covariate filtering:
#   - drop column if any NA / empty string
#   - drop if ≤1 unique value
#   - drop if determined by batch (one value per batch)
#   - drop if determined by phenotype (one value per phenotype level)
#   - drop if character/factor and unique values > 2
filter_covariates <- function(covars, batch, phenotype, dataset_label = "") {
  if (!ncol(covars)) return(covars)
  
  keep <- rep(TRUE, ncol(covars))
  names(keep) <- colnames(covars)
  
  for (j in seq_along(covars)) {
    v  <- covars[[j]]
    nm <- colnames(covars)[j]
    
    drop_reason <- NULL
    
    is_text <- is.character(v) || is.factor(v)
    if (is_text) {
      v_chr <- trimws(as.character(v))
    } else {
      v_chr <- NULL
    }
    
    # missing = NA or empty string
    if (is_text) {
      missing <- is.na(v_chr) | v_chr == ""
    } else {
      missing <- is.na(v)
    }
    
    # 1) any NA / empty -> drop
    if (any(missing)) {
      drop_reason <- sprintf("has %d NA/empty values", sum(missing))
    } else {
      # no missing now
      if (is_text) {
        v_use <- v_chr
      } else {
        v_use <- v
      }
      nunique <- length(unique(v_use))
      
      # 2) only 0–1 unique value
      if (nunique <= 1) {
        drop_reason <- "only 0–1 unique value"
      }
      
      # 3) determined by batch (one value per batch)
      if (is.null(drop_reason) && !is.null(batch) && length(batch) == length(v)) {
        b  <- batch
        ok <- !is.na(b)
        if (any(ok)) {
          b_use <- b[ok]
          x_use <- if (is_text) v_chr[ok] else v[ok]
          per_batch_unique <- tapply(x_use, b_use, function(z) length(unique(z)))
          if (all(per_batch_unique <= 1)) {
            drop_reason <- "determined by batch (one value per batch)"
          }
        }
      }
      
      # 4) determined by phenotype (one value per phenotype level)
      if (is.null(drop_reason) && !is.null(phenotype) && length(phenotype) == length(v)) {
        p  <- phenotype
        ok <- !is.na(p)
        if (any(ok)) {
          p_use <- p[ok]
          x_use <- if (is_text) v_chr[ok] else v[ok]
          per_pheno_unique <- tapply(x_use, p_use, function(z) length(unique(z)))
          if (all(per_pheno_unique <= 1)) {
            drop_reason <- "determined by phenotype (one value per phenotype level)"
          }
        }
      }
      
      # 5) text/factor with >2 unique values
      if (is.null(drop_reason) && is_text && nunique > 2) {
        drop_reason <- sprintf("categorical text with %d unique values (>2)", nunique)
      }
    }
    
    if (!is.null(drop_reason)) {
      keep[j] <- FALSE
      message(sprintf("[PLSDAbatch] Dropping covariate '%s': %s", nm, drop_reason))
    }
  }
  
  covars[, keep, drop = FALSE]
}

# ---------- Load vignette dataset (AD_data) ----------

data("AD_data", package = "PLSDAbatch")

# Count matrix and sample metadata
ad.count    <- SummarizedExperiment::assays(AD_data$FullData)[["Count"]]
ad.metadata <- SummarizedExperiment::rowData(AD_data$FullData)
ad.metadata <- as.data.frame(ad.metadata)

# Build batch and treatment per vignette
ad.batch <- factor(
  ad.metadata$sequencing_run_date,
  levels = unique(ad.metadata$sequencing_run_date)
)
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

# --- Phenotype: original values and column name ---
phenotype <- ad.trt
message("[PLSDAbatch] phenotype source column : 'initial_phenol_concentration.regroup' (original values)")

# --- Batch: recode sequencing_run_date -> Batch 1.. ---
orig_batch_vec <- as.character(ad.batch)
batch_vec      <- recode_batches(orig_batch_vec)

message("[PLSDAbatch] batch source column    : 'sequencing_run_date' (re-coded to 'Batch 1..')")

# --- Covariates: all metadata columns minus ID / batch / phenotype ---
id_like_cols <- intersect(c("sample", "sample_id", "SampleID", "Sample_ID"), colnames(ad.metadata))

covar_raw <- ad.metadata[
  ,
  setdiff(
    colnames(ad.metadata),
    c(id_like_cols, "sequencing_run_date", "initial_phenol_concentration.regroup")
  ),
  drop = FALSE
]

covar_filtered <- filter_covariates(
  covars        = covar_raw,
  batch         = batch_vec,
  phenotype     = phenotype,
  dataset_label = "PLSDAbatch"
)

# --- Build matrix: ensure samples x OTUs, numeric only, no headers ---
if (sample_in_rows) {
  M <- as.matrix(ad.filter)       # already samples x OTUs
  row_order <- rownames(M)
} else {
  M <- t(as.matrix(ad.filter))    # transpose to samples x OTUs
  row_order <- rownames(M)
}
storage.mode(M) <- "double"

# --- Build metadata (batch + original phenotype + covariates) and align to matrix rows ---
metadata <- data.frame(
  batch = batch_vec,
  initial_phenol_concentration.regroup = phenotype,
  covar_filtered,
  stringsAsFactors = FALSE
)

idx <- match(row_order, sample_ids)
if (any(is.na(idx))) stop("Row names of the matrix do not match sample IDs from metadata.")
metadata <- metadata[idx, , drop = FALSE]

# Final checks
if (nrow(M) != nrow(metadata)) stop("Matrix rows and metadata rows disagree.")

# --- Write files ---
matrix_path   <- "raw_PLSDAbatch.csv"
metadata_path <- "metadata_PLSDAbatch.csv"

# Matrix: NO headers
write.table(M, file = matrix_path, sep = ",", row.names = FALSE, col.names = FALSE)

# Metadata: with headers
write.csv(metadata, file = metadata_path, row.names = FALSE, quote = TRUE)

cat("Done.\n",
    " - ", matrix_path,   " (samples x OTUs, no headers)\n",
    " - ", metadata_path, " (batch=Batch 1.., initial_phenol_concentration.regroup (original phenotype), filtered covariates)\n",
    sep = "")
