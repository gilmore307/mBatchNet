# get_ComBat_bladderbatch_data.R
# Outputs:
#   1) raw_combat.csv        -> samples x genes (numeric), NO row/col names
#   2) metadata_ComBat.csv   -> CSV with headers: batch_id (Batch 1..), phenotype (0/1)

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

# Minimal deps
quiet_install_bioc(c("bladderbatch", "Biobase"))

suppressPackageStartupMessages({
  library(bladderbatch)
  library(Biobase)
})

# --- Load demo data ---
data("bladderdata", package = "bladderbatch")  # loads 'bladderEset' (ExpressionSet)

# If you want to mimic the vignette's 50-gene subset, set n_features <- 50
n_features <- NULL
eset <- if (is.null(n_features)) bladderEset else bladderEset[seq_len(n_features), ]

pheno <- Biobase::pData(eset)     # samples x covariates
edata <- Biobase::exprs(eset)     # genes x samples

# --- Ensure sample order consistency (ExpressionSet already aligned) ---
# Columns of edata correspond to rows of pheno
stopifnot(ncol(edata) == nrow(pheno))

# --- Build phenotype (binary 0/1) from 'cancer' ---
# If 'cancer' has >2 levels (e.g., "cancer","biopsy","control"), set phenotype=1 for 'cancer', else 0
to_binary01 <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  if (is.numeric(x)) { ux <- sort(unique(x)); if (length(ux) == 2L) return(match(x, ux) - 1L) }
  if (is.factor(x) || is.character(x)) { f <- factor(x); if (nlevels(f) == 2L) return(as.integer(f) - 1L) }
  stop("Input must have exactly two unique values (or be logical).")
}

phenotype_vec <- NULL
if ("cancer" %in% colnames(pheno)) {
  cx <- pheno$cancer
  # Try strict 2-level coercion
  phenotype_vec <- tryCatch(to_binary01(cx), error = function(e) NULL)
  if (is.null(phenotype_vec)) {
    # Collapse to binary: label 1 if value matches 'cancer' (case-insensitive), else 0
    cx_chr <- tolower(as.character(cx))
    phenotype_vec <- as.integer(grepl("^cancer$", cx_chr))
  }
} else if ("outcome" %in% colnames(pheno)) {
  # Fallback: 1 for any non-"Normal", 0 for "Normal"
  ox <- tolower(as.character(pheno$outcome))
  phenotype_vec <- as.integer(ox != "normal")
} else {
  stop("Neither 'cancer' nor 'outcome' found in phenotype data.")
}

# --- Recode batch to 'Batch 1..' (distinguishable, mapping not saved) ---
if (!"batch" %in% colnames(pheno)) stop("'batch' column not found in phenotype data.")
batch_raw     <- as.character(pheno$batch)
batch_levels  <- unique(batch_raw)                                   # order of first appearance
batch_map     <- setNames(paste0("Batch ", seq_along(batch_levels)), batch_levels)
batch_id_vec  <- unname(batch_map[batch_raw])

# --- Build outputs ---
# Matrix: samples x genes (transpose), numeric only, no headers
M <- t(as.matrix(edata))   # samples x genes
storage.mode(M) <- "double"

# Metadata: batch_id + phenotype (0/1)
metadata <- data.frame(
  batch_id  = batch_id_vec,
  phenotype = phenotype_vec,
  stringsAsFactors = FALSE
)

# Sanity check
stopifnot(nrow(M) == nrow(metadata))

# --- Write files ---
write.table(M, file = "raw_combat.csv", sep = ",", row.names = FALSE, col.names = FALSE)
write.csv(metadata, file = "metadata_ComBat.csv", row.names = FALSE, quote = TRUE)

cat("Done.\n",
    " - raw_combat.csv (samples x genes, no headers)\n",
    " - metadata_ComBat.csv (batch_id=Batch 1.., phenotype=0/1)\n", sep = "")
