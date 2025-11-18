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
stopifnot(ncol(edata) == nrow(pheno))

# --- Pick phenotype column, keep original name and values ---
if ("cancer" %in% colnames(pheno)) {
  phenotype_colname <- "cancer"
} else if ("outcome" %in% colnames(pheno)) {
  phenotype_colname <- "outcome"
} else {
  stop("Neither 'cancer' nor 'outcome' found in phenotype data.")
}

phenotype_vec <- as.character(pheno[[phenotype_colname]])

# 将 biopsy/biospy 统一映射为 "Normal"（大小写不敏感）
idx_biopsy <- tolower(phenotype_vec) %in% c("biopsy", "biospy")
if (any(idx_biopsy)) {
  phenotype_vec[idx_biopsy] <- "Normal"
}

# --- Recode batch to 'Batch 1..' (mapping not saved) ---
if (!"batch" %in% colnames(pheno)) {
  stop("'batch' column not found in phenotype data.")
}

batch_raw    <- as.character(pheno$batch)
batch_levels <- unique(batch_raw)   # order of first appearance
batch_map    <- setNames(paste0("Batch ", seq_along(batch_levels)), batch_levels)
batch_vec    <- unname(batch_map[batch_raw])

# --- Build expression matrix output ---
# Matrix: samples x genes (transpose), numeric only, no headers
M <- t(as.matrix(edata))   # samples x genes
storage.mode(M) <- "double"

# --- Select covariate columns: remove batch / cancer / outcome / sample ---
covar_cols <- setdiff(colnames(pheno), c("batch", "cancer", "outcome", "sample"))
covars <- if (length(covar_cols)) {
  pheno[, covar_cols, drop = FALSE]
} else {
  pheno[, 0, drop = FALSE]   # empty data.frame with correct row number
}

# --- Build metadata: batch + (cancer/outcome) + covariates ---
batch_df <- data.frame(batch = batch_vec, stringsAsFactors = FALSE)
phenotype_df <- setNames(
  data.frame(phenotype_vec, stringsAsFactors = FALSE),
  phenotype_colname
)

metadata <- data.frame(
  batch_df,
  phenotype_df,
  covars,
  stringsAsFactors = FALSE
)

# Sanity check
stopifnot(nrow(M) == nrow(metadata))

# --- Write files (names include data source: bladderbatch) ---
write.table(
  M,
  file = "raw_bladderbatch.csv",
  sep = ",",
  row.names = FALSE,
  col.names = FALSE
)

write.csv(
  metadata,
  file = "metadata_bladderbatch.csv",
  row.names = FALSE,
  quote = TRUE
)
