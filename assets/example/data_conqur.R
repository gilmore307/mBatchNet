suppressPackageStartupMessages({
  library(ConQuR)
})

# -------- Load sample data from ConQuR --------
data("Sample_Data", package = "ConQuR")

# taxa: first 100 columns are OTU counts; rows = samples
otu_cols <- colnames(Sample_Data)[1:100]
taxa     <- as.matrix(Sample_Data[, otu_cols, drop = FALSE])

# metadata raw columns
batch_raw <- Sample_Data[, "batchid"]

# -------- Choose phenotype column: prefer 'sex', fallback to 'race' --------
if ("sex" %in% colnames(Sample_Data)) {
  phenotype_colname <- "sex"
} else if ("race" %in% colnames(Sample_Data)) {
  phenotype_colname <- "race"
} else {
  stop("Neither 'sex' nor 'race' found in Sample_Data.")
}

# Use original column name and values, do not binarize
phenotype_vec <- as.character(Sample_Data[[phenotype_colname]])

# -------- Recode batch to "Batch 1.." --------
batch_chr    <- as.character(batch_raw)
batch_levels <- unique(batch_chr)   # order of first appearance
batch_map    <- setNames(paste0("Batch ", seq_along(batch_levels)), batch_levels)
batch_vec    <- unname(batch_map[batch_chr])

# -------- Build expression matrix output --------
# Matrix: ensure numeric; samples x OTUs; no headers
storage.mode(taxa) <- "double"
M <- taxa  # already samples x OTUs in ConQuR sample data

# -------- Build covariates: drop OTU columns + batchid + phenotype source --------
all_cols  <- colnames(Sample_Data)
batch_col <- "batchid"

drop_cols <- c(
  otu_cols,          # OTU count columns
  batch_col,         # raw batch column (mapped to batch)
  phenotype_colname  # original column used as phenotype (sex or race)
)

covar_cols <- setdiff(all_cols, drop_cols)

covars <- if (length(covar_cols)) {
  Sample_Data[, covar_cols, drop = FALSE]
} else {
  # empty data.frame with correct number of rows
  Sample_Data[, 0, drop = FALSE]
}

# -------- Metadata: batch + (sex/race as original) + covariates --------
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
if (nrow(M) != nrow(metadata)) {
  stop("Matrix rows and metadata rows disagree.")
}

# -------- Write files (use data name: ConQuR) --------
matrix_path   <- "raw_ConQuR.csv"
metadata_path <- "metadata_ConQuR.csv"

write.table(
  M,
  file = matrix_path,
  sep = ",",
  row.names = FALSE,
  col.names = FALSE
)

write.csv(
  metadata,
  file = metadata_path,
  row.names = FALSE,
  quote = TRUE
)
