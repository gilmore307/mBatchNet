# get_ConQuR_data.R
# Outputs:
#   1) raw_conqur.csv        -> samples x OTUs, numeric only, NO row/col names
#   2) metadata_ConQuR.csv   -> CSV with headers:
#        batch (Batch 1..), phenotype (0/1),
#        and covariates (excluding OTUs / batchid / sex-or-race used for phenotype)

quiet_install_cran <- function(pkgs) {
  missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing)) {
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
}

# Try CRAN first
quiet_install_cran(c("ConQuR", "doParallel"))

# Fallback to GitHub if ConQuR not found
if (!requireNamespace("ConQuR", quietly = TRUE)) {
  quiet_install_cran("remotes")
  if (requireNamespace("remotes", quietly = TRUE)) {
    try(remotes::install_github("wdl2459/ConQuR"), silent = TRUE)
  }
}

suppressPackageStartupMessages({
  library(ConQuR)
  library(doParallel)  # ConQuR vignette recommends loading doParallel together
})

# -------- Load sample data from ConQuR --------
data("Sample_Data", package = "ConQuR")

# taxa: first 100 columns are OTU counts; rows = samples
otu_cols <- colnames(Sample_Data)[1:100]
taxa     <- as.matrix(Sample_Data[, otu_cols, drop = FALSE])

# metadata raw columns
batch_raw <- Sample_Data[, "batchid"]
sex_raw   <- Sample_Data[, "sex"   ]
race_raw  <- Sample_Data[, "race"  ]

# -------- Helper: convert to binary 0/1 --------
to_binary01 <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  if (is.numeric(x)) {
    ux <- sort(unique(x))
    if (length(ux) == 2L) return(match(x, ux) - 1L)
  }
  if (is.factor(x) || is.character(x)) {
    f <- factor(x)  # for character: alphabetical; for factor: level order
    if (nlevels(f) == 2L) return(as.integer(f) - 1L)
  }
  stop("Phenotype must have exactly two unique values (or be logical).")
}

# -------- Choose phenotype: prefer 'sex', fallback to 'race' --------
phenotype_vec    <- NULL
phenotype_source <- NULL

# Try sex first
phenotype_vec <- tryCatch(
  {
    phenotype_source <<- "sex"
    to_binary01(sex_raw)
  },
  error = function(e) NULL
)

# If sex is not strictly binary, try race
if (is.null(phenotype_vec)) {
  phenotype_vec <- tryCatch(
    {
      phenotype_source <<- "race"
      to_binary01(race_raw)
    },
    error = function(e2) {
      stop("Neither 'sex' nor 'race' is strictly binary.")
    }
  )
}

# -------- Recode batch to "Batch 1.." --------
batch_chr    <- as.character(batch_raw)
batch_levels <- unique(batch_chr)   # order of first appearance
batch_map    <- setNames(paste0("Batch ", seq_along(batch_levels)), batch_levels)
batch     <- unname(batch_map[batch_chr])

# -------- Build expression matrix output --------
# Matrix: ensure numeric; samples x OTUs; no headers
storage.mode(taxa) <- "double"
M <- taxa  # already samples x OTUs in ConQuR sample data

# -------- Build covariates: drop OTU columns + batchid + phenotype source --------
all_cols  <- colnames(Sample_Data)
batch_col <- "batchid"

drop_cols <- c(
  otu_cols,        # OTU count columns
  batch_col,       # raw batch column (mapped to batch)
  phenotype_source # the column used to build phenotype (sex or race)
)

covar_cols <- setdiff(all_cols, drop_cols)

covars <- if (length(covar_cols)) {
  Sample_Data[, covar_cols, drop = FALSE]
} else {
  # empty data.frame with correct number of rows
  Sample_Data[, 0, drop = FALSE]
}

# -------- Metadata: batch + phenotype + covariates --------
metadata <- data.frame(
  batch  = batch,
  phenotype = phenotype_vec,
  covars,
  stringsAsFactors = FALSE
)

# Sanity check
if (nrow(M) != nrow(metadata)) {
  stop("Matrix rows and metadata rows disagree.")
}

# -------- Write files --------
matrix_path   <- "raw_conqur.csv"
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

cat(
  "Done.\n",
  " - ", matrix_path,   " (samples x OTUs, no headers)\n",
  " - ", metadata_path, " (batch=Batch 1.., phenotype=0/1, + covariates w/o OTUs/batchid/sex-or-race)\n",
  sep = ""
)

