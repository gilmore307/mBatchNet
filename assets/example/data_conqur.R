# get_ConQuR_data.R
# Outputs:
#   1) raw_conqur.csv        -> samples x OTUs, numeric only, NO row/col names
#   2) metadata_ConQuR.csv   -> CSV with headers: batch_id (Batch 1..), phenotype (0/1)

quiet_install_cran <- function(pkgs) {
  missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
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
taxa <- as.matrix(Sample_Data[, 1:100, drop = FALSE])

# metadata columns
batch_raw <- Sample_Data[, "batchid"]
sex_raw   <- Sample_Data[, "sex"   ]
race_raw  <- Sample_Data[, "race"  ]

# -------- Helpers --------
to_binary01 <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  if (is.numeric(x)) {
    ux <- sort(unique(x))
    if (length(ux) == 2L) return(match(x, ux) - 1L)
  }
  if (is.factor(x) || is.character(x)) {
    f <- factor(x)  # if character, alphabetical levels; if factor, keep level order
    if (nlevels(f) == 2L) return(as.integer(f) - 1L)
  }
  stop("Phenotype must have exactly two unique values (or be logical).")
}

# Choose phenotype: prefer 'sex', fallback to 'race'
phenotype_vec <- tryCatch(to_binary01(sex_raw),
                          error = function(e) tryCatch(to_binary01(race_raw), error = function(e2) stop("Neither 'sex' nor 'race' is strictly binary.")))

# Recode batch to "Batch 1.."
batch_chr    <- as.character(batch_raw)
batch_levels <- unique(batch_chr)                                # order of first appearance
batch_map    <- setNames(paste0("Batch ", seq_along(batch_levels)), batch_levels)
batch_id     <- unname(batch_map[batch_chr])

# -------- Build outputs --------
# Matrix: ensure numeric; samples x OTUs; no headers
storage.mode(taxa) <- "double"
M <- taxa  # already samples x OTUs in ConQuR sample data

# Metadata
metadata <- data.frame(
  batch_id  = batch_id,
  phenotype = phenotype_vec,
  stringsAsFactors = FALSE
)

# Sanity check
if (nrow(M) != nrow(metadata)) stop("Matrix rows and metadata rows disagree.")

# -------- Write files --------
matrix_path   <- "raw_conqur.csv"
metadata_path <- "metadata_ConQuR.csv"

write.table(M, file = matrix_path, sep = ",", row.names = FALSE, col.names = FALSE)
write.csv(metadata, file = metadata_path, row.names = FALSE, quote = TRUE)

cat("Done.\n",
    " - ", matrix_path,   " (samples x OTUs, no headers)\n",
    " - ", metadata_path, " (batch_id=Batch 1.., phenotype=0/1)\n", sep = "")
