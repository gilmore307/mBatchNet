# get_MetaDICT_data.R
# Outputs:
#   1) matrix_MetaDICT.csv    -> samples x OTUs, numeric only, NO row/col names
#   2) metadata_MetaDICT.csv  -> has headers; includes batch_id and phenotype (0/1); keeps other covariates if present

quiet_install <- function(pkgs) {
  pkgs <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(pkgs)) install.packages(pkgs, repos = "https://cloud.r-project.org")
}

quiet_install(c("devtools"))

if (!requireNamespace("MetaDICT", quietly = TRUE)) {
  devtools::install_github("BoYuan07/MetaDICT", build_vignettes = FALSE, quiet = TRUE)
}

suppressPackageStartupMessages(library(MetaDICT))

# ---- Load vignette example ----
data("exampleData", package = "MetaDICT")
O    <- exampleData$O      # taxa x sample matrix
meta <- exampleData$meta   # sample metadata (contains 'batch', 'Y', 'Y2', ...)

# ---- Helper: coerce a vector to binary 0/1 safely ----
to_binary01 <- function(x) {
  # logical: FALSE->0, TRUE->1
  if (is.logical(x)) return(as.integer(x))
  
  # numeric with exactly two unique values
  if (is.numeric(x)) {
    ux <- sort(unique(x))
    if (length(ux) == 2L) {
      m <- match(x, ux) - 1L        # smaller -> 0, larger -> 1
      attr(m, "mapping") <- setNames(c(0,1), ux)
      return(m)
    }
  }
  
  # factor/character with exactly two levels
  if (is.factor(x) || is.character(x)) {
    f <- factor(x)                   # preserves level order if factor, alphabetical if character
    if (nlevels(f) == 2L) {
      out <- as.integer(f) - 1L      # first level -> 0, second -> 1
      attr(out, "mapping") <- setNames(c(0,1), levels(f))
      return(out)
    }
  }
  
  stop("phenotype must have exactly two unique values (or be logical). Cannot safely coerce to 0/1.")
}

# ---- Build metadata: batch_id + phenotype (0/1) + other covariates ----
raw_pheno <- meta$Y  # using Y as the informative phenotype per the vignette
phenotype01 <- to_binary01(raw_pheno)

md <- data.frame(
  batch_id  = as.character(meta$batch),
  phenotype = phenotype01,
  stringsAsFactors = FALSE
)

# Append other covariates (if any), excluding 'batch' and 'Y' to avoid duplication
other_cols <- setdiff(colnames(meta), c("batch", "Y"))
if (length(other_cols) > 0) {
  md <- cbind(md, meta[, other_cols, drop = FALSE])
}

# ---- Build matrix: samples x OTUs, NO headers ----
if (ncol(O) != nrow(md)) {
  stop("Dimension mismatch: ncol(O) must equal number of samples in metadata.")
}
M <- t(as.matrix(O))   # samples x OTUs
storage.mode(M) <- "double"

# ---- Write outputs ----
matrix_path   <- "raw_MetaDICT.csv"
metadata_path <- "metadata_MetaDICT.csv"

write.table(M, file = matrix_path, sep = ",", row.names = FALSE, col.names = FALSE)
write.csv(md,  file = metadata_path, row.names = FALSE, quote = TRUE)

# Optional: print mapping info for reproducibility
map <- attr(phenotype01, "mapping")
if (!is.null(map)) {
  cat("Phenotype mapping (original -> 0/1):\n")
  print(map)
}
cat("Done.\n - ", matrix_path, " (samples x OTUs, no headers)\n - ", metadata_path, " (batch_id, phenotype=0/1)\n", sep = "")
