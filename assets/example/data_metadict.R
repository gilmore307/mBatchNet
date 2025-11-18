# get_MetaDICT_data.R
# Outputs:
#   1) raw_MetaDICT.csv      -> samples x OTUs, numeric only, NO row/col names
#   2) metadata_MetaDICT.csv -> has headers; includes:
#        - batch (Batch 1..)
#        - Y (phenotype, original values, unchanged)
#        - filtered covariates:
#            * drop any column that has at least one NA or empty string
#            * drop columns with ≤1 unique value
#            * drop columns determined by batch (one value per batch)
#            * drop columns determined by phenotype (one value per phenotype level)
#            * drop text/factor columns with >2 unique values

quiet_install <- function(pkgs) {
  pkgs <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(pkgs)) install.packages(pkgs, repos = "https://cloud.r-project.org")
}

quiet_install(c("devtools"))

if (!requireNamespace("MetaDICT", quietly = TRUE)) {
  devtools::install_github("BoYuan07/MetaDICT", build_vignettes = FALSE, quiet = TRUE)
}

suppressPackageStartupMessages(library(MetaDICT))

# ----------------- Helpers -----------------

# Recode batches (character vector) to "Batch 1.."
recode_batches <- function(batch_chr) {
  lv <- unique(batch_chr)
  mp <- setNames(paste0("Batch ", seq_along(lv)), lv)
  unname(mp[batch_chr])
}

# Covariate filtering:
# - drop column if any NA / empty string
# - drop if ≤1 unique value
# - drop if determined by batch (one value per batch)
# - drop if determined by phenotype (one value per phenotype level)
# - drop if character/factor and unique values > 2
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
      # now v has no NA/empty
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
      
      # 5) text/factor with >2 levels
      if (is.null(drop_reason) && is_text && nunique > 2) {
        drop_reason <- sprintf("categorical text with %d unique values (>2)", nunique)
      }
    }
    
    if (!is.null(drop_reason)) {
      keep[j] <- FALSE
      message(sprintf("[MetaDICT] Dropping covariate '%s': %s", nm, drop_reason))
    }
  }
  
  covars[, keep, drop = FALSE]
}

# ----------------- Load vignette example -----------------

data("exampleData", package = "MetaDICT")
O    <- exampleData$O      # taxa x sample matrix
meta <- exampleData$meta   # sample metadata (contains 'batch', 'Y', 'Y2', ...)

# ----------------- Phenotype (original) -----------------

phenotype <- meta$Y  # use Y directly, original values and column name
message("[MetaDICT] phenotype source column : 'Y' (original values, not recoded)")

# ----------------- Batch -----------------

raw_batch <- meta$batch
batch_chr <- as.character(raw_batch)
batch     <- recode_batches(batch_chr)

message("[MetaDICT] batch source column : 'batch' (re-coded to 'Batch 1..')")

# ----------------- Covariates -----------------

# Start from all meta columns except those explicitly used for batch / phenotype
covar_raw <- meta[, setdiff(colnames(meta), c("batch", "Y")), drop = FALSE]

# Filter covariates with strict rules
covar_filtered <- filter_covariates(
  covars        = covar_raw,
  batch         = batch,
  phenotype     = phenotype,
  dataset_label = "MetaDICT"
)

# ----------------- Build metadata -----------------

md <- data.frame(
  batch = batch,
  Y     = phenotype,
  covar_filtered,
  stringsAsFactors = FALSE
)

# ----------------- Build matrix: samples x OTUs, NO headers -----------------

if (ncol(O) != nrow(md)) {
  stop("Dimension mismatch: ncol(O) must equal number of samples in metadata.")
}
M <- t(as.matrix(O))   # samples x OTUs
storage.mode(M) <- "double"

# ----------------- Write outputs -----------------

matrix_path   <- "raw_MetaDICT.csv"
metadata_path <- "metadata_MetaDICT.csv"

write.table(M, file = matrix_path, sep = ",", row.names = FALSE, col.names = FALSE)
write.csv(md,  file = metadata_path, row.names = FALSE, quote = TRUE)

cat("Done.\n",
    " - ", matrix_path,   " (samples x OTUs, no headers)\n",
    " - ", metadata_path, " (batch=Batch 1.., Y (original phenotype), filtered covariates)\n",
    sep = "")
