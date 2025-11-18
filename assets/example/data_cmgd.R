# get_curatedMD_CRC_IBD.R
# Outputs:
#   CRC:
#     - raw_crc_cmgd.csv        (samples x OTUs, numeric, NO headers)
#     - metadata_crc_cmgd.csv   (batch = Batch 1.., study_condition (original labels), + covariates)
#   IBD:
#     - raw_ibd_cmgd.csv        (samples x OTUs, numeric, NO headers)
#     - metadata_ibd_cmgd.csv   (batch = Batch 1.., study_condition (original labels), + covariates)

quiet_install_bioc <- function(pkgs) {
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager", repos = "https://cloud.r-project.org")
  miss <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(miss)) BiocManager::install(miss, ask = FALSE, update = FALSE)
}

# Minimal deps (base only; avoid dplyr to reduce noise)
quiet_install_bioc(c("curatedMetagenomicData", "SummarizedExperiment", "Biobase"))

suppressPackageStartupMessages({
  library(curatedMetagenomicData)
  library(SummarizedExperiment)
  library(Biobase)
})

# ----------------------- Helpers -----------------------

# Fetch a MetaPhlAn-like abundance matrix (features x samples) for a study.
# Prefer ".metaphlan_bugs_list", fall back to ".relative_abundance".
# Do NOT filter by samples here; we merge first, intersect later.
fetch_metaphlan_for_study <- function(study_name) {
  product_candidates <- c(
    paste0(study_name, ".metaphlan_bugs_list"),
    paste0(study_name, ".relative_abundance")
  )
  obj <- NULL
  for (prod in product_candidates) {
    msg <- try({
      res <- curatedMetagenomicData(prod, dryrun = FALSE, counts = FALSE)
      obj <- res[[1]]
    }, silent = TRUE)
    if (!inherits(msg, "try-error") && !is.null(obj)) break
  }
  if (is.null(obj)) return(NULL)
  
  if (inherits(obj, "ExpressionSet")) {
    mat <- Biobase::exprs(obj)               # features x samples
    smd <- Biobase::pData(obj)
    if ("sample_id" %in% colnames(smd)) colnames(mat) <- smd$sample_id
  } else if (inherits(obj, "SummarizedExperiment")) {
    mat <- SummarizedExperiment::assay(obj, 1) # features x samples
    smd <- as.data.frame(SummarizedExperiment::colData(obj))
    if ("sample_id" %in% colnames(smd)) colnames(mat) <- smd$sample_id
  } else {
    return(NULL)
  }
  
  mat <- as.matrix(mat)
  storage.mode(mat) <- "double"
  mat[is.na(mat)] <- 0
  mat[mat < 0] <- 0
  mat
}

# Merge study-level matrices by UNION of features (rows); fill missing with 0
merge_union_by_rows <- function(accum_mat, new_mat) {
  new_mat <- as.matrix(new_mat)
  storage.mode(new_mat) <- "double"
  
  if (is.null(accum_mat) || is.null(dim(accum_mat)) || ncol(accum_mat) == 0) {
    rn <- rownames(new_mat); cn <- colnames(new_mat)
    out <- matrix(new_mat, nrow = nrow(new_mat), ncol = ncol(new_mat),
                  dimnames = list(rn, cn))
    return(out)
  }
  accum_mat <- as.matrix(accum_mat)
  storage.mode(accum_mat) <- "double"
  
  all_rows <- union(rownames(accum_mat), rownames(new_mat))
  
  A <- matrix(0, nrow = length(all_rows), ncol = ncol(accum_mat),
              dimnames = list(all_rows, colnames(accum_mat)))
  B <- matrix(0, nrow = length(all_rows), ncol = ncol(new_mat),
              dimnames = list(all_rows, colnames(new_mat)))
  
  A[rownames(accum_mat), colnames(accum_mat)] <- accum_mat
  B[rownames(new_mat),  colnames(new_mat)]    <- new_mat
  
  cbind(A, B)
}

# Recode batches (character vector) to "Batch 1.."
recode_batches <- function(batch_chr) {
  lv <- unique(batch_chr)
  mp <- setNames(paste0("Batch ", seq_along(lv)), lv)
  unname(mp[batch_chr])
}

# Covariate filtering based on your rules
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
    
    # Define missing as NA or empty string
    if (is_text) {
      missing <- is.na(v_chr) | v_chr == ""
    } else {
      missing <- is.na(v)
    }
    
    # 1) If any NA / empty values, drop the entire column
    if (any(missing)) {
      drop_reason <- sprintf("has %d NA/empty values", sum(missing))
    } else {
      # Use only non-missing values
      if (is_text) {
        v_use <- v_chr
      } else {
        v_use <- v
      }
      nunique <- length(unique(v_use))
      
      # 2) Only 0–1 unique values
      if (nunique <= 1) {
        drop_reason <- "only 0–1 unique value"
      }
      
      # 3) Fully determined by batch: one value per batch
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
      
      # 4) Fully determined by phenotype: one value per phenotype level
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
      
      # 5) Textual categorical with >2 unique levels
      if (is.null(drop_reason) && is_text && nunique > 2) {
        drop_reason <- sprintf("categorical text with %d unique values (>2)", nunique)
      }
    }
    
    if (!is.null(drop_reason)) {
      keep[j] <- FALSE
      message(sprintf("[%s] Dropping covariate '%s': %s", dataset_label, nm, drop_reason))
    }
  }
  
  covars[, keep, drop = FALSE]
}

# Write matrix (samples x OTUs) WITHOUT headers, and metadata with required columns
write_outputs <- function(M_samples_x_otus, metadata_df, prefix) {
  stopifnot(nrow(M_samples_x_otus) == nrow(metadata_df))
  write.table(M_samples_x_otus, file = paste0("raw_", prefix, ".csv"),
              sep = ",", row.names = FALSE, col.names = FALSE)
  write.csv(metadata_df, file = paste0("metadata_", prefix, ".csv"),
            row.names = FALSE, quote = TRUE)
}

# ----------------------- CRC pipeline (robust) -----------------------
message("==== CRC ====")
smd <- curatedMetagenomicData::sampleMetadata

crc_studies <- unique(na.omit(smd$study_name[smd$study_condition == "CRC"]))
crc_meta <- smd[smd$study_name %in% crc_studies &
                  smd$study_condition %in% c("control","CRC"), ]
rownames(crc_meta) <- crc_meta$sample_id

crc_mat <- NULL
for (st in unique(crc_meta$study_name)) {
  st_mat <- fetch_metaphlan_for_study(st)
  if (!is.null(st_mat) && length(dim(st_mat)) == 2L && ncol(st_mat) > 0L) {
    message(sprintf("[CRC] %s -> features=%d, samples=%d", st, nrow(st_mat), ncol(st_mat)))
    crc_mat <- merge_union_by_rows(crc_mat, st_mat)
  } else {
    message(sprintf("[CRC] %s -> no matrix", st))
  }
}
if (is.null(crc_mat) || is.null(dim(crc_mat)))
  stop("CRC: merged matrix is empty; no products found.")

# Intersect with selected samples only here
keep_cols <- intersect(colnames(crc_mat), rownames(crc_meta))
if (!length(keep_cols))
  stop("CRC: no overlapping sample_ids between merged matrix and crc_meta.")
crc_mat  <- as.matrix(crc_mat[, keep_cols, drop = FALSE])
crc_meta <- crc_meta[keep_cols, , drop = FALSE]

# Processing rules
# 1) remove "HanniganGD_2017"
crc_meta <- crc_meta[crc_meta$study_name != "HanniganGD_2017", ]
crc_mat  <- crc_mat[, rownames(crc_meta), drop = FALSE]

# 2) remove "ThomasAM_2019_c"
crc_meta <- crc_meta[crc_meta$study_name != "ThomasAM_2019_c", ]
crc_mat  <- crc_mat[, rownames(crc_meta), drop = FALSE]

# 3) drop all-zero taxa
if (nrow(crc_mat) == 0L) stop("CRC: zero features before zero-row filter.")
crc_mat <- crc_mat[rowSums(crc_mat) > 0, , drop = FALSE]
if (nrow(crc_mat) == 0L) stop("CRC: all features were zero rows after filtering.")

# 4) merge "ThomasAM_2018a/b" -> "ThomasAM_2018"
crc_meta$study_name <- sub("ThomasAM_2018a", "ThomasAM_2018", crc_meta$study_name)
crc_meta$study_name <- sub("ThomasAM_2018b", "ThomasAM_2018", crc_meta$study_name)

# Reorder columns to metadata order
crc_mat <- crc_mat[, rownames(crc_meta), drop = FALSE]

# Build outputs
crc_M <- t(crc_mat)
storage.mode(crc_M) <- "double"           # samples x OTUs, numeric

crc_meta_df   <- as.data.frame(crc_meta)
crc_batch     <- recode_batches(crc_meta_df$study_name)
crc_pheno_raw <- crc_meta_df$study_condition

message("[CRC] batch source column      : 'study_name'")
message("[CRC] phenotype source column : 'study_condition' (no recoding)")

# Covariates: remove columns used for ID / batch / phenotype
crc_covars_raw <- crc_meta_df[, setdiff(colnames(crc_meta_df),
                                        c("sample_id", "study_name", "study_condition")),
                              drop = FALSE]

# Apply covariate filters (phenotype passed as original study_condition)
crc_covars <- filter_covariates(crc_covars_raw,
                                batch      = crc_batch,
                                phenotype  = crc_pheno_raw,
                                dataset_label = "CRC")

crc_metadata <- data.frame(
  batch          = crc_batch,
  study_condition = crc_pheno_raw,
  crc_covars,
  stringsAsFactors = FALSE
)

write_outputs(crc_M, crc_metadata, prefix = "crc_cmgd")
message("Wrote: raw_crc_cmgd.csv, metadata_crc_cmgd.csv")

# ----------------------- IBD pipeline (robust) -----------------------
message("==== IBD ====")
ibd_studies <- unique(na.omit(smd$study_name[smd$study_condition == "IBD"]))

# include LifeLinesDeep_2016 (merged later into VilaAV_2018)
ibd_meta <- smd[smd$study_name %in% c(ibd_studies, "LifeLinesDeep_2016"), ]
ibd_meta <- ibd_meta[ibd_meta$study_condition %in% c("control", "IBD"), ]
rownames(ibd_meta) <- ibd_meta$sample_id

ibd_mat <- NULL
for (st in unique(ibd_meta$study_name)) {
  st_mat <- fetch_metaphlan_for_study(st)
  if (!is.null(st_mat) && length(dim(st_mat)) == 2L && ncol(st_mat) > 0L) {
    message(sprintf("[IBD] %s -> features=%d, samples=%d", st, nrow(st_mat), ncol(st_mat)))
    ibd_mat <- merge_union_by_rows(ibd_mat, st_mat)
  } else {
    message(sprintf("[IBD] %s -> no matrix", st))
  }
}
if (is.null(ibd_mat) || is.null(dim(ibd_mat)))
  stop("IBD: merged matrix is empty; no products found.")

# Intersect with selected samples only here
keep_cols <- intersect(colnames(ibd_mat), rownames(ibd_meta))
if (!length(keep_cols))
  stop("IBD: no overlapping sample_ids between merged matrix and ibd_meta.")
ibd_mat  <- as.matrix(ibd_mat[, keep_cols, drop = FALSE])
ibd_meta <- ibd_meta[keep_cols, , drop = FALSE]

# Processing rules
# 1) remove "LiJ_2014"
ibd_meta <- ibd_meta[ibd_meta$study_name != "LiJ_2014", ]
ibd_mat  <- ibd_mat[, rownames(ibd_meta), drop = FALSE]

# 2) drop samples missing in matrix (e.g. one HMP sample)
ibd_meta <- ibd_meta[rownames(ibd_meta) %in% colnames(ibd_mat), , drop = FALSE]
ibd_mat  <- ibd_mat[, rownames(ibd_meta), drop = FALSE]

# 3) drop all-zero taxa
if (nrow(ibd_mat) == 0L) stop("IBD: zero features before zero-row filter.")
ibd_mat <- ibd_mat[rowSums(ibd_mat) > 0, , drop = FALSE]
if (nrow(ibd_mat) == 0L) stop("IBD: all features were zero rows after filtering.")

# 4) merge "LifeLinesDeep_2016" into "VilaAV_2018"
ibd_meta$study_name <- gsub("LifeLinesDeep_2016", "VilaAV_2018", ibd_meta$study_name)

# Reorder columns to metadata order
ibd_mat <- ibd_mat[, rownames(ibd_meta), drop = FALSE]

# Build outputs
ibd_M <- t(ibd_mat)
storage.mode(ibd_M) <- "double"           # samples x OTUs

ibd_meta_df   <- as.data.frame(ibd_meta)
ibd_batch     <- recode_batches(ibd_meta_df$study_name)
ibd_pheno_raw <- ibd_meta_df$study_condition

message("[IBD] batch source column      : 'study_name'")
message("[IBD] phenotype source column : 'study_condition' (no recoding)")

# Covariates: remove columns used for ID / batch / phenotype
ibd_covars_raw <- ibd_meta_df[, setdiff(colnames(ibd_meta_df),
                                        c("sample_id", "study_name", "study_condition")),
                              drop = FALSE]

# Apply covariate filters (phenotype passed as original study_condition)
ibd_covars <- filter_covariates(ibd_covars_raw,
                                batch      = ibd_batch,
                                phenotype  = ibd_pheno_raw,
                                dataset_label = "IBD")

ibd_metadata <- data.frame(
  batch          = ibd_batch,
  study_condition = ibd_pheno_raw,
  ibd_covars,
  stringsAsFactors = FALSE
)

write_outputs(ibd_M, ibd_metadata, prefix = "ibd_cmgd")
message("Wrote: raw_ibd_cmgd.csv, metadata_ibd_cmgd.csv")

cat("Done.\n")
