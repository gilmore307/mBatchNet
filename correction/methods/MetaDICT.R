source(file.path("correction", "correction.R"))

prepare_method("MetaDICT")

run_method("MetaDICT", {
  suppressPackageStartupMessages({ library(MetaDICT); library(vegan) })
  
  orig_samples  <- rownames(base_M)
  orig_features <- colnames(base_M)
  
  O_samp_feat <- get_input_for("MetaDICT", base_M, base_form)   # n_samples × n_taxa
  stopifnot(identical(rownames(O_samp_feat), rownames(metadata)))
  O <- t(O_samp_feat)                                           # n_taxa × n_samples
  rm(O_samp_feat)
  
  meta <- metadata[colnames(O), , drop = FALSE]
  meta$batch <- droplevels(factor(meta$batch_id))
  if (anyNA(meta$batch))   fail_step("MetaDICT", "batch_id contains NA after alignment.")
  if (nlevels(meta$batch) < 2L) fail_step("MetaDICT", "batch_id has <2 levels after filtering.")
  
  storage.mode(O) <- "double"
  if (any(!is.finite(O))) fail_step("MetaDICT", "Non-finite values in input matrix (NA/Inf).")
  if (min(O) < 0)         fail_step("MetaDICT", "Negative values found — Bray–Curtis requires non-negatives.")
  
  emit_ids <- function(title, ids, n = 10L){
    if (!length(ids)) return(invisible())
    say(title, " ", length(ids), " -> ",
        paste(utils::head(ids, n), collapse = ", "),
        if (length(ids) > n) paste0(" ... (", length(ids) - n, " more)") else "")
  }
  
  dropped_any <- FALSE
  taxa_sum   <- rowSums(O)
  sample_sum <- colSums(O)
  if (any(sample_sum == 0)) {
    bad <- names(sample_sum[sample_sum == 0])
    warn_step("MetaDICT", paste0("Dropping ", length(bad), " all-zero samples (columns)."))
    emit_ids("sample_all_zero:", bad)
    O   <- O[, sample_sum > 0, drop = FALSE]
    meta <- meta[colnames(O), , drop = FALSE]
    dropped_any <- TRUE
  }
  if (any(taxa_sum == 0)) {
    bad <- names(taxa_sum[taxa_sum == 0])
    warn_step("MetaDICT", paste0("Dropping ", length(bad), " all-zero taxa (rows)."))
    emit_ids("taxa_all_zero:", bad)
    O <- O[taxa_sum > 0, , drop = FALSE]
    dropped_any <- TRUE
  }
  if (ncol(O) < 2L) fail_step("MetaDICT", "Insufficient non-empty samples after filtering.")
  if (nrow(O) < 2L) fail_step("MetaDICT", "Insufficient non-empty taxa after filtering.")
  
  D <- NULL
  D_try <- try(as.matrix(vegan::vegdist(O, method = "bray")), silent = TRUE) # 行=taxa
  if (inherits(D_try, "try-error")) {
    warn_step("MetaDICT", "vegdist failed; falling back to internal distances.")
  } else {
    nonfin <- unique(c(rownames(D_try)[!is.finite(rowSums(D_try))],
                       rownames(D_try)[!is.finite(colSums(D_try))]))
    if (length(nonfin)) {
      warn_step("MetaDICT", paste0("Removing ", length(nonfin), " taxa with non-finite Bray distances."))
      emit_ids("taxa_nonfinite_bray:", nonfin)
      keep_taxa <- setdiff(rownames(O), nonfin)
      if (length(keep_taxa) < 2L) fail_step("MetaDICT", "Too few taxa remain after removing non-finite-distance taxa.")
      O <- O[keep_taxa, , drop = FALSE]
      D_try <- as.matrix(vegan::vegdist(O, method = "bray"))
      dropped_any <- TRUE
    }
    if (any(!is.finite(D_try))) {
      warn_step("MetaDICT", "Bray matrix still has non-finite values; using internal distances.")
    } else {
      D <- D_try[rownames(O), rownames(O), drop = FALSE]
    }
  }
  if (!dropped_any) say("No samples/features dropped.")
  
  res <- MetaDICT(
    count = O,
    meta  = meta,
    distance_matrix = D,
    customize_parameter = TRUE,
    alpha = 0.05, beta = 0.20,
    normalization = "uq",
    max_iter = 2000,
    verbose = TRUE
  )
  
  out_pos <- t(res$count)
  
  out_full <- matrix(0, nrow = length(orig_samples), ncol = length(orig_features))
  rownames(out_full) <- orig_samples
  colnames(out_full) <- orig_features
  r_common <- intersect(rownames(out_pos), orig_samples)
  c_common <- intersect(colnames(out_pos), orig_features)
  if (length(r_common) && length(c_common)) {
    out_full[r_common, c_common] <- out_pos[r_common, c_common, drop = FALSE]
  }
  
  storage.mode(out_full) <- "double"
  write_tss_clr("MetaDICT", out_full, "positive", "normalized_metadict.csv")
})

finalize_method()
