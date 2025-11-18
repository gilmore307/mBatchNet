source(file.path("correction", "correction.R"))

prepare_method("ConQuR")

run_method("ConQuR", {
  suppressPackageStartupMessages({ library(ConQuR); library(doParallel) })
  X_cnt <- get_input_for("ConQuR", base_M, base_form)
  covariates <- metadata[, colnames(covar), drop = FALSE]

  lib_sizes <- rowSums(X_cnt)
  keep_lib <- !is.na(lib_sizes) & lib_sizes > 0
  if (!all(keep_lib)) {
    warn_step(
      "ConQuR",
      sprintf(
        "Dropped %d sample(s) with zero/NA library size before ConQuR.",
        sum(!keep_lib)
      )
    )
    metadata <- metadata[keep_lib, , drop = FALSE]
    X_cnt <- X_cnt[keep_lib, , drop = FALSE]
    covariates <- covariates[keep_lib, , drop = FALSE]
  }
  complete_rows <- !is.na(metadata$batch)
  if (ncol(covariates)) {
    complete_rows <- complete_rows & stats::complete.cases(covariates)
  }

  if (!all(complete_rows)) {
    warn_step(
      "ConQuR",
      sprintf(
        "Dropped %d sample(s) with missing batch/covariate values before ConQuR.",
        sum(!complete_rows)
      )
    )
    metadata <- metadata[complete_rows, , drop = FALSE]
    X_cnt <- X_cnt[complete_rows, , drop = FALSE]
    covariates <- covariates[complete_rows, , drop = FALSE]
  }

  batch_values <- unique(stats::na.omit(as.character(metadata$batch)))
  if (length(batch_values) < 2) {
    stop(
      sprintf(
        "ConQuR requires at least two batch levels; found %d level%s (%s).",
        length(batch_values),
        if (length(batch_values) == 1) "" else "s",
        paste(batch_values, collapse = ", ")
      )
    )
  }

  if (!ncol(covariates)) {
    covariates <- NULL
  } else {
    invariant_covariates <- vapply(
      covariates,
      function(col) {
        if (is.numeric(col)) return(FALSE)
        length(unique(stats::na.omit(as.character(col)))) < 2
      },
      logical(1)
    )

    if (any(invariant_covariates)) {
      dropped <- names(invariant_covariates)[invariant_covariates]
      warn_step(
        "ConQuR",
        sprintf(
          "Dropped invariant covariate(s) with <2 levels: %s",
          paste(dropped, collapse = ", ")
        )
      )
      covariates <- covariates[, !invariant_covariates, drop = FALSE]
    }

    if (!ncol(covariates)) {
      covariates <- NULL
    } else {
      rownames(covariates) <- NULL
    }
  }
  num_cores <- max(1, parallel::detectCores(TRUE) - 1)
  batch <- droplevels(as.factor(metadata$batch))
  batch_counts <- table(batch)

  if (any(batch_counts < 1) || length(batch_counts) < 2) {
    stop(
      sprintf(
        "ConQuR requires at least two batch levels with samples after filtering (counts: %s).",
        paste(sprintf("%s=%d", names(batch_counts), as.integer(batch_counts)), collapse = ", ")
      )
    )
  }

  res_pos <- suppressWarnings(
    tryCatch(
      ConQuR(
        tax_tab = X_cnt,
        batchid = batch,
        covariates = covariates,
        batch_ref = as.character(reference_batch),
        logistic_lasso = FALSE, quantile_type = "standard", simple_match = FALSE,
        lambda_quantile = "2p/n", interplt = FALSE, delta = 0.4999,
        taus = seq(0.05, 0.95, by = 0.05), num_core = num_cores
      ),
      error = function(e) {
        if (grepl("contrasts can be applied only to factors with 2 or more levels", e$message, fixed = TRUE)) {
          stop(
            sprintf(
              paste(
                "ConQuR failed because a factor dropped to <2 levels after filtering.",
                "Batch counts before failure: %s."
              ),
              paste(sprintf("%s=%d", names(batch_counts), as.integer(batch_counts)), collapse = ", ")
            )
          )
        }
        stop(e)
      }
    )
  )
  write_tss_clr("ConQuR", res_pos, "positive", "normalized_conqur.csv")
})

finalize_method()
