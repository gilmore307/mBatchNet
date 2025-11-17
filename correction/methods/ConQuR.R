source(file.path("correction", "correction.R"))

prepare_method("ConQuR")

run_method("ConQuR", {
  suppressPackageStartupMessages({ library(ConQuR); library(doParallel) })
  X_cnt <- get_input_for("ConQuR", base_M, base_form)
  batch_values <- unique(stats::na.omit(as.character(metadata$batch_id)))
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

  covariates <- metadata[, colnames(covar), drop = FALSE]
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
  res_pos <- suppressWarnings(
    ConQuR(
      tax_tab = X_cnt,
      batchid = as.factor(metadata$batch_id),
      covariates = covariates,
      batch_ref = as.character(reference_batch),
      logistic_lasso = FALSE, quantile_type = "standard", simple_match = FALSE,
      lambda_quantile = "2p/n", interplt = FALSE, delta = 0.4999,
      taus = seq(0.05, 0.95, by = 0.05), num_core = num_cores
    )
  )
  write_tss_clr("ConQuR", res_pos, "positive", "normalized_conqur.csv")
})

finalize_method()
