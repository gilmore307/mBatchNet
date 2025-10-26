if (!exists(".CORRECTION_BOOTSTRAPPED", envir = .GlobalEnv, inherits = FALSE)) {
  source(file.path("correction", "correction.R"))
}

METHOD_CODE <- "ConQuR"

run_correction_method <- function() {
  prepare_method(METHOD_CODE)

  run_method(METHOD_CODE, {
    suppressPackageStartupMessages({ library(ConQuR); library(doParallel) })
    X_cnt <- get_input_for(METHOD_CODE, base_M, base_form)
    covariates <- metadata[, colnames(covar), drop = FALSE]; rownames(covariates) <- NULL
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
    write_tss_clr(METHOD_CODE, res_pos, "positive", "normalized_conqur.csv")
  })

  finalize_method()
}

if (!exists(".CORRECTION_DISPATCH_ACTIVE", envir = .GlobalEnv, inherits = FALSE)) {
  run_correction_method()
}
