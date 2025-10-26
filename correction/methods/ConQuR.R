source(file.path("correction", "methods", "common.R"))

prepare_method("ConQuR")

run_method("ConQuR", {
  suppressPackageStartupMessages({ library(ConQuR); library(doParallel) })
  X_cnt <- get_input_for("ConQuR", base_M, base_form)
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
  write_tss_clr("ConQuR", res_pos, "positive", "normalized_conqur.csv")
})

finalize_method()
