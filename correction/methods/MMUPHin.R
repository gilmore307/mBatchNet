if (!exists(".CORRECTION_BOOTSTRAPPED", envir = .GlobalEnv, inherits = FALSE)) {
  source(file.path("correction", "correction.R"))
}

METHOD_CODE <- "MMUPHin"

run_correction_method <- function() {
  prepare_method(METHOD_CODE)

  run_method(METHOD_CODE, {
    require(MMUPHin)
    X_tss <- get_input_for(METHOD_CODE, base_M, base_form)
    feat_counts <- t(round(X_tss * 1e6))
    fit <- adjust_batch(
      feature_abd = feat_counts,
      batch       = "batch_id",
      covariates  = colnames(covar),
      data        = transform(metadata, batch_id=factor(batch_id)),
      control     = list(verbose = FALSE, diagnostic_plot = NULL)
    )
    out_pos <- t(fit$feature_abd_adj)
    write_tss_clr(METHOD_CODE, out_pos, "positive", "normalized_mmuphin.csv")
  })

  finalize_method()
}

if (!exists(".CORRECTION_DISPATCH_ACTIVE", envir = .GlobalEnv, inherits = FALSE)) {
  run_correction_method()
}
