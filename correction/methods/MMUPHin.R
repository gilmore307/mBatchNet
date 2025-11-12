source(file.path("correction", "correction.R"))

prepare_method("MMUPHin")

run_method("MMUPHin", {
  require(MMUPHin)
  X_tss <- get_input_for("MMUPHin", base_M, base_form)
  feat_counts <- t(round(X_tss * 1e6))
  covariate_names <- colnames(covar)
  if (!length(covariate_names)) {
    covariate_names <- NULL
  }

  fit <- adjust_batch(
    feature_abd = feat_counts,
    batch       = "batch_id",
    covariates  = covariate_names,
    data        = transform(metadata, batch_id=factor(batch_id)),
    control     = list(verbose = FALSE, diagnostic_plot = NULL)
  )
  out_pos <- t(fit$feature_abd_adj)
  write_tss_clr("MMUPHin", out_pos, "positive", "normalized_mmuphin.csv")
})

finalize_method()
