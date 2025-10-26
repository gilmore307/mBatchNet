source(file.path("correction", "methods", "common.R"))

prepare_method("limma")

run_method("limma", {
  require(limma)
  X_log <- get_input_for("limma", base_M, base_form)
  adj_t <- removeBatchEffect(
    t(X_log),
    batch = factor(batch_id),
    covariates = if (ncol(covar) > 0) as.matrix(covar) else NULL
  )
  adj <- t(adj_t)
  write_tss_clr("limma", adj, "log", "normalized_limma.csv")
})

finalize_method()
