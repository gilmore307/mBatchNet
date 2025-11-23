source(file.path("correction", "correction.R"))

prepare_method("ComBat")

run_method("ComBat", {
  require(sva)
  X_log <- get_input_for("ComBat", base_M, base_form)
  par_prior <- isTRUE(get_param("par.prior", FALSE))
  adj_t <- ComBat(
    dat = t(X_log),
    batch = batch,
    mod = if (ncol(covar) > 0) model.matrix(~ ., data = covar) else NULL,
    par.prior = par_prior, prior.plots = FALSE
  )
  adj <- t(adj_t)
  write_tss_clr("ComBat", adj, "log", "normalized_combat.csv")
})

finalize_method()
