if (!exists(".CORRECTION_BOOTSTRAPPED", envir = .GlobalEnv, inherits = FALSE)) {
  source(file.path("correction", "correction.R"))
}

METHOD_CODE <- "ComBat"

run_correction_method <- function() {
  prepare_method(METHOD_CODE)

  run_method(METHOD_CODE, {
    require(sva)
    X_log <- get_input_for(METHOD_CODE, base_M, base_form)
    adj_t <- ComBat(
      dat = t(X_log),
      batch = batch_id,
      mod = if (ncol(covar) > 0) model.matrix(~ ., data = covar) else NULL,
      par.prior = FALSE, prior.plots = FALSE
    )
    adj <- t(adj_t)
    write_tss_clr(METHOD_CODE, adj, "log", "normalized_combat.csv")
  })

  finalize_method()
}

if (!exists(".CORRECTION_DISPATCH_ACTIVE", envir = .GlobalEnv, inherits = FALSE)) {
  run_correction_method()
}
