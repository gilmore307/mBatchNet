if (!exists(".CORRECTION_BOOTSTRAPPED", envir = .GlobalEnv, inherits = FALSE)) {
  source(file.path("correction", "correction.R"))
}

METHOD_CODE <- "limma"

run_correction_method <- function() {
  prepare_method(METHOD_CODE)

  run_method(METHOD_CODE, {
    require(limma)
    X_log <- get_input_for(METHOD_CODE, base_M, base_form)
    adj_t <- removeBatchEffect(
      t(X_log),
      batch = factor(batch_id),
      covariates = if (ncol(covar) > 0) as.matrix(covar) else NULL
    )
    adj <- t(adj_t)
    write_tss_clr(METHOD_CODE, adj, "log", "normalized_limma.csv")
  })

  finalize_method()
}

if (!exists(".CORRECTION_DISPATCH_ACTIVE", envir = .GlobalEnv, inherits = FALSE)) {
  run_correction_method()
}
