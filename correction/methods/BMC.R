if (!exists(".CORRECTION_BOOTSTRAPPED", envir = .GlobalEnv, inherits = FALSE)) {
  source(file.path("correction", "correction.R"))
}

METHOD_CODE <- "BMC"

run_correction_method <- function() {
  prepare_method(METHOD_CODE)

  run_method(METHOD_CODE, {
    require(pamr)
    X_log  <- get_input_for(METHOD_CODE, base_M, base_form)
    pam_in <- list(x = as.matrix(t(X_log)), batchlabels = factor(batch_id))
    adj_log <- t(pamr.batchadjust(pam_in)$x)
    write_tss_clr(METHOD_CODE, adj_log, "log", "normalized_bmc.csv")
  })

  finalize_method()
}

if (!exists(".CORRECTION_DISPATCH_ACTIVE", envir = .GlobalEnv, inherits = FALSE)) {
  run_correction_method()
}
