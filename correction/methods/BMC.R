source(file.path("correction", "correction.R"))

prepare_method("BMC")

run_method("BMC", {
  require(pamr)
  X_log  <- get_input_for("BMC", base_M, base_form)
  pam_in <- list(x = as.matrix(t(X_log)), batchlabels = factor(batch))
  adj_log <- t(pamr.batchadjust(pam_in)$x)
  write_tss_clr("BMC", adj_log, "log", "normalized_bmc.csv")
})

finalize_method()
