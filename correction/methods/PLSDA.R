source(file.path("correction", "correction.R"))

prepare_method("PLSDA")

run_method("PLSDAbatch", {
  require(PLSDAbatch)
  if (!("target_binary" %in% colnames(metadata))) fail_step("PLSDAbatch", "'target_binary' not found.")
  if (length(unique(metadata$target_binary)) != 2) fail_step("PLSDAbatch", "'target_binary' must be binary.")
  X_clr <- get_input_for("PLSDA", base_M, base_form)
  ncomp_trt <- suppressWarnings(as.integer(get_param("ncomp.trt", 1)))
  if (!is.finite(ncomp_trt) || ncomp_trt < 1L) ncomp_trt <- 1L
  ncomp_bat <- suppressWarnings(as.integer(get_param("ncomp.bat", 5)))
  if (!is.finite(ncomp_bat) || ncomp_bat < 1L) ncomp_bat <- 5L
  keepX_trt <- get_param("keepX.trt", NULL)
  if (is.character(keepX_trt) && length(keepX_trt) == 1L && !nzchar(keepX_trt)) keepX_trt <- NULL
  near_zero_var <- isTRUE(get_param("near.zero.var", FALSE))
  balance <- isTRUE(get_param("balance", FALSE))
  res <- PLSDA_batch(
    X = X_clr,
    Y.trt = as.factor(metadata$target_binary),
    Y.bat = as.factor(metadata$batch),
    ncomp.trt = ncomp_trt, ncomp.bat = ncomp_bat,
    keepX.trt = keepX_trt,
    near.zero.var = near_zero_var,
    balance = balance
  )
  write_tss_clr("PLSDAbatch", res$X.nobatch, "clr", "normalized_plsda.csv")
})

finalize_method()
