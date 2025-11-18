source(file.path("correction", "correction.R"))

prepare_method("PLSDA")

run_method("PLSDAbatch", {
  require(PLSDAbatch)
  if (!("target_binary" %in% colnames(metadata))) fail_step("PLSDAbatch", "'target_binary' not found.")
  if (length(unique(metadata$target_binary)) != 2) fail_step("PLSDAbatch", "'target_binary' must be binary.")
  X_clr <- get_input_for("PLSDA", base_M, base_form)
  res <- PLSDA_batch(
    X = X_clr,
    Y.trt = as.factor(metadata$target_binary),
    Y.bat = as.factor(metadata$batch_id),
    ncomp.trt = 1, ncomp.bat = 5
  )
  write_tss_clr("PLSDAbatch", res$X.nobatch, "clr", "normalized_plsda.csv")
})

finalize_method()
