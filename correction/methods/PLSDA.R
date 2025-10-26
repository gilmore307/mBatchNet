source(file.path("correction", "methods", "common.R"))

prepare_method("PLSDA")

run_method("PLSDAbatch", {
  require(PLSDAbatch)
  if (!("phenotype" %in% colnames(metadata))) fail_step("PLSDAbatch", "'phenotype' not found.")
  if (length(unique(metadata$phenotype)) != 2) fail_step("PLSDAbatch", "'phenotype' must be binary.")
  X_clr <- get_input_for("PLSDA", base_M, base_form)
  res <- PLSDA_batch(
    X = X_clr,
    Y.trt = as.factor(metadata$phenotype),
    Y.bat = as.factor(metadata$batch_id),
    ncomp.trt = 1, ncomp.bat = 5
  )
  write_tss_clr("PLSDAbatch", res$X.nobatch, "clr", "normalized_plsda.csv")
})

finalize_method()
