if (!exists(".CORRECTION_BOOTSTRAPPED", envir = .GlobalEnv, inherits = FALSE)) {
  source(file.path("correction", "correction.R"))
}

METHOD_CODE  <- "PLSDA"
METHOD_LABEL <- "PLSDAbatch"

run_correction_method <- function() {
  prepare_method(METHOD_CODE)

  run_method(METHOD_LABEL, {
    require(PLSDAbatch)
    if (!("phenotype" %in% colnames(metadata))) fail_step(METHOD_LABEL, "'phenotype' not found.")
    if (length(unique(metadata$phenotype)) != 2) fail_step(METHOD_LABEL, "'phenotype' must be binary.")
    X_clr <- get_input_for(METHOD_CODE, base_M, base_form)
    res <- PLSDA_batch(
      X = X_clr,
      Y.trt = as.factor(metadata$phenotype),
      Y.bat = as.factor(metadata$batch_id),
      ncomp.trt = 1, ncomp.bat = 5
    )
    write_tss_clr(METHOD_LABEL, res$X.nobatch, "clr", "normalized_plsda.csv")
  })

  finalize_method()
}

if (!exists(".CORRECTION_DISPATCH_ACTIVE", envir = .GlobalEnv, inherits = FALSE)) {
  run_correction_method()
}
