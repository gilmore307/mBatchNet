if (!exists(".CORRECTION_BOOTSTRAPPED", envir = .GlobalEnv, inherits = FALSE)) {
  source(file.path("correction", "correction.R"))
}

METHOD_CODE <- "MetaDICT"

run_correction_method <- function() {
  prepare_method(METHOD_CODE)

  run_method(METHOD_CODE, {
    suppressPackageStartupMessages({ library(MetaDICT); library(vegan) })
    O <- t(get_input_for(METHOD_CODE, base_M, base_form))
    meta <- transform(metadata[colnames(O), , drop = FALSE], batch = batch_id)
    D <- as.matrix(vegdist(O, method = "bray"))
    res <- MetaDICT(O, meta, distance_matrix = D, max_iter = 2000, customize_parameter = TRUE, alpha = 0.05, beta  = 0.20)
    out_pos <- t(res$count)
    write_tss_clr(METHOD_CODE, out_pos, "positive", "normalized_metadict.csv")
  })

  finalize_method()
}

if (!exists(".CORRECTION_DISPATCH_ACTIVE", envir = .GlobalEnv, inherits = FALSE)) {
  run_correction_method()
}
