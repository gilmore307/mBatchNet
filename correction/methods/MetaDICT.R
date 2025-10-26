source(file.path("correction", "methods", "common.R"))

prepare_method("MetaDICT")

run_method("MetaDICT", {
  suppressPackageStartupMessages({ library(MetaDICT); library(vegan) })
  O <- t(get_input_for("MetaDICT", base_M, base_form))
  meta <- transform(metadata[colnames(O), , drop = FALSE], batch = batch_id)
  D <- as.matrix(vegdist(O, method = "bray"))
  res <- MetaDICT(O, meta, distance_matrix = D, max_iter = 2000, customize_parameter = TRUE, alpha = 0.05, beta  = 0.20)
  out_pos <- t(res$count)
  write_tss_clr("MetaDICT", out_pos, "positive", "normalized_metadict.csv")
})

finalize_method()
