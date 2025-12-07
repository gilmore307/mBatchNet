source(file.path("correction", "correction.R"))

prepare_method("QN")

run_method("QN", {
  X_tss  <- as.matrix(get_input_for("QN", base_M, base_form))
  storage.mode(X_tss) <- "double"
  ref_tss <- X_tss[ref_idx, , drop = FALSE]
  if (nrow(ref_tss) < 2) warn_step("QN", "Reference batch has <2 samples; results may be unstable.")

  sorted_ref <- apply(ref_tss, 1, function(r) sort(r, na.last = TRUE))
  if (is.null(dim(sorted_ref))) sorted_ref <- matrix(sorted_ref, ncol = 1)
  target <- rowMeans(sorted_ref, na.rm = TRUE)

  qn_tss <- matrix(NA_real_, nrow(X_tss), ncol(X_tss), dimnames = dimnames(X_tss))
  for (i in seq_len(nrow(X_tss))) {
    xi  <- X_tss[i, ]
    o   <- order(xi, na.last = TRUE)
    out <- xi
    out[o] <- target
    qn_tss[i, ] <- out
  }
  write_tss_clr("QN", qn_tss, "positive", "normalized_qn.csv")
}, log_name = "Quantile Normalization")

finalize_method()
