source(file.path("correction", "methods", "common.R"))

prepare_method("FSQN")

run_method("FSQN", {
  X_tss  <- as.matrix(get_input_for("FSQN", base_M, base_form))
  storage.mode(X_tss) <- "double"

  ref_mask <- seq_len(nrow(X_tss)) %in% ref_idx
  ref_tss  <- X_tss[ref_mask, , drop = FALSE]
  if (nrow(ref_tss) < 2) warn_step("FSQN", "Reference batch has <2 samples; columns with <2 ref values will be left unchanged.")

  quantile_normalize_by_feature <- function(X, Xref) {
    n <- nrow(X); p <- ncol(X)
    out <- matrix(NA_real_, n, p, dimnames = dimnames(X))
    for (j in seq_len(p)) {
      x  <- X[, j]
      xr <- Xref[, j]
      xr <- xr[is.finite(xr)]
      if (length(xr) < 2L) { out[, j] <- x; next }
      xr <- sort(xr, na.last = TRUE)
      nref <- length(xr)

      idx <- which(is.finite(x))
      k <- length(idx)
      if (k == 0L) { out[, j] <- x; next }

      ord <- order(x[idx], na.last = NA)
      if (k == nref) {
        tgt <- xr
      } else {
        pref <- (seq_len(nref) - 0.5) / nref
        pk   <- (seq_len(k)    - 0.5) / k
        f <- stats::approxfun(pref, xr, rule = 2, ties = "ordered")
        tgt <- f(pk)
      }
      y <- x
      y[idx[ord]] <- tgt
      out[, j] <- y
    }
    rownames(out) <- rownames(X); colnames(out) <- colnames(X)
    out
  }

  out_tss <- quantile_normalize_by_feature(X_tss, ref_tss)
  write_tss_clr("FSQN", out_tss, "positive", "normalized_fsqn.csv")
})

finalize_method()
