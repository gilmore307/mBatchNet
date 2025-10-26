source(file.path("correction", "correction.R"))

prepare_method("FAbatch")

run_method("FAbatch", {
  suppressPackageStartupMessages(library(bapred))
  if (!("phenotype" %in% colnames(metadata))) fail_step("FAbatch", "'phenotype' is required.")
  pheno_vals <- unique(metadata$phenotype)
  if (length(pheno_vals) != 2) fail_step("FAbatch", "'phenotype' must be binary.")
  X_log <- get_input_for("FAbatch", base_M, base_form)
  X_log <- t(apply(X_log, 1, function(r){
    r[!is.finite(r)] <- NA
    r[is.na(r)] <- mean(r, na.rm = TRUE)
    r
  }))
  y     <- factor(metadata$phenotype, levels = sort(pheno_vals))
  batch <- factor(metadata$batch_id)
  v  <- apply(X_log, 2, var)
  keep_var <- is.finite(v) & v > 1e-12
  if (!any(keep_var)) fail_step("FAbatch", "All features ~zero variance.")
  Xv <- X_log[, keep_var, drop = FALSE]
  max_nb <- max(table(batch))
  K      <- min(ncol(Xv), max_nb + 5L)
  if (K <= max_nb) {
    fail_step("FAbatch", sprintf("Need p > max batch size (have %d, need > %d).", ncol(Xv), max_nb))
  }
  ord <- order(apply(Xv, 2, var), decreasing = TRUE)
  sel <- ord[seq_len(K)]
  Xk  <- Xv[, sel, drop = FALSE]
  Xz <- scale(Xk, center = TRUE, scale = TRUE)
  fa_out <- tryCatch(
    fabatch(
      x = Xz, y = y, batch = batch,
      nbf = NULL, minerr = 1e-6, probcrossbatch = FALSE, maxiter = 100, maxnbf = 8
    ),
    error = function(e) e
  )
  if (inherits(fa_out, "error")) {
    warn_step("FAbatch", paste("Retry with tiny jitter:", conditionMessage(fa_out)))
    Xz <- Xz + matrix(rnorm(length(Xz), 0, 1e-8), nrow(Xz))
    fa_out <- fabatch(
      x = Xz, y = y, batch = batch,
      nbf = NULL, minerr = 1e-6, probcrossbatch = FALSE, maxiter = 100, maxnbf = 8
    )
  }
  Xadj <- X_log
  m <- attr(Xz, "scaled:center"); s <- attr(Xz, "scaled:scale")
  Xadj_sub <- sweep(fa_out$xadj, 2, s, `*`)
  Xadj_sub <- sweep(Xadj_sub, 2, m, `+`)
  Xadj[, colnames(Xk)] <- Xadj_sub
  write_tss_clr("FAbatch", Xadj, "log", "normalized_fabatch.csv")
})

finalize_method()
