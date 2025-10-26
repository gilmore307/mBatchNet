if (!exists(".CORRECTION_BOOTSTRAPPED", envir = .GlobalEnv, inherits = FALSE)) {
  source(file.path("correction", "correction.R"))
}

METHOD_CODE <- "SVD"

run_correction_method <- function() {
  prepare_method(METHOD_CODE)

  run_method(METHOD_CODE, {
    cat("Running SVD-based batch correction in log/CLR space...\n")
    X_log <- get_input_for(METHOD_CODE, base_M, base_form)
    X_log <- t(apply(X_log, 1, function(r){ r[!is.finite(r)] <- NA; r[is.na(r)] <- mean(r, na.rm = TRUE); r }))
    feature_sd <- apply(X_log, 2, sd)
    zero_var_cols <- which(feature_sd == 0)
    variable_cols <- setdiff(seq_len(ncol(X_log)), zero_var_cols)
    if (!length(variable_cols)) fail_step(METHOD_CODE, "All features zero variance.")
    Xv <- X_log[, variable_cols, drop = FALSE]
    mu <- colMeans(Xv); sdv <- apply(Xv, 2, sd)
    Z  <- scale(Xv, center = TRUE, scale = TRUE)
    s  <- svd(crossprod(Z))
    a1 <- s$u[,1]
    t1 <- Z %*% a1 / sqrt(drop(crossprod(a1)))
    c1 <- crossprod(Z, t1) / drop(crossprod(t1))
    Zdef <- Z - t1 %*% t(c1)
    Xrest <- sweep(Zdef, 2, sdv, `*`)
    Xrest <- sweep(Xrest, 2, mu, `+`)
    full <- X_log
    full[, variable_cols] <- Xrest
    if (length(zero_var_cols) > 0) full[, zero_var_cols] <- X_log[, zero_var_cols]
    write_tss_clr(METHOD_CODE, full, "log", "normalized_svd.csv")
  })

  finalize_method()
}

if (!exists(".CORRECTION_DISPATCH_ACTIVE", envir = .GlobalEnv, inherits = FALSE)) {
  run_correction_method()
}
