if (!exists(".CORRECTION_BOOTSTRAPPED", envir = .GlobalEnv, inherits = FALSE)) {
  source(file.path("correction", "correction.R"))
}

METHOD_CODE <- "DEBIAS"

run_correction_method <- function() {
  prepare_method(METHOD_CODE)

  run_method(METHOD_CODE, {
  suppressPackageStartupMessages(library(reticulate))
  py <- Sys.getenv("RETICULATE_PYTHON")
  if (!nzchar(py)) {
    cand <- Sys.which(c("python3", "python"))
    cand <- unname(cand[cand != ""])  
    if (length(cand) > 0) py <- cand[[1]]
  }
  if (!nzchar(py)) py <- "python"
  reticulate::use_python(py, required = TRUE)

  have_np <- py_module_available("numpy")
  have_dm <- py_module_available("debiasm")
  if (!have_np || !have_dm) {
    system2(py, c("-m","pip","install","--upgrade","pip"), stdout = TRUE, stderr = TRUE)
    if (!have_np) system2(py, c("-m","pip","install","numpy"), stdout = TRUE, stderr = TRUE)
    if (!have_dm) system2(py, c("-m","pip","install","DEBIAS-M"), stdout = TRUE, stderr = TRUE)
  }

  np <- import("numpy", delay_load = TRUE)
  debiasm <- import("debiasm", delay_load = TRUE)

  X_cnt <- tryCatch(
    get_input_for(METHOD_CODE, base_M, base_form),
    error = function(e) { say("DEBIAS: expected_input missing; converting to counts."); to_counts(base_M, base_form) }
  )
  X_cnt[!is.finite(X_cnt)] <- 0
  X_cnt[X_cnt < 0] <- 0
  if (!("phenotype" %in% colnames(metadata))) {
    fail_step(METHOD_CODE, "'phenotype' column required in metadata.")
  }
  y_all <- metadata$phenotype

  is_num <- is.numeric(y_all)
  uniq_vals <- unique(y_all[!is.na(y_all)])
  is_integerish <- is_num && all(abs(uniq_vals - round(uniq_vals)) < 1e-8)
  use_classifier <- (!is_num) || (is_integerish && length(uniq_vals) <= 10)

  b0 <- as.integer(factor(batch_id)) - 1L
  if (any(is.na(b0))) fail_step(METHOD_CODE, "Invalid batch IDs.")
  X_with_batch <- cbind(b0, round(X_cnt))

  uniq_b <- sort(unique(b0))
  if (length(uniq_b) >= 2) {
    val_batch <- tail(uniq_b, 1)
    val_inds  <- (b0 == val_batch)
  } else {
    set.seed(123)
    val_inds  <- rep(FALSE, nrow(X_with_batch))
    val_inds[sample.int(nrow(X_with_batch), max(1L, floor(0.2 * nrow(X_with_batch))))] <- TRUE
  }
  if (sum(!val_inds) < 2) {
    set.seed(123)
    val_inds  <- rep(FALSE, nrow(X_with_batch))
    val_inds[sample.int(nrow(X_with_batch), max(1L, floor(0.2 * nrow(X_with_batch))))] <- TRUE
  }

  X_train_R <- X_with_batch[!val_inds, , drop = FALSE]
  X_val_R   <- X_with_batch[val_inds, , drop = FALSE]
  y_train_R <- y_all[!val_inds]

  if (use_classifier && length(unique(y_train_R[!is.na(y_train_R)])) < 2 && length(unique(y_all)) >= 2) {
    set.seed(42)
    val_inds  <- rep(FALSE, nrow(X_with_batch))
    val_inds[sample.int(nrow(X_with_batch), max(1L, floor(0.2 * nrow(X_with_batch))))] <- TRUE
    X_train_R <- X_with_batch[!val_inds, , drop = FALSE]
    X_val_R   <- X_with_batch[val_inds, , drop = FALSE]
    y_train_R <- y_all[!val_inds]
  }

  X_train <- np$array(X_train_R, dtype = "int64")
  X_val   <- np$array(X_val_R,   dtype = "int64")
  X_full  <- np$array(X_with_batch, dtype = "int64")

  if (use_classifier) {
    yf <- as.factor(y_train_R)
    if (anyNA(yf)) { tab <- sort(table(yf), decreasing = TRUE); yf[is.na(yf)] <- names(tab)[1] }
    y_train <- np$array(as.integer(yf) - 1L, dtype = "int64")
    model <- debiasm$DebiasMClassifier(x_val = X_val)
  } else {
    yr <- as.numeric(y_train_R); if (anyNA(yr)) yr[is.na(yr)] <- mean(yr, na.rm = TRUE)
    y_train <- np$array(yr, dtype = "float32")
    model <- debiasm$DebiasMRegressor(x_val = X_val)
  }

  model$fit(X_train, y_train)
  X_debiased <- model$transform(X_full)
  Xd <- as.matrix(py_to_r(X_debiased))

  p_full <- ncol(X_with_batch)
  p_feat <- ncol(X_cnt)
  if (nrow(Xd) == p_full && ncol(Xd) == nrow(X_with_batch)) Xd <- t(Xd)

  if (ncol(Xd) == p_full) {
    out_counts <- Xd[, -1, drop = FALSE]
  } else if (ncol(Xd) == p_feat) {
    out_counts <- Xd
  } else {
    fail_step(METHOD_CODE, sprintf("Unexpected transform shape: got %d cols (p_feat=%d, p_full=%d).",
                                   ncol(Xd), p_feat, p_full))
  }

  dimnames(out_counts) <- dimnames(X_cnt)
  write_tss_clr(METHOD_CODE, out_counts, "counts", "normalized_debias.csv")
  })

  finalize_method()
}

if (!exists(".CORRECTION_DISPATCH_ACTIVE", envir = .GlobalEnv, inherits = FALSE)) {
  run_correction_method()
}
