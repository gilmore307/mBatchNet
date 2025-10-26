if (!exists(".CORRECTION_BOOTSTRAPPED", envir = .GlobalEnv, inherits = FALSE)) {
  source(file.path("correction", "correction.R"))
}

METHOD_CODE <- "PN"

percentile_norm <- function(data, batch, trt, ctrl.grp = 0, n_control_thresh = 10, otu_thresh = 0.3){
  X <- as.matrix(data)
  nb <- nrow(X); nf <- ncol(X)
  if (is.null(nf) || nf == 0) return(X)
  bfac <- as.factor(batch)
  out <- matrix(NA_real_, nrow = nb, ncol = nf, dimnames = dimnames(X))
  if (is.null(otu_thresh) || is.na(otu_thresh)) {
    ctrl_mask <- (trt == ctrl.grp)
    case_mask <- (trt != ctrl.grp)
    frac_ctrl_all <- if (any(ctrl_mask, na.rm = TRUE)) colMeans(X[ctrl_mask, , drop = FALSE] > 0, na.rm = TRUE) else rep(0, nf)
    frac_case_all <- if (any(case_mask, na.rm = TRUE)) colMeans(X[case_mask, , drop = FALSE] > 0, na.rm = TRUE) else rep(0, nf)
    m <- pmax(frac_ctrl_all, frac_case_all)
    q <- suppressWarnings(as.numeric(stats::quantile(m, probs = 0.25, na.rm = TRUE)))
    if (!is.finite(q)) q <- 0.3
    otu_thresh_eff <- min(0.50, max(0.10, q))
    if (exists("say")) try(say("PN: auto otu_thresh=", sprintf("%.2f", otu_thresh_eff)), silent = TRUE)
  } else {
    otu_thresh_eff <- otu_thresh
  }
  levs <- levels(bfac)
  for (b in levs){
    idx_b <- which(bfac == b)
    if (!length(idx_b)) next
    ctrl_idx <- idx_b[which(trt[idx_b] == ctrl.grp)]
    case_idx <- idx_b[which(trt[idx_b] != ctrl.grp)]
    ctrl_n <- length(ctrl_idx)
    use_batch_ecdf <- FALSE
    if (is.null(n_control_thresh) || is.na(n_control_thresh)) {
      if (ctrl_n >= 10) {
      } else if (ctrl_n >= 5) {
        if (exists("say")) try(say("PN: batch '", b, "' controls=", ctrl_n, ", below 10: proceeding with available controls."), silent = TRUE)
      } else {
        use_batch_ecdf <- TRUE
        if (exists("say")) try(say("PN: batch '", b, "' controls=", ctrl_n, " <5: fallback to batch ECDF."), silent = TRUE)
      }
    } else {
      if (ctrl_n < n_control_thresh) {
        use_batch_ecdf <- (ctrl_n < 5)
        if (!use_batch_ecdf) {
          if (exists("say")) try(say("PN: batch '", b, "' controls=", ctrl_n, ", below threshold ", n_control_thresh, ": proceeding with available controls."), silent = TRUE)
        } else {
          if (exists("say")) try(say("PN: batch '", b, "' controls=", ctrl_n, " <5: fallback to batch ECDF."), silent = TRUE)
        }
      }
    }
    keep <- rep(TRUE, nf)
    if (!is.na(otu_thresh_eff) && otu_thresh_eff > 0) {
      frac_ctrl <- if (length(ctrl_idx) > 0) colMeans(X[ctrl_idx, , drop = FALSE] > 0, na.rm = TRUE) else rep(0, nf)
      frac_case <- if (length(case_idx) > 0) colMeans(X[case_idx, , drop = FALSE] > 0, na.rm = TRUE) else rep(0, nf)
      keep <- (frac_ctrl >= otu_thresh_eff) | (frac_case >= otu_thresh_eff)
    }
    if (!any(keep)) next
    for (j in seq_len(nf)){
      if (!keep[j]) next
      vals_b <- X[idx_b, j]
      ctrl_vals <- if (!use_batch_ecdf) X[ctrl_idx, j] else vals_b
      finite_ctrl <- ctrl_vals[is.finite(ctrl_vals)]
      if (length(finite_ctrl) < 1 || sd(finite_ctrl) == 0){
        out[idx_b, j] <- 0.5
      } else {
        mpos <- suppressWarnings(min(finite_ctrl[finite_ctrl > 0], na.rm = TRUE))
        if (!is.finite(mpos) || mpos <= 0) mpos <- suppressWarnings(min(finite_ctrl, na.rm = TRUE))
        vb <- vals_b
        zix <- which(!is.finite(vb) | vb <= 0)
        if (length(zix) > 0 && is.finite(mpos) && mpos > 0) {
          vb[zix] <- runif(length(zix), min = 0, max = mpos * 0.5)
        } else if (length(zix) > 0) {
          vb[zix] <- .Machine$double.eps
        }
        ec <- ecdf(finite_ctrl)
        out[idx_b, j] <- as.numeric(ec(vb))
      }
    }
  }
  out[out <= 0 | is.na(out)] <- .Machine$double.eps
  out
}

run_correction_method <- function() {
  prepare_method(METHOD_CODE)

  run_method(METHOD_CODE, {
    if (!("phenotype" %in% colnames(metadata))) fail_step(METHOD_CODE, "'phenotype' is required.")
    pheno_vals <- unique(metadata$phenotype)
    if (length(pheno_vals) != 2) fail_step(METHOD_CODE, "'phenotype' must be binary.")
    if (!is.na(CONTROL_LABEL) && CONTROL_LABEL %in% as.character(metadata$phenotype)) {
      trt <- ifelse(as.character(metadata$phenotype) == CONTROL_LABEL, 0, 1)
    } else {
      trt <- as.numeric(factor(metadata$phenotype, levels = sort(pheno_vals))) - 1
    }
    X_tss <- get_input_for(METHOD_CODE, base_M, base_form)
    if (all(X_tss == 0)) fail_step(METHOD_CODE, "All zero after TSS.")

    pn_pos <- percentile_norm(
      data = X_tss,
      batch = metadata$batch_id,
      trt = trt,
      ctrl.grp = 0,
      n_control_thresh = NA,
      otu_thresh = NA
    )
    write_tss_clr(METHOD_CODE, pn_pos, "positive", "normalized_pn.csv")
  })

  finalize_method()
}

if (!exists(".CORRECTION_DISPATCH_ACTIVE", envir = .GlobalEnv, inherits = FALSE)) {
  run_correction_method()
}
