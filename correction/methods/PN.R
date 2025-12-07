#!/usr/bin/env Rscript
# Percentile Normalization (PN) method in R
# Aligned in spirit with the Python q2-perc-norm implementation.

source(file.path("correction", "correction.R"))

prepare_method("PN")

percentile_norm <- function(data,
                            batch,
                            trt,
                            ctrl.grp = 0,
                            n_control_thresh = 10,
                            otu_thresh = 0.3) {
  # data: samples x features numeric matrix
  # batch: vector or factor giving batch for each sample
  # trt: numeric vector (0 = control, 1 = case)
  # ctrl.grp: value in `trt` that denotes controls (default 0)
  # n_control_thresh: minimum number of controls per batch
  # otu_thresh: minimum presence fraction in cases OR controls
  
  X  <- as.matrix(data)
  nb <- nrow(X)
  nf <- ncol(X)
  
  if (is.null(nf) || nf == 0L) {
    return(X)
  }
  
  # Build batch factor; if NULL, treat as a single batch
  bfac <- if (is.null(batch)) {
    factor(rep("batch1", nb))
  } else {
    as.factor(batch)
  }
  
  # Output matrix (samples x features)
  out <- matrix(NA_real_, nrow = nb, ncol = nf, dimnames = dimnames(X))
  
  # Use a fixed otu_thresh by default, as in the Python interface
  if (is.null(otu_thresh) || is.na(otu_thresh)) {
    otu_thresh_eff <- 0.3
  } else {
    otu_thresh_eff <- otu_thresh
  }
  
  batch_levels <- levels(bfac)
  
  for (b in batch_levels) {
    idx_b <- which(bfac == b)
    if (!length(idx_b)) next
    
    trt_b    <- trt[idx_b]
    ctrl_idx <- idx_b[trt_b == ctrl.grp]
    case_idx <- idx_b[trt_b != ctrl.grp]
    
    # 1) Require both controls and cases in this batch (as in the Python version)
    if (!length(ctrl_idx)) {
      stop(sprintf("PN: batch '%s' has no control samples.", b), call. = FALSE)
    }
    if (!length(case_idx)) {
      stop(sprintf("PN: batch '%s' has no case samples.", b), call. = FALSE)
    }
    
    # 2) Require at least n_control_thresh controls per batch
    ctrl_n <- length(ctrl_idx)
    if (!is.null(n_control_thresh) &&
        !is.na(n_control_thresh) &&
        ctrl_n < n_control_thresh) {
      
      stop(
        sprintf(
          "PN: batch '%s' has %d controls < n_control_thresh = %d.",
          b, ctrl_n, n_control_thresh
        ),
        call. = FALSE
      )
    }
    
    # 3) OTU filtering in this batch:
    #    keep features that are present in at least otu_thresh fraction
    #    of controls OR cases in this batch.
    keep <- rep(TRUE, nf)
    if (!is.na(otu_thresh_eff) && otu_thresh_eff > 0) {
      frac_ctrl <- if (length(ctrl_idx) > 0) {
        colMeans(X[ctrl_idx, , drop = FALSE] > 0, na.rm = TRUE)
      } else {
        rep(0, nf)
      }
      
      frac_case <- if (length(case_idx) > 0) {
        colMeans(X[case_idx, , drop = FALSE] > 0, na.rm = TRUE)
      } else {
        rep(0, nf)
      }
      
      keep <- (frac_ctrl >= otu_thresh_eff) | (frac_case >= otu_thresh_eff)
    }
    
    if (!any(keep)) next
    
    # 4) Percentile normalization within this batch
    for (j in seq_len(nf)) {
      if (!keep[j]) next
      
      vals_b    <- X[idx_b, j]
      ctrl_vals <- X[ctrl_idx, j]
      
      finite_ctrl <- ctrl_vals[is.finite(ctrl_vals)]
      
      # If no variability among controls (or no finite controls),
      # assign a neutral percentile (0.5) to all samples in this batch/feature.
      if (length(finite_ctrl) < 1L || stats::sd(finite_ctrl) == 0) {
        out[idx_b, j] <- 0.5
      } else {
        # Replace non-finite or non-positive values with small random noise
        # based on the minimum positive control value.
        mpos <- suppressWarnings(
          min(finite_ctrl[finite_ctrl > 0], na.rm = TRUE)
        )
        if (!is.finite(mpos) || mpos <= 0) {
          mpos <- suppressWarnings(min(finite_ctrl, na.rm = TRUE))
        }
        
        vb  <- vals_b
        zix <- which(!is.finite(vb) | vb <= 0)
        
        if (length(zix) > 0 && is.finite(mpos) && mpos > 0) {
          vb[zix] <- stats::runif(length(zix), min = 0, max = mpos * 0.5)
        } else if (length(zix) > 0) {
          vb[zix] <- .Machine$double.eps
        }
        
        # Empirical CDF over controls: maps each value to [0,1]
        ec <- stats::ecdf(finite_ctrl)
        out[idx_b, j] <- as.numeric(ec(vb))
      }
    }
  }
  
  # Ensure positive finite values
  out[!is.finite(out) | out <= 0] <- .Machine$double.eps
  out
}

# Integration with the correction framework
run_method("PN", {
  if (!("target_binary" %in% colnames(metadata))) {
    fail_step("PN", "'target_binary' column is required in metadata.")
  }
  
  pheno_vals <- unique(metadata$target_binary)
  if (length(pheno_vals) != 2) {
    fail_step("PN", "'target_binary' must have exactly 2 levels.")
  }
  
  # Map metadata to 0/1 (control / case) similarly to the Python logic
  if (!is.na(CONTROL_LABEL) &&
      CONTROL_LABEL %in% as.character(metadata$target_binary)) {
    
    trt <- ifelse(
      as.character(metadata$target_binary) == CONTROL_LABEL,
      0, 1
    )
  } else {
    # Fall back to sorted levels: first level -> 0, second level -> 1
    trt <- as.numeric(
      factor(metadata$target_binary, levels = sort(pheno_vals))
    ) - 1
  }
  
  # Input after TSS (samples x features)
  X_tss <- get_input_for("PN", base_M, base_form)
  if (all(X_tss == 0)) {
    fail_step("PN", "All values are zero after TSS.")
  }
  
  pn_pos <- percentile_norm(
    data             = X_tss,
    batch            = metadata$batch,
    trt              = trt,
    ctrl.grp         = 0,
    n_control_thresh = 10,   # match Python default
    otu_thresh       = 0.3   # match Python default
  )
  
  # Write out the normalized table
  write_tss_clr("PN", pn_pos, "positive", "normalized_pn.csv")
}, log_name = "Percentile Normalization")

finalize_method()
