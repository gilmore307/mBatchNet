# ---------------------------
# Handle Arguments
# ---------------------------
# Prevent accidental creation of Rplots.pdf in non-interactive runs.
# In batch mode, R's default device is pdf("Rplots.pdf"). If any code
# triggers a plot (even inside a package), that file appears in the CWD.
# Here we redirect the default device to the OS null sink.
if (!interactive()) {
  nullfile <- if (.Platform$OS.type == "windows") "NUL" else "/dev/null"
  options(device = function(...) grDevices::pdf(file = nullfile))
  # If a default device was already opened for any reason, close it.
  try({
    while (grDevices::dev.cur() > 1L) grDevices::dev.off()
  }, silent = TRUE)
}
# All Methods: FSQN, QN, BMC, limma, ConQuR, PLSDA, ComBat, MMUPHin, RUV, MetaDICT, SVD, PN, FAbatch, ComBatSeq, DEBIAS
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  args <- c("FSQN, QN, BMC, limma, ConQuR, PLSDA, ComBat, MMUPHin, RUV, MetaDICT, SVD, PN, FAbatch, ComBatSeq, DEBIAS", "output/example")
}

# Split and trim whitespace so names match exactly
method_list   <- trimws(unlist(strsplit(args[1], ",")))
# Drop empty tokens
method_list   <- method_list[nzchar(method_list)]
output_folder <- args[2]
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

# ---------------------------
# Load Libraries / Session summary helpers
# ---------------------------
# Keep top-level imports minimal; method-specific packages are loaded in each block below.

# Session summary file (written incrementally so Python can aggregate later)
summary_file <- file.path(output_folder, "session_summary.json")
.can_json <- suppressWarnings(requireNamespace("jsonlite", quietly = TRUE))
.session_summary <- list(
  session   = basename(output_folder),
  started   = as.character(Sys.time()),
  n_samples = NA_integer_,
  n_features= NA_integer_,
  methods   = list()
)
record_methods <- c(
  "QN","BMC","limma","ConQuR","PLSDAbatch","ComBat","FSQN","MMUPHin",
  "fastRUV-III-NB","MetaDICT","SVD","PN","FAbatch","ComBat-Seq","DEBIAS"
)
save_summary <- function(){
  if (.can_json) {
    try(jsonlite::write_json(.session_summary, summary_file, auto_unbox = TRUE, pretty = TRUE), silent = TRUE)
  }
}

# ---- Read session config (optional control label, reference batch) ----
CONTROL_LABEL <- NA_character_
REFERENCE_BATCH <- NA_character_
try({
  if (.can_json) {
    cfg_path <- file.path(output_folder, "session_config.json")
    if (file.exists(cfg_path)) {
      cfg <- jsonlite::fromJSON(cfg_path)
      if (!is.null(cfg$control_label) && nzchar(as.character(cfg$control_label))) {
        CONTROL_LABEL <- as.character(cfg$control_label)
      }
      if (!is.null(cfg$reference_batch) && nzchar(as.character(cfg$reference_batch))) {
        REFERENCE_BATCH <- as.character(cfg$reference_batch)
      }
    }
  }
}, silent = TRUE)

# ---------------------------
# Source preprocess.r (helpers: say(), write_tss_clr(), detect_input_form(),
# to_tss(), to_log(), to_clr(), to_counts(), check_table(), post_summary(), ...)
# Ensure preprocess.R uses our intended output/matrix paths (avoid commandArgs hijack)
# ---------------------------
assign("PREPROC_OUTPUT_DIR", output_folder, envir = .GlobalEnv)
assign("PREPROC_MATRIX_PATH", file.path(output_folder, "raw.csv"), envir = .GlobalEnv)
source("preprocess.R")

# If preprocess.r did not define the logger for some reason, provide a minimal one
if (!exists("say", mode = "function")) {
  .ts <- function() format(Sys.time(), "%H:%M:%S")
  say <- function(...){ cat(sprintf("[%s] ", .ts()), paste0(..., collapse=""), "\n") }
  start_step <- function(name){ say("▶️ START: ", name); proc.time() }
  ok_step    <- function(name, t0){ dt <- proc.time()-t0; say("✅ DONE:  ", name, sprintf(" (%.2fs)", dt["elapsed"])) }
  warn_step  <- function(name, msg){ say("⚠️ WARN:  ", name, " — ", msg) }
  fail_step  <- function(name, msg){ say("❌ FAIL:  ", name, " — ", msg); stop(paste0(name, ": ", msg)) }
}

# ---------------------------
# Helper to run a step with logging
# ---------------------------
run_method <- function(name, expr){
  t0 <- start_step(name)
  out <- withCallingHandlers(
    tryCatch({
      val <- force(expr)
      ok_step(name, t0)
      # Record successful method runtime (only for real methods)
      if (name %in% record_methods) {
        dt <- as.numeric((proc.time() - t0)["elapsed"])
        .session_summary$methods[[length(.session_summary$methods) + 1L]] <<- list(
          name = name,
          status = "success",
          elapsed_sec = round(dt, 3),
          finished = as.character(Sys.time())
        )
        save_summary()
      }
      val
    }, error = function(e){
      fail_step(name, conditionMessage(e))
    }),
    warning = function(w){ warn_step(name, conditionMessage(w)) }
  )
  invisible(out)
}

# ---------------------------
# Desired inputs per method (uses converters from preprocess.r)
# ---------------------------
expected_input <- list(
  QN        = "tss",
  BMC       = "log",
  limma     = "log",
  ConQuR    = "counts",
  PLSDA     = "clr",
  ComBat    = "log",
  FSQN      = "tss",
  MMUPHin   = "tss",
  RUV       = "counts",
  MetaDICT  = "tss",
  SVD       = "log",
  PN        = "tss",
  FAbatch   = "log",
  ComBatSeq = "counts",
  DEBIAS    = "counts"
)

# get input matrix for a method given a base matrix and its form
get_input_for <- function(method, base_M, base_form) {
  target <- expected_input[[method]]
  if (is.null(target)) stop("Unknown method in expected_input: ", method)
  if (target == base_form) return(base_M)
  say(sprintf("🔄 Convert for %s: %s → %s", method, base_form, target))
  out <- switch(
    target,
    "tss"    = to_tss(base_M, base_form),
    "log"    = to_log(base_M, base_form),
    "clr"    = to_clr(base_M, base_form),
    "counts" = to_counts(base_M, base_form),
    stop("Unhandled target: ", target)
  )
  say(sprintf("   ↳ done (%s → %s) | n=%d×%d", base_form, target, nrow(out), ncol(out)))
  out
}

# ---------------------------
# Load Data
# ---------------------------
custom_matrix_path    <- file.path(output_folder, "raw.csv")
custom_metadata_path  <- file.path(output_folder, "metadata.csv")
default_matrix_path   <- file.path("assets/example", "raw_1.csv")
default_metadata_path <- file.path("assets/example", "metadata_1.csv")

if (file.exists(custom_matrix_path) && file.exists(custom_metadata_path)) {
  say("✅ Using uploaded user files")
  uploaded_mat <- read.csv(custom_matrix_path, header = FALSE, check.names = FALSE)
  metadata     <- read.csv(custom_metadata_path, check.names = FALSE)
} else {
  say("⚠️ No uploaded files found — using default assets")
  uploaded_mat <- read.csv(default_matrix_path, header = FALSE, check.names = FALSE)
  metadata     <- read.csv(default_metadata_path, check.names = FALSE)
}

# ---------------------------
# Prepare INPUT
# ---------------------------
check_table(uploaded_mat, "uploaded_mat", allow_negative = TRUE)
input_form <- detect_input_form(uploaded_mat)
say("ℹ️ Detected input form: ", input_form)

if (!("sample_id" %in% colnames(metadata))) {
  metadata$sample_id <- sprintf("S%03d", seq_len(nrow(metadata)))
}

# Make duplicate sample IDs unique (append _1, _2, ...)
metadata <- make_unique_sample_ids(metadata, id_col = "sample_id", sep = "_")
if (nrow(uploaded_mat) != nrow(metadata))
  fail_step("Alignment", "Row count mismatch between matrix and metadata.")
rownames(uploaded_mat) <- metadata$sample_id
rownames(metadata)     <- metadata$sample_id

base_M     <- as.matrix(uploaded_mat)
base_form  <- input_form

# Save dataset size in summary (samples x features)
try({
  .session_summary$n_samples <- nrow(base_M)
  .session_summary$n_features <- ncol(base_M)
  save_summary()
}, silent = TRUE)

# Factor batch, covariates
metadata$batch_id <- factor(metadata$batch_id, levels = unique(metadata$batch_id))
batch_id <- metadata$batch_id

covar <- metadata[, !(colnames(metadata) %in% c("sample_id","batch_id","phenotype")), drop = FALSE]
covar <- as.data.frame(lapply(covar, function(col) {
  if (is.numeric(col))      col[is.na(col)] <- mean(col, na.rm = TRUE)
  else if (is.factor(col))  { if (anyNA(col)) col[is.na(col)] <- levels(col)[1] }
  else                      { if (anyNA(col)) { kept <- suppressWarnings(as.character(stats::na.omit(col))); if (length(kept)) col[is.na(col)] <- kept[1] } }
  col
}))

# Reference batch for methods needing it (allow override from session config)
lvl <- levels(batch_id)
if (!is.na(REFERENCE_BATCH) && REFERENCE_BATCH %in% lvl) {
  reference_batch <- REFERENCE_BATCH
} else {
  reference_batch <- lvl[1]
}
ref_idx <- which(batch_id == reference_batch)

# ---------------------------
# Global checks
# ---------------------------
run_method("Input checks", {
  check_table(base_M, "base_M (detected input)", allow_negative = TRUE)
  check_table(metadata, "metadata", allow_negative = TRUE)
  if (!identical(rownames(base_M), rownames(metadata))) {
    fail_step("Alignment", "rownames(base_M) != rownames(metadata).")
  }
  say("Method list: ", paste(method_list, collapse=", "))
  say("Samples: ", nrow(base_M), " | Features: ", ncol(base_M))
  say("Reference batch level: ", as.character(reference_batch),
      " | #ref samples: ", length(ref_idx))
  if (length(ref_idx) < 1) warn_step("Reference", "No samples in reference batch level.")
})

# ---------------------------
# Methods (uses get_input_for + write_tss_clr from preprocess.r)
# ---------------------------

# QN — expects TSS
if ("QN" %in% method_list) {
  run_method("QN", {
    X_tss  <- as.matrix(get_input_for("QN", base_M, base_form))   # rows = samples
    storage.mode(X_tss) <- "double"
    ref_tss <- X_tss[ref_idx, , drop = FALSE]
    if (nrow(ref_tss) < 2) warn_step("QN", "Reference batch has <2 samples; results may be unstable.")
    
    # Build reference target: average of each ref sample's sorted feature vector
    # sorted_ref: features x (#ref samples)
    sorted_ref <- apply(ref_tss, 1, function(r) sort(r, na.last = TRUE))
    if (is.null(dim(sorted_ref))) sorted_ref <- matrix(sorted_ref, ncol = 1)
    target <- rowMeans(sorted_ref, na.rm = TRUE)   # length = ncol(X_tss)
    
    # Map each sample's ranks to the target
    qn_tss <- matrix(NA_real_, nrow(X_tss), ncol(X_tss), dimnames = dimnames(X_tss))
    for (i in seq_len(nrow(X_tss))) {
      xi  <- X_tss[i, ]
      o   <- order(xi, na.last = TRUE)    # indices of features sorted within the sample
      out <- xi
      out[o] <- target
      qn_tss[i, ] <- out
    }
    write_tss_clr("QN", qn_tss, "positive", "normalized_qn.csv")
  })
}

# BMC — expects log(TSS)
if ("BMC" %in% method_list) {
  run_method("BMC", {
    require(pamr)
    X_log  <- get_input_for("BMC", base_M, base_form)
    pam_in <- list(x = as.matrix(t(X_log)), batchlabels = factor(batch_id))
    adj_log <- t(pamr.batchadjust(pam_in)$x)
    write_tss_clr("BMC", adj_log, "log", "normalized_bmc.csv")
  })
}

# limma — expects log
if ("limma" %in% method_list) {
  run_method("limma", {
    require(limma)
    X_log <- get_input_for("limma", base_M, base_form)
    adj_t <- removeBatchEffect(
      t(X_log),
      batch = factor(batch_id),
      covariates = if (ncol(covar) > 0) as.matrix(covar) else NULL
    )
    adj <- t(adj_t)
    write_tss_clr("limma", adj, "log", "normalized_limma.csv")
  })
}

# ConQuR — expects counts
if ("ConQuR" %in% method_list) {
  run_method("ConQuR", {
    suppressPackageStartupMessages({ library(ConQuR); library(doParallel) })
    X_cnt <- get_input_for("ConQuR", base_M, base_form)
    covariates <- metadata[, colnames(covar), drop = FALSE]; rownames(covariates) <- NULL
    num_cores <- max(1, parallel::detectCores(TRUE) - 1)
    res_pos <- suppressWarnings(
      ConQuR(
        tax_tab = X_cnt,
        batchid = as.factor(metadata$batch_id),
        covariates = covariates,
        batch_ref = as.character(reference_batch),
        logistic_lasso = FALSE, quantile_type = "standard", simple_match = FALSE,
        lambda_quantile = "2p/n", interplt = FALSE, delta = 0.4999,
        taus = seq(0.05, 0.95, by = 0.05), num_core = num_cores
      )
    )
    write_tss_clr("ConQuR", res_pos, "positive", "normalized_conqur.csv")
  })
}

# PLSDA — expects CLR
if ("PLSDA" %in% method_list) {
  run_method("PLSDAbatch", {
    require(PLSDAbatch)
    if (!("phenotype" %in% colnames(metadata))) fail_step("PLSDAbatch", "'phenotype' not found.")
    if (length(unique(metadata$phenotype)) != 2) fail_step("PLSDAbatch", "'phenotype' must be binary.")
    X_clr <- get_input_for("PLSDA", base_M, base_form)
    res <- PLSDA_batch(
      X = X_clr,
      Y.trt = as.factor(metadata$phenotype),
      Y.bat = as.factor(metadata$batch_id),
      ncomp.trt = 1, ncomp.bat = 5
    )
    write_tss_clr("PLSDAbatch", res$X.nobatch, "clr", "normalized_plsda.csv")
  })
}

# ComBat — expects log
if ("ComBat" %in% method_list) {
  run_method("ComBat", {
    require(sva)
    X_log <- get_input_for("ComBat", base_M, base_form)
    adj_t <- ComBat(
      dat = t(X_log),
      batch = batch_id,
      mod = if (ncol(covar) > 0) model.matrix(~ ., data = covar) else NULL,
      par.prior = FALSE, prior.plots = FALSE
    )
    adj <- t(adj_t)
    write_tss_clr("ComBat", adj, "log", "normalized_combat.csv")
  })
}

# FSQN — expects TSS
if ("FSQN" %in% method_list) {
  run_method("FSQN", {
    # Input should be TSS with rows = samples, cols = features
    X_tss  <- as.matrix(get_input_for("FSQN", base_M, base_form))
    storage.mode(X_tss) <- "double"
    
    # Reference = samples in the chosen reference batch
    ref_mask <- seq_len(nrow(X_tss)) %in% ref_idx
    ref_tss  <- X_tss[ref_mask, , drop = FALSE]
    if (nrow(ref_tss) < 2) warn_step("FSQN", "Reference batch has <2 samples; columns with <2 ref values will be left unchanged.")
    
    # Pure-R FSQN (handles n_ref != n via quantile interpolation)
    quantile_normalize_by_feature <- function(X, Xref) {
      n <- nrow(X); p <- ncol(X)
      out <- matrix(NA_real_, n, p, dimnames = dimnames(X))
      for (j in seq_len(p)) {
        x  <- X[, j]
        xr <- Xref[, j]
        xr <- xr[is.finite(xr)]
        if (length(xr) < 2L) { out[, j] <- x; next }  # not enough ref points
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
}

# MMUPHin — expects TSS
if ("MMUPHin" %in% method_list) {
  run_method("MMUPHin", {
    require(MMUPHin)
    X_tss <- get_input_for("MMUPHin", base_M, base_form)
    feat_counts <- t(round(X_tss * 1e6))  # features x samples
    fit <- adjust_batch(
      feature_abd = feat_counts,
      batch       = "batch_id",
      covariates  = colnames(covar),
      data        = transform(metadata, batch_id=factor(batch_id)),
      control     = list(verbose = FALSE, diagnostic_plot = NULL)
    )
    out_pos <- t(fit$feature_abd_adj)
    write_tss_clr("MMUPHin", out_pos, "positive", "normalized_mmuphin.csv")
  })
}

# RUV-III-NB — expects counts; use the *counts* assay for outputs
if ("RUV" %in% method_list) {
  run_method("RUV-III-NB", {
    suppressPackageStartupMessages({
      library(DescTools)   # ruvIII.nb calls DescTools::Winsorize internally
      library(ruvIIInb)
      library(Matrix)
    })
    
    # ---- Session-level shim: make DescTools::Winsorize accept probs/na.rm ----
    patch_DescTools_Winsorize <- function() {
      orig <- getFromNamespace("Winsorize", "DescTools")
      if (!exists(".DescTools_Winsorize_backup", envir = .GlobalEnv, inherits = FALSE)) {
        assign(".DescTools_Winsorize_backup", orig, envir = .GlobalEnv)
      }
      compat <- function(x, probs = c(0.05, 0.95), na.rm = FALSE, ...) {
        orig_sig <- names(formals(get(".DescTools_Winsorize_backup", envir = .GlobalEnv)))
        if (all(c("probs", "na.rm") %in% orig_sig)) {
          return(get(".DescTools_Winsorize_backup", envir = .GlobalEnv)(x, probs = probs, na.rm = na.rm, ...))
        }
        qs <- stats::quantile(x, probs = probs, na.rm = na.rm, names = FALSE, type = 7)
        lo <- qs[1]; hi <- qs[2]
        x[x < lo] <- lo; x[x > hi] <- hi
        x
      }
      utils::assignInNamespace("Winsorize", compat, ns = "DescTools")
      invisible(TRUE)
    }
    patch_DescTools_Winsorize()
    
    # ---- Stable ruvIII.nb path (single-core, no pseudosamples) ----
    Sys.setenv(OMP_NUM_THREADS = "1", MKL_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1")
    
    # 1) Build genes x samples matrix and sanitize to integer counts
    Y <- t(get_input_for("RUV", base_M, base_form))  # genes x samples
    if (is.null(colnames(Y))) colnames(Y) <- rownames(metadata)
    storage.mode(Y) <- "double"
    Y[!is.finite(Y) | Y < 0] <- 0
    Y <- round(Y); storage.mode(Y) <- "integer"
    say("RUV: Y prepared")
    
    samp_ids <- colnames(Y)
    
    # 2) Align metadata / batch
    if (is.null(rownames(metadata)))
      fail_step("RUV", "metadata rownames must match colnames (currently NULL).")
    in_meta <- !is.na(samp_ids) & (samp_ids %in% rownames(metadata))
    if (!isTRUE(all(in_meta))) {
      dropped <- samp_ids[!in_meta]
      if (length(dropped)) message("Dropping samples missing in metadata: ", paste(dropped, collapse = ", "))
      Y <- Y[, in_meta, drop = FALSE]
      samp_ids <- colnames(Y)
    }
    if (!("batch_id" %in% colnames(metadata)))
      fail_step("RUV", "metadata must contain 'batch_id' column.")
    
    batch_vec <- metadata[samp_ids, "batch_id"]
    if (anyNA(batch_vec)) {
      bad <- samp_ids[is.na(batch_vec)]
      message("Dropping samples with NA batch_id: ", paste(bad, collapse = ", "))
      keep <- !is.na(batch_vec)
      Y <- Y[, keep, drop = FALSE]
      samp_ids <- colnames(Y)
      batch_vec <- batch_vec[keep]
    }
    batch_factor <- droplevels(factor(batch_vec))
    if (nlevels(batch_factor) < 1L)
      fail_step("RUV", "No valid batch levels after cleaning.")
    
    # 3) Drop all-zero genes and zero-library samples
    keep_rows <- rowSums(Y, na.rm = TRUE) > 0L
    if (!isTRUE(any(keep_rows)))
      fail_step("RUV", "All genes have zero or non-finite counts after cleaning.")
    Y <- Y[keep_rows, , drop = FALSE]
    
    lib <- colSums(Y, na.rm = TRUE)
    if (any(!is.finite(lib) | lib <= 0L)) {
      drop <- which(!is.finite(lib) | lib <= 0L)
      message("Dropping zero-library samples: ", paste(colnames(Y)[drop], collapse = ", "))
      Y <- Y[, lib > 0L & is.finite(lib), drop = FALSE]
      samp_ids <- colnames(Y)
      batch_factor <- droplevels(factor(metadata[samp_ids, "batch_id"]))
      if (nlevels(batch_factor) < 1L)
        fail_step("RUV", "No valid batch levels after zero-library drop.")
    }
    
    # 4) Build replicate matrix M (cells x groups) via one-hot batch encoding
    M <- model.matrix(~ 0 + batch_factor)
    rownames(M) <- samp_ids
    cs <- colSums(M); if (any(cs == 0L)) M <- M[, cs > 0L, drop = FALSE]
    rs <- rowSums(M)
    if (any(rs == 0L)) {
      bad_rows <- which(rs == 0L)
      message("Dropping unannotated cells (zero rows in M): ", paste(rownames(M)[bad_rows], collapse = ", "))
      M <- M[rs > 0L, , drop = FALSE]
      Y <- Y[, rownames(M), drop = FALSE]
      samp_ids <- colnames(Y)
      batch_factor <- droplevels(factor(metadata[samp_ids, "batch_id"]))
    }
    
    # 5) Control set (use all genes by default)
    ctl_names <- rownames(Y)
    say("RUV: design ready (ruvIII.nb)")
    
    # 6) Fit (single core; stable settings)
    fit <- ruvIIInb::ruvIII.nb(
      Y = as.matrix(Y),
      M = as.matrix(M),
      ctl = ctl_names,
      k = 2,
      batch = as.numeric(batch_factor),
      ncores = 1L,
      use.pseudosample = FALSE,
      batch.disp = FALSE,
      zeroinf = rep(FALSE, ncol(Y))
    )
    say("RUV: fit complete (ruvIII.nb)")
    
    # 7) Emit counts (samples x genes/features)
    out_counts <- t(as.matrix(fit$counts))
    write_tss_clr("RUV-III-NB", out_counts, "counts", "normalized_ruv.csv")
  })
}

# MetaDICT — expects TSS
if ("MetaDICT" %in% method_list) {
  run_method("MetaDICT", {
    suppressPackageStartupMessages({ library(MetaDICT); library(vegan) })
    O <- t(get_input_for("MetaDICT", base_M, base_form))  # samples x features for vegdist
    meta <- transform(metadata[colnames(O), , drop = FALSE], batch = batch_id)
    D <- as.matrix(vegdist(O, method = "bray"))
    res <- MetaDICT(O, meta, distance_matrix = D, max_iter = 2000, customize_parameter = TRUE, alpha = 0.05, beta  = 0.20,)
    out_pos <- t(res$count)
    write_tss_clr("MetaDICT", out_pos, "positive", "normalized_metadict.csv")
  })
}

# SVD — expects log
if ("SVD" %in% method_list) {
  run_method("SVD", {
    cat("Running SVD-based batch correction in log/CLR space...\n")
    X_log <- get_input_for("SVD", base_M, base_form)
    # Replace non-finite with row means
    X_log <- t(apply(X_log, 1, function(r){ r[!is.finite(r)] <- NA; r[is.na(r)] <- mean(r, na.rm = TRUE); r }))
    # Identify variable features
    feature_sd <- apply(X_log, 2, sd)
    zero_var_cols <- which(feature_sd == 0)
    variable_cols <- setdiff(seq_len(ncol(X_log)), zero_var_cols)
    if (!length(variable_cols)) fail_step("SVD", "All features zero variance.")
    Xv <- X_log[, variable_cols, drop = FALSE]
    mu <- colMeans(Xv); sdv <- apply(Xv, 2, sd)
    Z  <- scale(Xv, center = TRUE, scale = TRUE)
    # Remove first principal component (surrogate/batch component)
    s  <- svd(crossprod(Z))
    a1 <- s$u[,1]
    t1 <- Z %*% a1 / sqrt(drop(crossprod(a1)))
    c1 <- crossprod(Z, t1) / drop(crossprod(t1))
    Zdef <- Z - t1 %*% t(c1)
    # Rescale back to log space
    Xrest <- sweep(Zdef, 2, sdv, `*`)
    Xrest <- sweep(Xrest, 2, mu, `+`)
    full <- X_log
    full[, variable_cols] <- Xrest
    if (length(zero_var_cols) > 0) full[, zero_var_cols] <- X_log[, zero_var_cols]
    write_tss_clr("SVD", full, "log", "normalized_svd.csv")
  })
}

# PN — expects TSS;
if ("PN" %in% method_list) {
percentile_norm <- function(data, batch, trt, ctrl.grp = 0, n_control_thresh = 10, otu_thresh = 0.3){
  # data: samples x features (numeric matrix or data.frame convertible)
  # batch: factor/character vector length nrow(data)
  # trt: numeric/character vector length nrow(data); controls identified by ctrl.grp
  X <- as.matrix(data)
  nb <- nrow(X); nf <- ncol(X)
  if (is.null(nf) || nf == 0) return(X)
  bfac <- as.factor(batch)
  out <- matrix(NA_real_, nrow = nb, ncol = nf, dimnames = dimnames(X))
  # Auto-select otu_thresh if NA: based on global presence across cases/controls
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
    # Auto-select control threshold if NA: use 10 when enough, else relax; <5 fallback to batch ECDF
    use_batch_ecdf <- FALSE
    if (is.null(n_control_thresh) || is.na(n_control_thresh)) {
      if (ctrl_n >= 10) {
        # ok
      } else if (ctrl_n >= 5) {
        if (exists("say")) try(say("PN: batch '", b, "' controls=", ctrl_n, ", below 10: proceeding with available controls."), silent = TRUE)
      } else {
        use_batch_ecdf <- TRUE
        if (exists("say")) try(say("PN: batch '", b, "' controls=", ctrl_n, " <5: fallback to batch ECDF."), silent = TRUE)
      }
    } else {
      # fixed threshold behavior
      if (ctrl_n < n_control_thresh) {
        # For strict behavior, stop; but prefer graceful fallback
        use_batch_ecdf <- (ctrl_n < 5)
        if (!use_batch_ecdf) {
          if (exists("say")) try(say("PN: batch '", b, "' controls=", ctrl_n, ", below threshold ", n_control_thresh, ": proceeding with available controls."), silent = TRUE)
        } else {
          if (exists("say")) try(say("PN: batch '", b, "' controls=", ctrl_n, " <5: fallback to batch ECDF."), silent = TRUE)
        }
      }
    }
    # Feature filter: keep OTUs present in at least otu_thresh fraction of cases OR controls
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
        # Replace zeros and non-finite with small random values to break ties
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
    # Ensure strictly positive (avoid exact 0)
    out[out <= 0 | is.na(out)] <- .Machine$double.eps
    out
  }

  run_method("PN", {
    if (!("phenotype" %in% colnames(metadata))) fail_step("PN", "'phenotype' is required.")
    pheno_vals <- unique(metadata$phenotype)
    if (length(pheno_vals) != 2) fail_step("PN", "'phenotype' must be binary.")
    # Build treatment vector using user-selected control label if provided
    if (!is.na(CONTROL_LABEL) && CONTROL_LABEL %in% as.character(metadata$phenotype)) {
      trt <- ifelse(as.character(metadata$phenotype) == CONTROL_LABEL, 0, 1)
    } else {
      trt <- as.numeric(factor(metadata$phenotype, levels = sort(pheno_vals))) - 1
    }
    X_tss <- get_input_for("PN", base_M, base_form)
    if (all(X_tss == 0)) fail_step("PN", "All zero after TSS.")

    # Use pure R control-based ECDF percentile normalization with thresholds and per-batch processing
    pn_pos <- percentile_norm(
      data = X_tss,
      batch = metadata$batch_id,
      trt = trt,
      ctrl.grp = 0,
      n_control_thresh = NA,
      otu_thresh = NA
    )
    write_tss_clr("PN", pn_pos, "positive", "normalized_pn.csv")
  })
}

# FAbatch — expects log
if ("FAbatch" %in% method_list) {
  run_method("FAbatch", {
    suppressPackageStartupMessages(library(bapred))
    if (!("phenotype" %in% colnames(metadata))) fail_step("FAbatch", "'phenotype' is required.")
    pheno_vals <- unique(metadata$phenotype)
    if (length(pheno_vals) != 2) fail_step("FAbatch", "'phenotype' must be binary.")
    X_log <- get_input_for("FAbatch", base_M, base_form)
    # replace non-finite per row
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
}

# ComBat-Seq — expects counts
if ("ComBatSeq" %in% method_list) {
  run_method("ComBat-Seq", {
    require(sva)
    if (!("phenotype" %in% colnames(metadata))) fail_step("ComBat-Seq", "'phenotype' is required.")
    counts <- get_input_for("ComBatSeq", base_M, base_form)
    libsz <- rowSums(counts); keep <- libsz > 0
    if (any(!keep)) {
      say("ComBat-Seq: removing ", sum(!keep), " samples with zero library size.")
      counts  <- counts[keep, , drop = FALSE]
      metadata <- metadata[keep, , drop = FALSE]
    }
    if (!all(rownames(counts) == rownames(metadata))) fail_step("ComBat-Seq", "Sample IDs mismatch after filtering.")
    adj <- ComBat_seq(counts = t(counts), batch = metadata$batch_id, group = metadata$phenotype)
    out_counts <- t(adj)
    write_tss_clr("ComBat-Seq", out_counts, "counts", "normalized_combatseq.csv")
  })
}

# DEBIAS — Python package via reticulate; expects counts + phenotype; writes TSS+CLR
if ("DEBIAS" %in% method_list) {
  run_method("DEBIAS", {
    suppressPackageStartupMessages(library(reticulate))
    py <- Sys.getenv("RETICULATE_PYTHON")
    if (!nzchar(py)) {
      cand <- Sys.which(c("python3", "python"))
      cand <- unname(cand[cand != ""])  # drop blanks
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
      get_input_for("DEBIAS", base_M, base_form),
      error = function(e) { say("DEBIAS: expected_input missing; converting to counts."); to_counts(base_M, base_form) }
    )
    X_cnt[!is.finite(X_cnt)] <- 0
    X_cnt[X_cnt < 0] <- 0
    if (!("phenotype" %in% colnames(metadata))) {
      fail_step("DEBIAS", "'phenotype' column required in metadata.")
    }
    y_all <- metadata$phenotype
    
    # classifier vs regressor
    is_num <- is.numeric(y_all)
    uniq_vals <- unique(y_all[!is.na(y_all)])
    is_integerish <- is_num && all(abs(uniq_vals - round(uniq_vals)) < 1e-8)
    use_classifier <- (!is_num) || (is_integerish && length(uniq_vals) <= 10)
    
    # batch in first col (0-based), then counts
    b0 <- as.integer(factor(batch_id)) - 1L
    if (any(is.na(b0))) fail_step("DEBIAS", "Invalid batch IDs.")
    X_with_batch <- cbind(b0, round(X_cnt))
    
    # split (prefer a held-out batch); ensure >=2 train rows
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
    
    # numpy arrays
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
    
    # robust shape handling
    p_full <- ncol(X_with_batch)   # 1 + features
    p_feat <- ncol(X_cnt)
    if (nrow(Xd) == p_full && ncol(Xd) == nrow(X_with_batch)) Xd <- t(Xd)
    
    if (ncol(Xd) == p_full) {
      out_counts <- Xd[, -1, drop = FALSE]          # includes batch -> drop it
    } else if (ncol(Xd) == p_feat) {
      out_counts <- Xd                               # features-only
    } else {
      fail_step("DEBIAS", sprintf("Unexpected transform shape: got %d cols (p_feat=%d, p_full=%d).",
                                  ncol(Xd), p_feat, p_full))
    }
    
    dimnames(out_counts) <- dimnames(X_cnt)
    write_tss_clr("DEBIAS", out_counts, "counts", "normalized_debias.csv")
  })
}

# ---------------------------
# Output audit
# ---------------------------
expected_outputs <- list(
  QN="normalized_qn.csv", BMC="normalized_bmc.csv", limma="normalized_limma.csv",
  ConQuR="normalized_conqur.csv", PLSDA="normalized_plsda.csv", ComBat="normalized_combat.csv",
  FSQN="normalized_fsqn.csv", MMUPHin="normalized_mmuphin.csv",
  RUV="normalized_ruv.csv", MetaDICT="normalized_metadict.csv", SVD="normalized_svd.csv",
  PN="normalized_pn.csv", FAbatch="normalized_fabatch.csv", ComBatSeq="normalized_combatseq.csv",
  DEBIAS = "normalized_debias.csv"
)

audit_outputs <- function(selected, out_dir) {
  say("— Output audit (TSS & CLR) —")
  for (m in selected) {
    fname <- expected_outputs[[m]]
    if (is.null(fname)) { say("  ", m, ": (no declared output)"); next }
    base <- sub("\\.csv$", "", fname, ignore.case = TRUE)
    p_tss <- file.path(out_dir, paste0(base, "_tss.csv"))
    p_clr <- file.path(out_dir, paste0(base, "_clr.csv"))
    
    if (file.exists(p_tss)) {
      sz <- file.info(p_tss)$size
      say("  ✅ ", m, " -> ", basename(p_tss), " (", format(sz, big.mark=","), " bytes)")
    } else {
      say("  ❌ ", m, " -> ", basename(p_tss), " (missing)")
    }
    
    if (file.exists(p_clr)) {
      sz <- file.info(p_clr)$size
      say("  ✅ ", m, " -> ", basename(p_clr), " (", format(sz, big.mark=","), " bytes)")
    } else {
      say("  ❌ ", m, " -> ", basename(p_clr), " (missing)")
    }
  }
}

say("🎉 All requested methods completed. (Outputs are TSS + CLR)")
audit_outputs(method_list, output_folder)
# Read session config (e.g., PN control label, reference batch) if present
CONTROL_LABEL <- NA_character_
REFERENCE_BATCH <- NA_character_
try({
  if (.can_json) {
    cfg_path <- file.path(output_folder, "session_config.json")
    if (file.exists(cfg_path)) {
      cfg <- jsonlite::fromJSON(cfg_path)
      if (!is.null(cfg$control_label) && nzchar(as.character(cfg$control_label))) {
        CONTROL_LABEL <- as.character(cfg$control_label)
      }
      if (!is.null(cfg$reference_batch) && nzchar(as.character(cfg$reference_batch))) {
        REFERENCE_BATCH <- as.character(cfg$reference_batch)
      }
    }
  }
}, silent = TRUE)
