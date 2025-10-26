source(file.path("correction", "methods", "common.R"))

prepare_method("RUV")

run_method("RUV-III-NB", {
  suppressPackageStartupMessages({
    library(DescTools)
    library(ruvIIInb)
    library(Matrix)
  })

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

  Sys.setenv(OMP_NUM_THREADS = "1", MKL_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1")

  Y <- t(get_input_for("RUV", base_M, base_form))
  if (is.null(colnames(Y))) colnames(Y) <- rownames(metadata)
  storage.mode(Y) <- "double"
  Y[!is.finite(Y) | Y < 0] <- 0
  Y <- round(Y); storage.mode(Y) <- "integer"
  say("RUV: Y prepared")

  samp_ids <- colnames(Y)

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

  ctl_names <- rownames(Y)
  say("RUV: design ready (ruvIII.nb)")

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

  out_counts <- t(as.matrix(fit$counts))
  write_tss_clr("RUV-III-NB", out_counts, "counts", "normalized_ruv.csv")
})

finalize_method()
