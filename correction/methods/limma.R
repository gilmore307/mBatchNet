source(file.path("correction", "correction.R"))

prepare_method("limma")

run_method("limma", {
  require(limma)
  X_log <- get_input_for("limma", base_M, base_form)

  limma_design <- matrix(1, nrow = nrow(metadata), ncol = 1)
  design_source <- NULL
  if (!is.null(label_col) && label_col %in% colnames(metadata)) {
    design_source <- metadata[[label_col]]
  } else if (TARGET_BINARY_COL %in% colnames(metadata)) {
    design_source <- metadata[[TARGET_BINARY_COL]]
  }
  if (!is.null(design_source)) {
    if (is.numeric(design_source) || is.logical(design_source)) {
      target_vals <- suppressWarnings(as.numeric(design_source))
      if (all(is.finite(target_vals)) && length(unique(target_vals)) > 1) {
        limma_design <- model.matrix(~ target_vals)
      }
    } else {
      target_vals <- droplevels(factor(design_source))
      if (!anyNA(target_vals) && nlevels(target_vals) > 1) {
        limma_design <- model.matrix(~ target_vals)
      }
    }
  }

  limma_covariates <- covar
  target_columns <- unique(c(label_col, make.names(label_col), TARGET_BINARY_COL, make.names(TARGET_BINARY_COL)))
  target_columns <- target_columns[!is.na(target_columns)]
  drop_target_cols <- intersect(colnames(limma_covariates), target_columns)
  if (length(drop_target_cols)) {
    limma_covariates <- limma_covariates[, !(colnames(limma_covariates) %in% drop_target_cols), drop = FALSE]
  }

  adj_t <- removeBatchEffect(
    t(X_log),
    batch = factor(batch),
    covariates = if (ncol(limma_covariates) > 0) as.matrix(limma_covariates) else NULL,
    design = limma_design
  )
  adj <- t(adj_t)
  write_tss_clr("limma", adj, "log", "normalized_limma.csv")
})

finalize_method()
