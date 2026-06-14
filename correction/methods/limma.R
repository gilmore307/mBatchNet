source(file.path("correction", "correction.R"))

prepare_method("limma")

run_method("limma", {
  require(limma)
  X_log <- get_input_for("limma", base_M, base_form)

  limma_design <- matrix(1, nrow = nrow(metadata), ncol = 1)
  if (TARGET_BINARY_COL %in% colnames(metadata)) {
    target_vals <- suppressWarnings(as.numeric(metadata[[TARGET_BINARY_COL]]))
    if (all(is.finite(target_vals)) && length(unique(target_vals)) > 1) {
      limma_design <- model.matrix(~ target_vals)
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
