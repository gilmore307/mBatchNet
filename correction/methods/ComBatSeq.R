source(file.path("correction", "correction.R"))

prepare_method("ComBatSeq")

run_method("ComBat-Seq", {
  require(sva)
  
  if (!("target_binary" %in% colnames(metadata))) {
    fail_step("ComBat-Seq", "'target_binary' is required.")
  }
  if (!("batch" %in% colnames(metadata))) {
    fail_step("ComBat-Seq", "'batch' is required.")
  }
  
  counts <- get_input_for("ComBatSeq", base_M, base_form)
  
  ## 1. 按库大小筛样本 -----------------------------------------------------
  libsz <- rowSums(counts)
  keep_lib <- !is.na(libsz) & libsz > 0
  if (!all(keep_lib)) {
    say(
      "ComBat-Seq: removing ", sum(!keep_lib),
      " samples with zero/NA library size."
    )
    counts   <- counts[keep_lib, , drop = FALSE]
    metadata <- metadata[keep_lib, , drop = FALSE]
  }
  
  ## 2. 从 metadata 取 covariates（按 covar 的列名） -------------------------
  covariates <- NULL
  if (exists("covar") && is.data.frame(covar) && ncol(covar) > 0) {
    covar_cols <- intersect(colnames(covar), colnames(metadata))
    if (length(covar_cols)) {
      covariates <- metadata[, covar_cols, drop = FALSE]
    }
  }
  
  ## 3. 按 batch / target_binary / covariates 完整性再过滤一遍样本 ----------
  complete_rows <- !is.na(metadata$batch) & !is.na(metadata$target_binary)
  if (!is.null(covariates) && ncol(covariates)) {
    complete_rows <- complete_rows & stats::complete.cases(covariates)
  }
  
  if (!all(complete_rows)) {
    warn_step(
      "ComBat-Seq",
      sprintf(
        "Dropped %d sample(s) with missing batch/target_binary/covariate values before ComBat-Seq.",
        sum(!complete_rows)
      )
    )
    counts     <- counts[complete_rows, , drop = FALSE]
    metadata   <- metadata[complete_rows, , drop = FALSE]
    if (!is.null(covariates) && ncol(covariates)) {
      covariates <- covariates[complete_rows, , drop = FALSE]
    }
  }
  
  ## 4. 丢掉 <2 level 的因子协变量 -----------------------------------------
  covariates_clean <- NULL
  if (!is.null(covariates) && ncol(covariates)) {
    invariant_covariates <- vapply(
      covariates,
      function(col) {
        if (is.numeric(col)) return(FALSE)
        length(unique(stats::na.omit(as.character(col)))) < 2
      },
      logical(1)
    )
    if (any(invariant_covariates)) {
      dropped <- names(invariant_covariates)[invariant_covariates]
      warn_step(
        "ComBat-Seq",
        sprintf(
          "Dropped invariant covariate(s) with <2 levels for ComBat-Seq: %s",
          paste(dropped, collapse = ", ")
        )
      )
    }
    covariates_clean <- covariates[, !invariant_covariates, drop = FALSE]
    if (!ncol(covariates_clean)) covariates_clean <- NULL
  }
  
  ## 5. 再次检查 sample ID 一致 --------------------------------------------
  if (!all(rownames(counts) == rownames(metadata))) {
    fail_step("ComBat-Seq", "Sample IDs mismatch between counts and metadata after filtering.")
  }
  
  ## 6. 构造 design: 先只包含 batch + group（target_binary） --------------
  batch <- droplevels(as.factor(metadata$batch))
  group <- droplevels(as.factor(metadata$target_binary))
  
  batchmod <- model.matrix(~ -1 + batch)      # 和 ComBat_seq 源码一致
  n_batch  <- nlevels(batch)
  
  if (nlevels(group) > 1) {
    mod_base <- model.matrix(~ group)
  } else {
    mod_base <- model.matrix(~ 1, data = as.data.frame(t(counts)))
  }
  
  ## 7. 仿照 ComBat_seq 的 covar_mod 构造方式 + 逐个协变量检测共线 ----------
  build_covar_mod <- function(df) {
    if (is.null(df) || !ncol(df)) return(NULL)
    if (is.data.frame(df)) {
      mm_list <- lapply(seq_len(ncol(df)), function(i) {
        model.matrix(~ df[, i])
      })
      cm <- do.call(cbind, mm_list)
    } else {
      cm <- as.matrix(df)
    }
    cm <- cm[, !apply(cm, 2, function(x) all(x == 1)), drop = FALSE]
    cm
  }
  
  kept_covars <- character(0)
  dropped_covars_conf <- character(0)
  
  if (!is.null(covariates_clean) && ncol(covariates_clean)) {
    for (cn in colnames(covariates_clean)) {
      # 尝试把当前协变量加进去，看 design 是否仍然满秩
      tmp_df   <- covariates_clean[, c(kept_covars, cn), drop = FALSE]
      cov_tmp  <- build_covar_mod(tmp_df)
      mod_tmp  <- cbind(mod_base, cov_tmp)
      design   <- cbind(batchmod, mod_tmp)
      
      # 删除“全 1”列（截距），与 ComBat_seq 一致
      check_int <- apply(design, 2, function(x) all(x == 1))
      design    <- as.matrix(design[, !check_int, drop = FALSE])
      
      # 设计矩阵总 rank & covariate 部分 rank（去掉 batch 列）
      rank_all   <- qr(design)$rank
      ncol_all   <- ncol(design)
      design_cov <- design[, -(1:n_batch), drop = FALSE]
      rank_cov   <- qr(design_cov)$rank
      ncol_cov   <- ncol(design_cov)
      
      if (rank_all < ncol_all || rank_cov < ncol_cov) {
        # 加上这个协变量会导致“covariates are confounded”
        dropped_covars_conf <- c(dropped_covars_conf, cn)
      } else {
        kept_covars <- c(kept_covars, cn)
      }
    }
  }
  
  if (length(dropped_covars_conf)) {
    warn_step(
      "ComBat-Seq",
      sprintf(
        "Dropped confounded covariate(s) for ComBat-Seq (would cause 'covariates are confounded' error): %s",
        paste(dropped_covars_conf, collapse = ", ")
      )
    )
  }
  
  covar_mod <- NULL
  if (length(kept_covars)) {
    covar_mod <- build_covar_mod(covariates_clean[, kept_covars, drop = FALSE])
  } else {
    if (!is.null(covariates_clean)) {
      warn_step(
        "ComBat-Seq",
        "All candidate covariates are confounded with batch/each other; running ComBat-Seq without covariates."
      )
    }
  }
  
  ## 8. 最终调用 ComBat_seq（不会再出现 covariates are confounded 报错） ------
  adj <- ComBat_seq(
    counts    = t(counts),                # gene x sample
    batch     = batch,
    group     = group,
    covar_mod = covar_mod                 # 可能为 NULL
    # full_mod 用默认 TRUE
  )
  
  out_counts <- t(adj)
  write_tss_clr("ComBat-Seq", out_counts, "counts", "normalized_combatseq.csv")
})

finalize_method()
