# ===================== One-way ANOVA R^2 boxplots (Batch vs Target) — CLR only =====================
suppressPackageStartupMessages({
  library(ggplot2)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(forcats)
  library(jsonlite)
})

source("plots/helper.R")

# ----------------- Args / IO -----------------

format_method_label <- function(label) {
  if (identical(label, "Before correction")) {
    return("Before\ncorrection")
  }
  if (identical(label, "PLSDA-batch")) {
    return(sub("-", "\n-", label, fixed = TRUE))
  }
  label
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  args <- "output/example"  # default folder for quick runs
}
output_folder <- args[1]
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

opt_fig_width_px  <- NA_real_
opt_fig_height_px <- NA_real_
opt_fig_dpi       <- NA_real_
opt_fig_ncol      <- NA_integer_

for (a in args[-1]) {
  if (grepl("^--fig-width-px=", a)) {
    opt_fig_width_px <- suppressWarnings(as.numeric(sub("^--fig-width-px=", "", a)))
    if (!is.finite(opt_fig_width_px) || opt_fig_width_px <= 0) opt_fig_width_px <- NA_real_
  }
  if (grepl("^--fig-height-px=", a)) {
    opt_fig_height_px <- suppressWarnings(as.numeric(sub("^--fig-height-px=", "", a)))
    if (!is.finite(opt_fig_height_px) || opt_fig_height_px <= 0) opt_fig_height_px <- NA_real_
  }
  if (grepl("^--fig-dpi=", a)) {
    opt_fig_dpi <- suppressWarnings(as.numeric(sub("^--fig-dpi=", "", a)))
    if (!is.finite(opt_fig_dpi) || opt_fig_dpi <= 0) opt_fig_dpi <- NA_real_
  }
  if (grepl("^--fig-ncol=", a)) {
    opt_fig_ncol <- suppressWarnings(as.integer(sub("^--fig-ncol=", "", a)))
    if (!is.finite(opt_fig_ncol) || opt_fig_ncol <= 0) opt_fig_ncol <- NA_integer_
  }
}

apply_fig_overrides <- function(width_in, height_in, default_dpi = 300,
                                panel_cols = 1, panel_rows = 1) {
  dpi <- if (is.na(opt_fig_dpi) || opt_fig_dpi <= 0) default_dpi else opt_fig_dpi
  panel_cols <- max(1, as.integer(panel_cols))
  panel_rows <- max(1, as.integer(panel_rows))
  w <- width_in * panel_cols
  h <- height_in * panel_rows
  if (!is.na(opt_fig_width_px) && opt_fig_width_px > 0 && dpi > 0) {
    per_panel <- opt_fig_width_px / dpi
    w <- per_panel * panel_cols
  }
  if (!is.na(opt_fig_height_px) && opt_fig_height_px > 0 && dpi > 0) {
    per_panel <- opt_fig_height_px / dpi
    h <- per_panel * panel_rows
  }
  list(width = w, height = h, dpi = dpi)
}

# ----------------- Read metadata -----------------
meta_path <- if (file.exists(file.path(output_folder, "metadata_origin.csv"))) {
  file.path(output_folder, "metadata_origin.csv")
} else {
  file.path(output_folder, "metadata.csv")
}
metadata <- read_csv(meta_path, show_col_types = FALSE)
if (!("sample_id" %in% names(metadata))) {
  metadata$sample_id <- sprintf("S%03d", seq_len(nrow(metadata)))
}
metadata <- metadata |> mutate(sample_id = as.character(sample_id))

label_col <- "phenotype"
try({
  cfg_path <- file.path(output_folder, "session_config.json")
  if (file.exists(cfg_path)) {
    cfg <- jsonlite::fromJSON(cfg_path)
    if (!is.null(cfg$label_column)) label_col <- cfg$label_column
  }
}, silent = TRUE)
if (!(label_col %in% names(metadata))) {
  fallback <- c("group","condition","status","class","label")
  cand <- fallback[fallback %in% names(metadata)]
  if (length(cand)) {
    label_col <- cand[1]
  } else if ("phenotype" %in% names(metadata)) {
    label_col <- "phenotype"
  } else {
    stop("metadata file lacks a label column for ANOVA plots.")
  }
}

# ----------------- Find normalized files -----------------
clr_paths <- list.files(output_folder, pattern = "^normalized_.*_clr\\.csv$", full.names = TRUE)
# Fallback: if no suffix-specific outputs, use any normalized_*.csv as CLR
if (!length(clr_paths)) {
  clr_paths <- list.files(output_folder, pattern = "^normalized_.*\\.csv$", full.names = TRUE)
}

file_list_clr <- setNames(clr_paths, method_short_label(name_from(clr_paths, "clr")))

# Include raw_clr.csv as "Before correction" if present
raw_clr_fp <- file.path(output_folder, "raw_clr.csv")
if (file.exists(raw_clr_fp)) file_list_clr <- c("Before correction" = raw_clr_fp, file_list_clr)

names(file_list_clr) <- vapply(names(file_list_clr), format_method_label, character(1))

if (!length(file_list_clr)) {
  stop("No normalized CLR files found (expected raw_clr.csv and/or normalized_*_clr.csv or normalized_*.csv) in ", output_folder)
}

message("# files detected: ", length(file_list_clr), " -> ", paste(names(file_list_clr), collapse = ", "))

# ----------------- Helpers -----------------
safe_closure <- function(X) {
  X[!is.finite(X)] <- 0
  X[X < 0] <- 0
  rs <- rowSums(X, na.rm = TRUE)
  bad <- which(rs == 0 | !is.finite(rs))
  if (length(bad)) { X[bad, ] <- 1 / ncol(X); rs <- rowSums(X, na.rm = TRUE) }
  sweep(X, 1, rs, "/")
}

clr_transform <- function(X) {
  Xc <- safe_closure(X)
  for (i in seq_len(nrow(Xc))) {
    xi <- Xc[i, ]; pos <- xi > 0 & is.finite(xi)
    if (!any(pos)) { xi[] <- 1/length(xi); pos <- xi > 0 }
    if (any(!pos)) {
      m <- min(xi[pos], na.rm = TRUE)
      xi[!pos] <- min(m*0.5, 1e-8)
      xi <- xi / sum(xi)
    }
    Xc[i, ] <- xi
  }
  L <- log(Xc)
  sweep(L, 1, rowMeans(L), "-")
}

# Unadjusted ANOVA R^2 = 1 - SSE/SST (clamped to [0,1])
anova_r2 <- function(y, g) {
  ok <- is.finite(y) & !is.na(g)
  y <- y[ok]; g <- droplevels(factor(g[ok]))
  if (length(y) < 3 || nlevels(g) < 2) return(NA_real_)
  fit <- tryCatch(lm(y ~ g), error = function(e) NULL)
  if (is.null(fit)) return(NA_real_)
  r2 <- summary(fit)$r.squared
  if (!is.finite(r2)) return(NA_real_)
  max(0, min(1, as.numeric(r2)))
}

# ----------------- Core: per-feature one-way ANOVA R^2 (Batch vs Target) -----------------
compute_anova_r2_BT <- function(df, meta, batch_col = "batch", treat_col = label_col) {
  # align to metadata
  if (!"sample_id" %in% names(df)) {
    if (nrow(df) == nrow(meta)) df$sample_id <- meta$sample_id
    else stop("Input lacks 'sample_id' and row count != metadata; can't align samples.")
  }
  df  <- df %>% mutate(sample_id = as.character(sample_id))
  dfx <- inner_join(df, meta, by = "sample_id")
  
  message("df rows/cols: ", nrow(df), "/", ncol(df))
  message("after join rows: ", nrow(dfx))
  
  if (!(batch_col %in% names(dfx))) stop(sprintf("Batch column '%s' not in metadata/joined data.", batch_col))
  if (!(treat_col %in% names(dfx))) stop(sprintf("Target column '%s' not in metadata/joined data.", treat_col))
  
  feat_cols <- setdiff(names(df), "sample_id")
  X0 <- dfx %>% select(all_of(feat_cols))
  message("numeric features before keep: ", ncol(select(X0, where(is.numeric))))
  X <- X0 %>% select(where(is.numeric)) %>% as.matrix()
  
  # --- 修复：先处理非有限值，再只按方差>0过滤 ---
  X[!is.finite(X)] <- 0
  keep <- apply(X, 2, function(z) sd(z, na.rm = TRUE) > 0)
  
  if (!any(keep)) {
    message("kept features: 0  (all constant after cleaning)")
    return(tibble(Feature = character(), Effect = character(), R2 = numeric()))
  }
  X <- X[, keep, drop = FALSE]
  message("kept features: ", ncol(X))
  
  # transform
  Y <- if (any(X < 0, na.rm = TRUE)) sweep(X, 1, rowMeans(X, na.rm = TRUE), "-") else clr_transform(X)
  colnames(Y) <- colnames(X)
  
  # rows to use
  keep_row <- !is.na(dfx[[batch_col]]) & !is.na(dfx[[treat_col]])
  dfx <- dfx[keep_row, , drop = FALSE]
  Y   <- Y[keep_row, , drop = FALSE]
  message("rows used for ANOVA: ", nrow(Y))
  
  dfx[[batch_col]] <- factor(dfx[[batch_col]])
  dfx[[treat_col]] <- factor(dfx[[treat_col]])
  
  res <- lapply(seq_len(ncol(Y)), function(j) {
    y <- Y[, j]
    r2_b <- anova_r2(y, dfx[[batch_col]])
    r2_t <- anova_r2(y, dfx[[treat_col]])
    tibble(Feature = colnames(Y)[j],
           Effect  = c("Batch","Target"),
           R2      = as.numeric(c(r2_b, r2_t)))
  }) %>% bind_rows()
  
  res %>% filter(is.finite(R2), R2 >= 0, R2 <= 1)
}

# ----------------- Build per-feature R^2 across all methods -----------------
if (!(label_col %in% names(metadata))) stop("metadata file lacks the label column.")
if (dplyr::n_distinct(metadata[[label_col]]) < 2) stop("Label column needs at least 2 levels.")
if (!("batch" %in% names(metadata))) stop("metadata file lacks 'batch'.")
if (dplyr::n_distinct(metadata$batch)   < 2) stop("'batch' needs at least 2 levels.")

# CLR set
r2_long_clr <- lapply(names(file_list_clr), function(nm) {
  message("Per-feature ANOVA R^2 (CLR): ", nm)
  df <- read_csv(file_list_clr[[nm]], show_col_types = FALSE)
out <- compute_anova_r2_BT(df, metadata, "batch", label_col)
  out$Method <- nm
  out
}) %>% bind_rows()

# tidy labels/order for plotting
method_levels_clr <- names(file_list_clr)

tidy_long <- function(df, method_levels) {
  if (!nrow(df)) return(df)
  df %>%
    mutate(
      Effect = case_when(
        tolower(Effect) %in% c("batch","batch") ~ "Batch",
        tolower(Effect) %in% c("treatment","phenotype","group","trt") ~ "Target",
        TRUE ~ Effect
      ),
      Effect = factor(Effect, levels = c("Batch","Target")),
      Method = factor(Method, levels = method_levels)
    ) %>%
    filter(!is.na(R2), is.finite(R2), R2 >= 0, R2 <= 1)
}
r2_long_clr <- tidy_long(r2_long_clr, method_levels_clr)

message("r2_long_clr nrow: ", nrow(r2_long_clr),
        "; methods present: ", paste(unique(r2_long_clr$Method), collapse = ", "))

# ----------------- Figure (auto facet only if >1 method) -----------------
make_boxplot <- function(r2_long_df, method_levels, title) {
  if (!nrow(r2_long_df)) return(NULL)
  med_df <- r2_long_df %>%
    dplyr::group_by(Method, Effect) %>%
    dplyr::summarize(med = median(R2), .groups = "drop")
  
  p <- ggplot(r2_long_df, aes(x = Effect, y = R2, fill = Effect)) +
    geom_boxplot(width = 0.7, outlier.size = 0.7) +
    stat_boxplot(aes(ymin = after_stat(ymax), ymax = after_stat(ymax)),
                 geom = "errorbar", width = 0.35) +
    stat_boxplot(aes(ymin = after_stat(ymin), ymax = after_stat(ymin)),
                 geom = "errorbar", width = 0.35) +
    scale_fill_manual(values = c(Batch = "#FF7F0E", Target = "#BDBDBD"),
                      name = "Effect", drop = FALSE) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(y = expression("Feature-wise ANOVA "*R^2), x = NULL, title = title) +
    theme_bw() +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 1),
      panel.grid   = element_blank(),
      axis.text    = element_text(size = 10),
      axis.title   = element_text(size = 12),
      strip.background = element_blank(),
      strip.text   = element_text(size = 10),
      plot.title   = element_text(hjust = 0.5, size = rel(1.2), face = "bold")
    ) +
    geom_text(
      data = med_df,
      aes(x = Effect, y = pmin(med + 0.03, 0.98), label = sprintf("%.3f", med)),
      inherit.aes = FALSE, size = 3
    )
  
  n_methods <- dplyr::n_distinct(r2_long_df$Method)
  if (n_methods > 1) {
    ncol_grid <- n_methods
    if (!is.na(opt_fig_ncol) && opt_fig_ncol >= 1) {
      ncol_grid <- max(1, opt_fig_ncol)
    }
    p <- p + facet_wrap(~ Method, scales = "free_x", ncol = ncol_grid)
  }
  p
}

# ----------------- Unified assessment table -----------------
median_r2_by_method <- r2_long_clr %>%
  group_by(Method, Effect) %>%
  summarise(median_R2 = median(R2, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Effect, values_from = median_R2) %>%
  mutate(Method = factor(Method, levels = method_levels_clr))

# ASCII 临时列名（避免在代码里使用 \u 上标导致反引号解析问题）
format_assessment_tbl <- function(df) {
  df <- df %>% ungroup()
  if (!("Method" %in% names(df)))     df$Method <- character(nrow(df))
  if (!("Batch" %in% names(df)))      df$Batch <- numeric(nrow(df))
  if (!("Target" %in% names(df)))  df$Target <- numeric(nrow(df))
  df %>%
    mutate(
      `Median R2 (Batch)` = Batch,
      `Median R2 (Target)` = Target
    ) %>%
    select(Method, `Median R2 (Batch)`, `Median R2 (Target)`) %>%
    arrange(Method)
}

# 将列名中的 R2 改成真正的 R²（仅在输出/打印前替换）
apply_R2_superscript_names <- function(df) {
  R2_SUP <- enc2utf8("\u00B2")  # 上标 ²
  nm <- names(df)
  nm[nm == "Median R2 (Batch)"]     <- paste0("Median R", R2_SUP, " (Batch)")
  nm[nm == "Median R2 (Target)"] <- paste0("Median R", R2_SUP, " (Target)")
  names(df) <- nm
  df
}

baseline_label <- "Before correction"

pre_assessment <- median_r2_by_method %>%
  filter(trimws(as.character(Method)) == baseline_label) %>%
  format_assessment_tbl()

post_assessment <- median_r2_by_method %>%
  format_assessment_tbl()

# —— 改名为带 R² 再打印/写盘 ——
pre_out  <- apply_R2_superscript_names(pre_assessment)
post_out <- apply_R2_superscript_names(post_assessment)

message("pre_assessment rows: ", nrow(pre_out),
        "; post_assessment rows: ", nrow(post_out))

print(pre_out,  n = nrow(pre_out))
print(post_out, n = nrow(post_out))

# 始终写出 CSV（即使 0 行）
readr::write_csv(pre_out,  file.path(output_folder, "anova_raw_assessment_pre.csv"))
readr::write_csv(post_out, file.path(output_folder, "anova_raw_assessment_post.csv"))

# 另外写一份 Excel 友好的 CSV（带 BOM，Windows Excel 打开更稳的 R² 显示）
readr::write_excel_csv(pre_out,  file.path(output_folder, "anova_raw_assessment_pre_excel.csv"))
readr::write_excel_csv(post_out, file.path(output_folder, "anova_raw_assessment_post_excel.csv"))

message("Saved CSV: anova_raw_assessment_pre.csv / anova_raw_assessment_post.csv (+ *_excel.csv)")

# ----------------- Plot after CSVs are written -----------------
p_clr <- make_boxplot(
  r2_long_clr, method_levels_clr,
  expression("Feature-wise ANOVA " * R^2 )
)
if (!is.null(p_clr)) {
  n_methods <- dplyr::n_distinct(r2_long_clr$Method)
  ncol_grid <- n_methods
  if (!is.na(opt_fig_ncol) && opt_fig_ncol >= 1) {
    ncol_grid <- max(1, opt_fig_ncol)
  }
  panel_cols <- min(ncol_grid, n_methods)
  panel_rows <- ceiling(n_methods / ncol_grid)
  fig_dims_clr <- apply_fig_overrides(960 / 300, 1200 / 300, 300, panel_cols, panel_rows)
  tif_path <- file.path(output_folder, "anova_aitchison.tif")
  ggsave(tif_path, p_clr,
         width = fig_dims_clr$width, height = fig_dims_clr$height, dpi = fig_dims_clr$dpi, compression = "lzw")
  message("Saved figure: anova_aitchison.tif")
} else {
  message("No data to plot; skip figure export.")
}
