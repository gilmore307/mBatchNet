# ===================== One-way ANOVA R^2 boxplots (Batch vs Treatment) — CLR only =====================
suppressPackageStartupMessages({
  library(ggplot2)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(forcats)
})

# ----------------- Args / IO -----------------

# Map method codes to short labels for figures
method_short_label <- function(x) {
  map <- c(
    qn = "QN", bmc = "BMC", limma = "Limma", conqur = "ConQuR",
    plsda = "PLSDA-batch", combat = "ComBat", fsqn = "FSQN", mmuphin = "MMUPHin",
    ruv = "RUV-III-NB", metadict = "MetaDICT", svd = "SVD", pn = "PN",
    fabatch = "FAbatch", combatseq = "ComBat-seq", debias = "DEBIAS-M"
  )
  sapply(x, function(v){ lv <- tolower(v); if (lv %in% names(map)) map[[lv]] else v })
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

apply_fig_overrides <- function(width_in, height_in, default_dpi = 300) {
  dpi <- if (is.na(opt_fig_dpi) || opt_fig_dpi <= 0) default_dpi else opt_fig_dpi
  w <- width_in
  h <- height_in
  if (!is.na(opt_fig_width_px) && opt_fig_width_px > 0 && dpi > 0) {
    w <- opt_fig_width_px / dpi
  }
  if (!is.na(opt_fig_height_px) && opt_fig_height_px > 0 && dpi > 0) {
    h <- opt_fig_height_px / dpi
  }
  list(width = w, height = h, dpi = dpi)
}

metadata <- read_csv(file.path(output_folder, "metadata.csv"), show_col_types = FALSE)
if (!("sample_id" %in% names(metadata))) {
  metadata$sample_id <- sprintf("S%03d", seq_len(nrow(metadata)))
}
metadata <- metadata |> mutate(sample_id = as.character(sample_id))

if (!("batch_id" %in% names(metadata)) && ("batch_id" %in% names(metadata))) {
  metadata$batch_id <- metadata$batch_id
}

# ---- Find normalized files ----
clr_paths <- list.files(output_folder, pattern = "^normalized_.*_clr\\.csv$", full.names = TRUE)

# Fallback: if no suffix-specific outputs, use any normalized_*.csv as CLR
if (!length(clr_paths)) {
  clr_paths <- list.files(output_folder, pattern = "^normalized_.*\\.csv$", full.names = TRUE)
}

name_from <- function(paths, suffix) gsub(paste0("^normalized_|_", suffix, "\\.csv$"), "", basename(paths))
file_list_clr <- setNames(clr_paths, method_short_label(name_from(clr_paths, "clr")))

# Include raw_clr.csv as "Before correction" if present
raw_clr_fp <- file.path(output_folder, "raw_clr.csv")
if (file.exists(raw_clr_fp)) file_list_clr <- c("Before correction" = raw_clr_fp, file_list_clr)

if (!length(file_list_clr)) {
  stop("No normalized CLR files found (expected raw_clr.csv and/or normalized_*_clr.csv) in ", output_folder)
}

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

# ----------------- Core: per-feature one-way ANOVA R^2 (Batch vs Treatment) -----------------
compute_anova_r2_BT <- function(df, meta, batch_col = "batch_id", treat_col = "phenotype") {
  # align to metadata
  if (!"sample_id" %in% names(df)) {
    if (nrow(df) == nrow(meta)) df$sample_id <- meta$sample_id
    else stop("Input lacks 'sample_id' and row count != metadata; can't align samples.")
  }
  df  <- df %>% mutate(sample_id = as.character(sample_id))
  dfx <- inner_join(df, meta, by = "sample_id")
  
  if (!(batch_col %in% names(dfx))) stop(sprintf("Batch column '%s' not in metadata.", batch_col))
  if (!(treat_col %in% names(dfx))) stop(sprintf("Treatment column '%s' not in metadata.", treat_col))
  
  feat_cols <- setdiff(names(df), "sample_id")
  X <- dfx %>% select(all_of(feat_cols)) %>% select(where(is.numeric)) %>% as.matrix()
  
  # drop features that are non-finite or constant
  keep <- apply(X, 2, function(z) all(is.finite(z)) && sd(z, na.rm = TRUE) > 0)
  X <- X[, keep, drop = FALSE]
  if (!ncol(X)) return(tibble(Feature = character(), Effect = character(), R2 = numeric()))
  
  Y <- if (any(X < 0, na.rm = TRUE)) sweep(X, 1, rowMeans(X, na.rm = TRUE), "-") else clr_transform(X)
  colnames(Y) <- colnames(X)
  
  keep_row <- !is.na(dfx[[batch_col]]) & !is.na(dfx[[treat_col]])
  dfx <- dfx[keep_row, , drop = FALSE]
  Y   <- Y[keep_row, , drop = FALSE]
  
  dfx[[batch_col]] <- factor(dfx[[batch_col]])
  dfx[[treat_col]] <- factor(dfx[[treat_col]])
  
  res <- lapply(seq_len(ncol(Y)), function(j) {
    y <- Y[, j]
    r2_b <- anova_r2(y, dfx[[batch_col]])
    r2_t <- anova_r2(y, dfx[[treat_col]])
    tibble(Feature = colnames(Y)[j],
           Effect  = c("Batch","Treatment"),
           R2      = as.numeric(c(r2_b, r2_t)))
  }) %>% bind_rows()
  
  res %>% filter(is.finite(R2), R2 >= 0, R2 <= 1)
}

# ----------------- Build per-feature R^2 across all methods -----------------
batch_col <- "batch_id"
treat_col <- "phenotype"
if (!("phenotype" %in% names(metadata))) stop("metadata.csv lacks 'phenotype'.")
if (dplyr::n_distinct(metadata$phenotype) < 2) stop("'phenotype' needs at least 2 levels.")
if (dplyr::n_distinct(metadata$batch_id)   < 2) stop("'batch_id' needs at least 2 levels.")

# CLR set
r2_long_clr <- lapply(names(file_list_clr), function(nm) {
  message("Per-feature ANOVA R^2 (CLR): ", nm)
  df <- read_csv(file_list_clr[[nm]], show_col_types = FALSE)
  out <- compute_anova_r2_BT(df, metadata, batch_col, treat_col)
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
        tolower(Effect) %in% c("batch","batch_id","batch_id") ~ "Batch",
        tolower(Effect) %in% c("treatment","phenotype","group","trt") ~ "Treatment",
        TRUE ~ Effect
      ),
      Effect = factor(Effect, levels = c("Batch","Treatment")),
      Method = factor(Method, levels = method_levels)
    ) %>%
    filter(!is.na(R2), is.finite(R2), R2 >= 0, R2 <= 1)
}
r2_long_clr <- tidy_long(r2_long_clr, method_levels_clr)

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
    scale_fill_manual(values = c(Batch = "#FF7F0E", Treatment = "#BDBDBD"),
                      name = "Effect", drop = FALSE) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(y = expression("Feature-wise ANOVA "*R^2), x = NULL, title = title) +
    theme_bw() +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 1),
      panel.grid   = element_blank(),
      axis.text    = element_text(size = 10),
      axis.title   = element_text(size = 12),
      strip.background = element_rect(fill = "grey90", colour = NA),
      strip.text   = element_text(size = 10),
      plot.title   = element_text(hjust = 0.5, size = rel(1.2))
    ) +
    geom_text(
      data = med_df,
      aes(x = Effect, y = pmin(med + 0.03, 0.98), label = sprintf("%.3f", med)),
      inherit.aes = FALSE, size = 3
    )
  
  n_methods <- dplyr::n_distinct(r2_long_df$Method)
  if (n_methods > 1) {
    p <- p + facet_grid(. ~ Method, scales = "free_x", space = "free_x")
  }
  p
}

p_clr <- make_boxplot(
  r2_long_clr, method_levels_clr,
  expression("Feature-wise ANOVA " * R^2 )
)
if (!is.null(p_clr)) {
  fig_dims_clr <- apply_fig_overrides(10, 5.2, 300)
  ggsave(file.path(output_folder, "R2_aitchison.png"), p_clr,
         width = fig_dims_clr$width, height = fig_dims_clr$height, dpi = fig_dims_clr$dpi)
  ggsave(file.path(output_folder, "R2_aitchison.tif"), p_clr,
         width = fig_dims_clr$width, height = fig_dims_clr$height, dpi = fig_dims_clr$dpi, compression = "lzw")
}

# ----------------- Unified assessment table -----------------
median_r2_by_method <- r2_long_clr %>%
  group_by(Method, Effect) %>%
  summarise(median_R2 = median(R2, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Effect, values_from = median_R2)

all_methods <- sort(unique(levels(r2_long_clr$Method)))
only_baseline <- length(all_methods) == 1L && identical(all_methods, "Before correction")
output_name <- if (only_baseline) "R2_raw_assessment_pre.csv" else "R2_raw_assessment_post.csv"

if (only_baseline) {
  assess_df <- median_r2_by_method %>%
    mutate(
      Median_R2_Batch = Batch,
      Median_R2_Treatment = Treatment
    ) %>%
    select(Method, Median_R2_Batch, Median_R2_Treatment)

  print(assess_df, n = nrow(assess_df))
  readr::write_csv(assess_df, file.path(output_folder, output_name))

  # No correction recommendation messages

} else {
  assessment_tbl <- median_r2_by_method %>%
    mutate(
      Median_R2_Batch = Batch,
      Median_R2_Treatment = Treatment
    ) %>%
    select(Method, Median_R2_Batch, Median_R2_Treatment)

  print(assessment_tbl, n = nrow(assessment_tbl))
  readr::write_csv(assessment_tbl, file.path(output_folder, output_name))
}
