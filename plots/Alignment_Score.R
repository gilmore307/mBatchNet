# ================= Alignment Score (AS) - CLR only =================
suppressPackageStartupMessages({
  library(FNN)
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(magick)
})

source("plots/helper.R")

# --------- Args / config ---------
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  args <- "output/example"  # default folder for quick runs
}
output_folder <- args[1]
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

K_NEIGHBORS   <- 10     # k in kNN
VAR_PROP_MIN  <- 0.95   # keep PCs explaining at least 95% variance
MAX_PCS       <- 10     # safety cap
set.seed(42)

opt_fig_width_px  <- NA_real_
opt_fig_height_px <- NA_real_
opt_fig_dpi       <- NA_real_
opt_fig_ncol      <- NA_integer_

# ---- Optional CLI flags ----
# Support: --k=INT  --var_prop_min=FLOAT  --max_pcs=INT
if (length(args) > 1) {
  for (a in args[grepl("^--", args)]) {
    if (grepl("^--k=", a)) {
      v <- suppressWarnings(as.integer(sub("^--k=", "", a)))
      if (is.finite(v) && v >= 1) K_NEIGHBORS <- v
    }
    if (grepl("^--var_prop_min=", a)) {
      v <- suppressWarnings(as.numeric(sub("^--var_prop_min=", "", a)))
      if (is.finite(v) && v > 0 && v <= 1) VAR_PROP_MIN <- v
    }
    if (grepl("^--max_pcs=", a)) {
      v <- suppressWarnings(as.integer(sub("^--max_pcs=", "", a)))
      if (is.finite(v) && v >= 2) MAX_PCS <- v
    }
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

# --------- Load metadata ---------
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

# --------- Collect CLR files ---------
clr_paths <- list.files(output_folder, pattern = "^normalized_.*_clr\\.csv$", full.names = TRUE)

# include raw_clr.csv (as baseline) if present
raw_clr_fp <- file.path(output_folder, "raw_clr.csv")
if (file.exists(raw_clr_fp)) clr_paths <- c(raw_clr_fp, clr_paths)

if (!length(clr_paths)) stop("No CLR matrices found (expected 'raw_clr.csv' or 'normalized_*_clr.csv').")

method_names <- ifelse(basename(clr_paths) == "raw_clr.csv",
                       "Before correction",
                       gsub("^normalized_|_clr\\.csv$", "", basename(clr_paths)))
file_list <- setNames(clr_paths, method_short_label(method_names))

# lock plotting order to the discovered order
method_levels <- names(file_list)

# --------- Helpers ---------
safe_numeric_matrix <- function(df) {
  num <- dplyr::select(df, where(is.numeric))
  if (!ncol(num)) return(matrix(numeric(0), nrow = nrow(df)))
  keep <- vapply(num, function(z) all(is.finite(z)) && sd(z) > 0, logical(1))
  as.matrix(num[, keep, drop = FALSE])
}

compute_alignment_score <- function(X, batch, k = 10, var_prop = 0.95, max_pcs = 10) {
  # X: samples x features (numeric), batch: factor/character vector
  if (!is.matrix(X) || nrow(X) < 3 || ncol(X) < 2) return(NA_real_)
  pca <- prcomp(X, scale. = TRUE)
  var_cum <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
  npc <- min(max(which(var_cum < var_prop)) + 1, ncol(pca$x))
  npc <- max(2, min(npc, max_pcs))
  coords <- pca$x[, seq_len(npc), drop = FALSE]
  k_eff <- max(1, min(k, nrow(coords) - 1))
  nn <- FNN::get.knn(coords, k = k_eff)$nn.index
  batch <- as.character(batch)
  si <- vapply(seq_len(nrow(coords)), function(i) {
    nb <- batch[nn[i, ]]
    1 - mean(nb == batch[i], na.rm = TRUE)  # fraction of neighbors from other batches
  }, numeric(1))
  mean(si, na.rm = TRUE)
}

# Simple summary (for CSV only; plotting keeps original order)
summarise_alignment_methods <- function(as_table) {
  baseline_row <- as_table %>% filter(Method == "Before correction")
  baseline_as <- if (nrow(baseline_row)) baseline_row$`Alignment Score`[1] else NA_real_
  as_table
}

# --------- Compute AS per method ---------
as_tbl <- tibble(Method = character(), `Alignment Score` = numeric())

for (nm in names(file_list)) {
  fp <- file_list[[nm]]
  df <- read_csv(fp, show_col_types = FALSE)
  if (!("sample_id" %in% names(df))) {
    if (nrow(df) == nrow(metadata)) {
      df$sample_id <- metadata$sample_id
    } else {
      warning(sprintf("Skipping %s: no sample_id and row count mismatch.", nm)); next
    }
  }
  df <- df |> mutate(sample_id = as.character(sample_id))
  merged <- inner_join(df, metadata, by = "sample_id")
  if (!nrow(merged)) { warning(sprintf("Skipping %s: no overlap with metadata.", nm)); next }
  # only use original data columns (exclude metadata)
  X <- safe_numeric_matrix(merged[, setdiff(names(df), "sample_id"), drop = FALSE])
  b <- merged$batch
  ascore <- tryCatch(
    compute_alignment_score(X, b, k = K_NEIGHBORS, var_prop = VAR_PROP_MIN, max_pcs = MAX_PCS),
    error = function(e) NA_real_
  )
  as_tbl <- bind_rows(as_tbl, tibble(Method = nm, `Alignment Score` = ascore))
}

# --------- Summaries (if applicable) + Save + Plot in original order ---------
only_baseline <- length(method_levels) == 1L && identical(method_levels, "Before correction")
output_name <- if (only_baseline) "alignment_score_raw_assessment_pre.csv" else "alignment_score_raw_assessment_post.csv"

if (only_baseline) {
  # ---- Baseline-only assessment (no ranking) ----
  base_row <- as_tbl %>% filter(Method == "Before correction")
  if (nrow(base_row) == 0) {
    stop("No Alignment Score computed for 'Before correction'.")
  }
  base_row <- base_row
  
  # save + print assessment
  base_summary <- summarise_alignment_methods(base_row)
  readr::write_csv(base_summary, file.path(output_folder, output_name))
  print(base_summary)
  
  # single-bar plot in original order
  plot_df <- base_row %>% mutate(Method = factor(Method, levels = method_levels))
  y_max <- max(plot_df$`Alignment Score`, na.rm = TRUE)
  y_upper <- if (is.finite(y_max)) y_max * 1.2 else NA_real_

  p_as <- ggplot(plot_df, aes(x = Method, y = `Alignment Score`, fill = Method)) +
    geom_col(width = 0.72, color = "white", linewidth = 0.4, show.legend = FALSE) +
    geom_text(aes(label = sprintf("%.3f", `Alignment Score`)), vjust = -0.4, size = 3.2) +
    scale_y_continuous(limits = c(0, y_upper), expand = expansion(mult = c(0, 0.02))) +
    labs(title = "Alignment Score (baseline)",
         x = "Method", y = "Score") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(hjust = 0.5, face = "plain")
    )

  fig_dims <- apply_fig_overrides(2800 / 300, 1800 / 300, 300)
  tif_path <- file.path(output_folder, "alignment_score.tif")
  ggsave(tif_path, p_as,
         width = fig_dims$width, height = fig_dims$height, dpi = fig_dims$dpi, compression = "lzw")
  create_png_thumbnail(tif_path)
  
  # No correction recommendation messages
  
} else {
  # ---- Multiple methods: keep bar order as in file_list; summarise without ranking ----
  as_summary <- summarise_alignment_methods(as_tbl)
  readr::write_csv(as_summary, file.path(output_folder, output_name))
  print(as_summary)
  
  # Plot bars in original method order (DO NOT reorder by AS)
  plot_df <- as_tbl %>%
    mutate(Method = factor(Method, levels = method_levels))
  y_max <- max(plot_df$`Alignment Score`, na.rm = TRUE)
  y_upper <- if (is.finite(y_max)) y_max * 1.2 else NA_real_

  p_as <- ggplot(plot_df, aes(x = Method, y = `Alignment Score`, fill = Method)) +
    geom_col(width = 0.72, color = "white", linewidth = 0.4, show.legend = FALSE) +
    geom_text(aes(label = sprintf("%.3f", `Alignment Score`)), vjust = -0.4, size = 3.2) +
    scale_y_continuous(limits = c(0, y_upper), expand = expansion(mult = c(0, 0.02))) +
    labs(title = "Alignment Score",
         x = "Method", y = "Score") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(hjust = 0.5, face = "plain")
    )

  fig_dims <- apply_fig_overrides(2800 / 300, 1800 / 300, 300)
  tif_path <- file.path(output_folder, "alignment_score.tif")
  ggsave(tif_path, p_as,
         width = fig_dims$width, height = fig_dims$height, dpi = fig_dims$dpi, compression = "lzw")
  create_png_thumbnail(tif_path)
}
