# ===================== EBM (CLR-only, UMAP+knn) =====================
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(uwot)     # UMAP
  library(FNN)      # kNN
})

source("plots/helper.R")

# --------- Args / config ---------

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  args <- "output/example"  # default folder for quick runs
}
output_folder <- args[1]
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

# UMAP (CLR/Aitchison geometry)
UMAP_NEIGHB   <- 15
UMAP_MIN_DIST <- 0.3
UMAP_METRIC   <- "euclidean"   # CLR -> Euclidean

# EBM (kNN) params - "his method"
KNN_K         <- 50
KNN_POOLS     <- 50
KNN_PER_LABEL <- 100

# EBM labels column (batch mixing uses batches)
LABEL_COL <- "batch"

# Baseline-only decision threshold removed; report metrics without recommendations

set.seed(42)  # uwot uses global RNG

opt_fig_width_px  <- NA_real_
opt_fig_height_px <- NA_real_
opt_fig_dpi       <- NA_real_
opt_fig_ncol      <- NA_integer_

# ---- Optional CLI flags ----
# Support: --umap_neighbors=INT  --umap_min_dist=FLOAT  --umap_metric=STR
#          --knn_k=INT  --knn_pools=INT  --knn_per_label=INT
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1) {
  for (a in args[grepl("^--", args)]) {
    if (grepl("^--umap_neighbors=", a)) {
      v <- suppressWarnings(as.integer(sub("^--umap_neighbors=", "", a)))
      if (is.finite(v) && v >= 2) UMAP_NEIGHB <- v
    }
    if (grepl("^--umap_min_dist=", a)) {
      v <- suppressWarnings(as.numeric(sub("^--umap_min_dist=", "", a)))
      if (is.finite(v) && v >= 0 && v <= 1) UMAP_MIN_DIST <- v
    }
    if (grepl("^--umap_metric=", a)) {
      v <- tolower(sub("^--umap_metric=", "", a))
      if (nzchar(v)) UMAP_METRIC <- v
    }
    if (grepl("^--knn_k=", a)) {
      v <- suppressWarnings(as.integer(sub("^--knn_k=", "", a)))
      if (is.finite(v) && v >= 1) KNN_K <- v
    }
    if (grepl("^--knn_pools=", a)) {
      v <- suppressWarnings(as.integer(sub("^--knn_pools=", "", a)))
      if (is.finite(v) && v >= 1) KNN_POOLS <- v
    }
    if (grepl("^--knn_per_label=", a)) {
      v <- suppressWarnings(as.integer(sub("^--knn_per_label=", "", a)))
      if (is.finite(v) && v >= 1) KNN_PER_LABEL <- v
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
if (!(LABEL_COL %in% names(metadata))) {
  stop(sprintf("metadata must contain a '%s' column.", LABEL_COL))
}
if (!("sample_id" %in% names(metadata))) {
  metadata$sample_id <- sprintf("S%03d", seq_len(nrow(metadata)))
}
metadata <- metadata |> mutate(sample_id = as.character(sample_id))
if (!(LABEL_COL %in% names(metadata))) {
  stop(sprintf("metadata must contain a '%s' column.", LABEL_COL))
}

# --------- Collect CLR files ---------
clr_paths <- list.files(output_folder, pattern = "^normalized_.*_clr\\.csv$", full.names = TRUE)

# include raw_clr.csv (as baseline) if present
raw_clr_fp <- file.path(output_folder, "raw_clr.csv")
if (file.exists(raw_clr_fp)) clr_paths <- c(raw_clr_fp, clr_paths)

if (!length(clr_paths)) stop("No CLR matrices found (expected 'raw_clr.csv' or 'normalized_*_clr.csv').")

method_names <- ifelse(basename(clr_paths) == "raw_clr.csv",
                       "Before correction",
                       gsub("^normalized_|_clr\\.csv$", "", basename(clr_paths)))
method_labels <- method_short_label(method_names)
file_list <- setNames(clr_paths, method_labels)
method_levels <- names(file_list)  # lock original order for plotting

# --------- Helpers ---------
`%||%` <- function(a,b) if (is.null(a)) b else a

safe_numeric_matrix <- function(df) {
  num <- dplyr::select(df, where(is.numeric))
  if (!ncol(num)) return(matrix(numeric(0), nrow = nrow(df)))
  keep <- vapply(num, function(z) all(is.finite(z)) && sd(z, na.rm = TRUE) > 0, logical(1))
  as.matrix(num[, keep, drop = FALSE])
}

# CLR transform (used for raw or any non-CLR input)
safe_closure <- function(X) {
  X <- as.matrix(X)
  X[!is.finite(X)] <- 0
  X[X < 0] <- 0
  rs <- rowSums(X)
  rs[rs == 0] <- 1
  sweep(X, 1, rs, "/")
}
clr_transform <- function(X, pseudocount = 1) {
  Xp <- safe_closure(X) + (pseudocount / max(1, ncol(X)))
  L  <- log(Xp)
  sweep(L, 1, rowMeans(L), "-")
}

get_umap2d <- function(X, n_neighbors = 15, min_dist = 0.3, metric = "euclidean") {
  if (!is.matrix(X) || nrow(X) < 3 || ncol(X) < 2) return(NULL)
  Xs <- scale(X)
  umap(Xs, n_neighbors = n_neighbors, min_dist = min_dist, metric = metric,
       n_components = 2, verbose = FALSE)
}

# ---- kNN-based EBM (higher = better) ----
normalized_entropy <- function(p, B) {
  p <- p[p > 0]
  if (!length(p)) return(0)
  -sum(p * log(p)) / log(B)
}
batch_entropy_mixing_knn <- function(
    X2d, batch, k = 50, n_pools = 50, n_per_label = 100, seed = NULL, return_per_anchor = FALSE
) {
  stopifnot(is.matrix(X2d) || is.data.frame(X2d))
  X2d <- as.matrix(X2d); n <- nrow(X2d)
  if (length(batch) != n) stop("length(batch) must equal nrow(X2d)")
  batch <- droplevels(factor(batch)); B <- nlevels(batch)
  if (B <= 1) {
    return(list(mean_entropy = 0, EBM_Score = 0,
                pool_means = rep(0, max(1, n_pools)),
                per_anchor = if (return_per_anchor) numeric(0) else NULL))
  }
  k_eff <- min(max(1, k), n - 1)
  nn <- FNN::get.knn(X2d, k = k_eff)$nn.index
  idx_by_batch <- split(seq_len(n), batch)
  per_pool_means <- numeric(n_pools)
  all_anchor_ent <- if (return_per_anchor) numeric(0) else NULL
  if (!is.null(seed)) set.seed(seed)
  
  for (p in seq_len(n_pools)) {
    anchors <- unlist(lapply(idx_by_batch, function(ix) {
      if (length(ix) >= n_per_label) sample(ix, n_per_label, replace = FALSE)
      else                           sample(ix, n_per_label, replace = TRUE)
    }), use.names = FALSE)
    
    ent <- vapply(anchors, function(i) {
      nbrs <- nn[i, ]
      cnt  <- table(batch[nbrs])
      pvec <- rep(0, B); names(pvec) <- levels(batch)
      pvec[names(cnt)] <- as.numeric(cnt) / sum(cnt)
      normalized_entropy(pvec, B)
    }, numeric(1))
    
    per_pool_means[p] <- mean(ent, na.rm = TRUE)
    if (return_per_anchor) all_anchor_ent <- c(all_anchor_ent, ent)
  }
  
  mean_ebm <- mean(per_pool_means, na.rm = TRUE)
  res <- list(mean_entropy = mean_ebm, EBM_Score = mean_ebm, pool_means = per_pool_means)
  if (return_per_anchor) res$per_anchor <- all_anchor_ent
  res
}

summarise_methods_ebm <- function(ebm_table) {
  ebm_table %>% filter(is.finite(`Entropy score`))
}

pretty_metric <- function(m) {
  m <- tolower(m)
  if (m == "euclidean") return("Euclidean (Aitchison)")
  if (m == "cosine")    return("Cosine")
  paste0(toupper(substr(m,1,1)), substr(m,2,nchar(m)))
}

# --------- Compute EBM per CLR matrix ---------
ebm_tbl <- tibble(Method = character(), `Entropy score` = numeric())

for (nm in names(file_list)) {
  cat("Processing:", nm, "\n")
  df <- read_csv(file_list[[nm]], show_col_types = FALSE)
  
  # Ensure sample_id present
  if (!("sample_id" %in% names(df))) {
    if (nrow(df) == nrow(metadata)) df$sample_id <- metadata$sample_id
    else { warning(sprintf("Skipping %s: no sample_id and row mismatch.", nm)); next }
  }
  df <- df |> mutate(sample_id = as.character(sample_id))
  merged <- inner_join(df, metadata, by = "sample_id")
  if (!nrow(merged)) { warning(sprintf("Skipping %s: no overlap with metadata.", nm)); next }
  
  # Features (CLR matrix or raw)
  X <- safe_numeric_matrix(merged[, setdiff(names(df), "sample_id"), drop = FALSE])
  if (nrow(X) < 3 || ncol(X) < 2) { warning(sprintf("Skipping %s: insufficient features.", nm)); next }
  b <- as.factor(merged[[LABEL_COL]])
  
  # If negatives exist, treat as already-CLR; else CLR-transform
  has_neg <- any(X < 0, na.rm = TRUE)
  Xclr <- if (has_neg) {
    # re-center rows (CLR-like input)
    sweep(X, 1, rowMeans(X, na.rm = TRUE), "-")
  } else {
    clr_transform(X, pseudocount = 1)
  }
  
  um <- tryCatch(
    get_umap2d(Xclr, n_neighbors = UMAP_NEIGHB, min_dist = UMAP_MIN_DIST, metric = UMAP_METRIC),
    error = function(e) NULL
  )
  if (is.null(um)) { warning(sprintf("Skipping %s: UMAP failed.", nm)); next }
  
  out <- tryCatch(
    batch_entropy_mixing_knn(
      um, b, k = KNN_K, n_pools = KNN_POOLS, n_per_label = KNN_PER_LABEL, seed = 42
    ),
    error = function(e) NULL
  )
  if (is.null(out)) { warning(sprintf("Skipping %s: Entropy score failed.", nm)); next }
  
  m  <- out$mean_entropy
  ebm_tbl <- bind_rows(ebm_tbl, tibble(Method = nm, `Entropy score` = m))
}

# --------- Save/plot with special handling for baseline-only ---------
only_baseline <- length(method_levels) == 1L && identical(method_levels, "Before correction")
output_name <- if (only_baseline) "ebm_raw_assessment_pre.csv" else "ebm_raw_assessment_post.csv"

if (only_baseline) {
  base_row <- ebm_tbl %>% filter(Method == "Before correction")
  if (nrow(base_row) == 0) stop("No Entropy score computed for 'Before correction'.")
  
  base_summary <- summarise_methods_ebm(base_row)
  readr::write_csv(base_summary, file.path(output_folder, output_name))
  print(base_summary)
  
  plot_df <- base_row %>% mutate(Method = factor(Method, levels = method_levels))
  y_max <- max(plot_df$`Entropy score`, na.rm = TRUE)
  y_upper <- if (is.finite(y_max)) y_max * 1.2 else NA_real_
  p_ebm <- ggplot(plot_df, aes(x = Method, y = `Entropy score`, fill = Method)) +
    geom_col(width = 0.72, color = "white", linewidth = 0.4, show.legend = FALSE) +
    geom_text(aes(label = sprintf("%.3f", `Entropy score`)), vjust = -0.4, size = 3.2) +
    scale_y_continuous(limits = c(0, y_upper), expand = expansion(mult = c(0, 0.02))) +
    labs(
      title = "Entropy Score (baseline)",
      x = "Method",
      y = "Score"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(hjust = 0.5, face = "plain")
    )

  fig_dims <- apply_fig_overrides(2800 / 300, 1800 / 300, 300)
  tif_path <- file.path(output_folder, "ebm.tif")
  ggsave(tif_path, p_ebm,
         width = fig_dims$width, height = fig_dims$height, dpi = fig_dims$dpi, compression = "lzw")
  
  # No correction recommendation messages
  
} else {
  # Multiple methods: summarise without ranking, KEEP ORIGINAL ORDER in the plot
  ebm_summary <- summarise_methods_ebm(ebm_tbl)
  readr::write_csv(ebm_summary, file.path(output_folder, output_name))
  print(ebm_summary, n = nrow(ebm_summary))
  
  plot_df <- ebm_tbl %>% mutate(Method = factor(Method, levels = method_levels))
  y_max <- max(plot_df$`Entropy score`, na.rm = TRUE)
  y_upper <- if (is.finite(y_max)) y_max * 1.2 else NA_real_
  p_ebm <- ggplot(plot_df, aes(x = Method, y = `Entropy score`, fill = Method)) +
    geom_col(width = 0.72, color = "white", linewidth = 0.4, show.legend = FALSE) +
    geom_text(aes(label = sprintf("%.3f", `Entropy score`)), vjust = -0.4, size = 3.2) +
    scale_y_continuous(limits = c(0, y_upper), expand = expansion(mult = c(0, 0.02))) +
    labs(
      title = "Entropy Score",
      x = "Method",
      y = "Score"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(hjust = 0.5, face = "bold")
    )

  fig_dims <- apply_fig_overrides(2800 / 300, 1800 / 300, 300)
  tif_path <- file.path(output_folder, "ebm.tif")
  ggsave(tif_path, p_ebm,
         width = fig_dims$width, height = fig_dims$height, dpi = fig_dims$dpi, compression = "lzw")
}
