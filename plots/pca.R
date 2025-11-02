# ==== Load Required Libraries ====
suppressPackageStartupMessages({
  library(ggplot2)
  library(readr)
  library(dplyr)
  library(patchwork)  # legend collecting & layout
  library(rlang)
})

# ---- helpers ----
mbecUpperCase <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))

# Map method codes from filenames to short display labels for figures
method_short_label <- function(x) {
  map <- c(
    qn = "QN",
    bmc = "BMC",
    limma = "Limma",
    conqur = "ConQuR",
    plsda = "PLSDA-batch",
    combat = "ComBat",
    fsqn = "FSQN",
    mmuphin = "MMUPHin",
    ruv = "RUV-III-NB",
    metadict = "MetaDICT",
    svd = "SVD",
    pn = "PN",
    fabatch = "FAbatch",
    combatseq = "ComBat-seq",
    debias = "DEBIAS-M"
  )
  sapply(x, function(v){ lv <- tolower(v); if (lv %in% names(map)) map[[lv]] else v })
}

guess_shape_var <- function(meta, batch_col = "batch_id") {
  cand <- c("group","phenotype","condition","status","case_control","class","disease","label")
  hit <- cand[cand %in% names(meta)]
  if (length(hit)) return(hit[1])
  facs <- names(Filter(function(v) {
    !is.numeric(meta[[v]]) && !is.integer(meta[[v]]) &&
      length(unique(meta[[v]])) <= 12
  }, meta))
  facs <- setdiff(facs, c("sample_id", batch_col))
  if (length(facs)) return(facs[1])
  return(NA_character_)
}

# union bounds of 95% normal ellipses across groups
ellipse_union_bounds <- function(df_scores, group_var, level = 0.95, n = 240) {
  if (!nrow(df_scores)) return(list(x = c(0,0), y = c(0,0)))
  chi <- sqrt(qchisq(level, df = 2))
  xmins <- c(); xmaxs <- c(); ymins <- c(); ymaxs <- c()
  levs <- levels(df_scores[[group_var]])
  if (is.null(levs)) levs <- unique(df_scores[[group_var]])
  for (lev in levs) {
    sub <- df_scores[df_scores[[group_var]] == lev, c("PCX","PCY"), drop = FALSE]
    if (nrow(sub) < 3 || any(!is.finite(as.matrix(sub)))) next
    S <- tryCatch(stats::cov(sub, use = "complete.obs"), error = function(e) NULL)
    mu <- colMeans(sub, na.rm = TRUE)
    if (is.null(S) || any(!is.finite(S))) next
    eg <- eigen(S, symmetric = TRUE)
    if (any(!is.finite(eg$values))) next
    t <- seq(0, 2*pi, length.out = n)
    R <- eg$vectors %*% diag(sqrt(pmax(eg$values, 0)))
    pts <- t(chi * R %*% rbind(cos(t), sin(t)))
    pts <- sweep(pts, 2, mu, FUN = "+")
    xmins <- c(xmins, min(pts[,1], na.rm = TRUE))
    xmaxs <- c(xmaxs, max(pts[,1], na.rm = TRUE))
    ymins <- c(ymins, min(pts[,2], na.rm = TRUE))
    ymaxs <- c(ymaxs, max(pts[,2], na.rm = TRUE))
  }
  if (!length(xmins) || !length(xmaxs) || !length(ymins) || !length(ymaxs)) {
    xr <- range(df_scores$PCX, na.rm = TRUE)
    yr <- range(df_scores$PCY, na.rm = TRUE)
    return(list(x = xr, y = yr))
  }
  list(x = c(min(xmins, na.rm = TRUE), max(xmaxs, na.rm = TRUE)),
       y = c(min(ymins, na.rm = TRUE), max(ymaxs, na.rm = TRUE)))
}

safe_range <- function(v) {
  v <- v[is.finite(v)]
  if (!length(v)) c(0, 0) else range(v, na.rm = TRUE)
}

finite_range <- function(...) {
  v <- unlist(list(...))
  v <- v[is.finite(v)]
  if (!length(v)) c(0, 0) else range(v, na.rm = TRUE)
}

safe_pad <- function(r, frac = 0.12) {
  dx <- diff(r)
  if (!is.finite(dx) || dx <= 0) return(1e-6) else dx * frac
}

panel_limits_for_scores <- function(scores, group_var = "batch", level = 0.95) {
  ell_bounds <- ellipse_union_bounds(scores, group_var, level = level, n = 240)
  xr <- safe_range(scores$PCX)
  yr <- safe_range(scores$PCY)
  xlim <- finite_range(xr, ell_bounds$x)
  ylim <- finite_range(yr, ell_bounds$y)
  pad_x <- safe_pad(xlim)
  pad_y <- safe_pad(ylim)
  list(
    xlim = c(xlim[1] - pad_x, xlim[2] + pad_x),
    ylim = c(ylim[1] - pad_y, ylim[2] + pad_y)
  )
}

# ==== Read UID argument ====
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
opt_fig_per_panel <- FALSE

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
  if (grepl("^--fig-per-panel=", a)) {
    val <- tolower(sub("^--fig-per-panel=", "", a))
    opt_fig_per_panel <- val %in% c("1", "true", "yes", "y")
  }
}

apply_fig_overrides <- function(width_in, height_in, default_dpi = 300,
                               panel_cols = 1, panel_rows = 1) {
  dpi <- if (is.na(opt_fig_dpi) || opt_fig_dpi <= 0) default_dpi else opt_fig_dpi
  w <- width_in
  h <- height_in
  panel_cols <- max(1, as.integer(panel_cols))
  panel_rows <- max(1, as.integer(panel_rows))
  if (!is.na(opt_fig_width_px) && opt_fig_width_px > 0 && dpi > 0) {
    per_panel <- opt_fig_width_px / dpi
    if (isTRUE(opt_fig_per_panel)) {
      w <- per_panel * panel_cols
    } else {
      w <- per_panel
    }
  }
  if (!is.na(opt_fig_height_px) && opt_fig_height_px > 0 && dpi > 0) {
    per_panel <- opt_fig_height_px / dpi
    if (isTRUE(opt_fig_per_panel)) {
      h <- per_panel * panel_rows
    } else {
      h <- per_panel
    }
  }
  list(width = w, height = h, dpi = dpi)
}

# ==== Read Metadata ====
metadata <- read_csv(file.path(output_folder, "metadata.csv"), show_col_types = FALSE)
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

# ==== build plot.df + metric.df like mbecPCA would ====
# ensures consistent factor levels across panels so legend can be shared
compute_pca_frames <- function(df, metadata, model.vars = c("batch_id","group"), n_pcs = 5) {
  if (!"sample_id" %in% names(df)) {
    if (nrow(df) == nrow(metadata)) df$sample_id <- metadata$sample_id
    else stop("Input lacks 'sample_id' and row count != metadata; can't align samples.")
  }
  df  <- df %>% mutate(sample_id = as.character(sample_id))
  dfm <- inner_join(df, metadata, by = "sample_id")
  
  feature_cols <- setdiff(names(df), "sample_id")
  X <- dfm %>% select(all_of(feature_cols)) %>% select(where(is.numeric))
  keep <- vapply(X, function(x) sd(x, na.rm = TRUE) > 0, logical(1))
  X <- as.matrix(X[, keep, drop = FALSE])
  if (!ncol(X)) stop("No variable numeric features remain for PCA.")
  
  # NOTE: If your inputs are CLR already, consider scale. = FALSE for strict Aitchison PCA.
  pc <- prcomp(X, center = TRUE, scale. = TRUE)
  k  <- min(n_pcs, ncol(pc$x))
  
  ve       <- (pc$sdev^2) / sum(pc$sdev^2)
  axis.min <- apply(pc$x[, seq_len(k), drop = FALSE], 2, min)
  axis.max <- apply(pc$x[, seq_len(k), drop = FALSE], 2, max)
  
  metric.df <- data.frame(
    axis.min      = as.numeric(axis.min),
    axis.max      = as.numeric(axis.max),
    var.explained = round(100 * ve[seq_len(k)], 2),
    stringsAsFactors = FALSE
  )
  
  plot.df <- data.frame(
    sample_id = dfm$sample_id,
    as.data.frame(pc$x[, seq_len(k), drop = FALSE]),
    stringsAsFactors = FALSE
  )
  colnames(plot.df)[2:(k + 1)] <- paste0("PC", seq_len(k))
  
  # attach *consistent* factor levels from metadata so legends match across panels
  present <- intersect(model.vars, names(dfm))
  for (v in present) {
    levs <- unique(as.character(metadata[[v]]))
    plot.df[[v]] <- factor(as.character(dfm[[v]]), levels = levs)
  }
  
  list(plot.df = plot.df, metric.df = metric.df, used.vars = present)
}

# ==== panel: scatter + marginal densities; legend kept (not collected here) ====
mbecPCAPlot <- function(plot.df, metric.df, model.vars, pca.axes, label=NULL) {
  
  mbecCols <- c("#9467bd","#BCBD22","#2CA02C","#E377C2","#1F77B4","#FF7F0E",
                "#AEC7E8","#FFBB78","#98DF8A","#D62728","#FF9896","#C5B0D5",
                "#8C564B","#C49C94","#F7B6D2","#7F7F7F","#C7C7C7","#DBDB8D",
                "#17BECF","#9EDAE5")
  
  var.color <- model.vars[1]
  
  xcol <- colnames(plot.df)[pca.axes[1] + 1]
  ycol <- colnames(plot.df)[pca.axes[2] + 1]

  x.label <- paste0(xcol, ": ", metric.df$var.explained[pca.axes[1]], "% expl.var")
  y.label <- paste0(ycol, ": ", metric.df$var.explained[pca.axes[2]], "% expl.var")

  # compute per-panel limits using ellipse/point extents (no global synchronisation)
  scores <- data.frame(
    PCX = plot.df[[xcol]],
    PCY = plot.df[[ycol]],
    batch = if (var.color %in% names(plot.df)) droplevels(plot.df[[var.color]]) else factor(1)
  )
  limits <- panel_limits_for_scores(scores, group_var = "batch", level = 0.95)
  xlim <- limits$xlim
  ylim <- limits$ylim

  pmar <- margin(10, 16, 10, 16)

  # main scatter (legend source)
  pMain <- ggplot(plot.df, aes(x = !!sym(xcol), y = !!sym(ycol), colour = !!sym(var.color))) +
    geom_point(shape = 16, size = 1.3, alpha = 0.85) +
    stat_ellipse(aes(group = !!sym(var.color)),
                 type = "norm", level = 0.95,
                 linewidth = 0.7, linetype = 1, show.legend = FALSE, na.rm = TRUE) +
    scale_color_manual(values = mbecCols, name = "Batch") +
    guides(
      colour = guide_legend(order = 1, nrow = 1, byrow = TRUE)  # many batches on one row
    ) +
    labs(title = NULL) +
    scale_x_continuous(limits = xlim, expand = expansion(mult = c(0.02, 0.02))) +
    scale_y_continuous(limits = ylim, expand = expansion(mult = c(0.02, 0.02))) +
    xlab(x.label) + ylab(y.label) + theme_bw() +
    theme(
      panel.background = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_text(size = 12, face = "plain"),
      axis.title.y = element_text(size = 12, face = "plain"),
      legend.position = 'bottom',
      legend.direction = 'horizontal',
      legend.box = 'vertical',
      plot.margin = pmar
    )
  
  # top density (PC1) — no legend
  pTop <- ggplot(plot.df, aes(x = !!sym(xcol))) +
    geom_density(aes(fill = !!sym(var.color)),
                 linewidth = 0.3, alpha = 0.5, show.legend = FALSE) +
    ylab("Density") +
    scale_fill_manual(values = mbecCols, guide = "none") +
    scale_x_continuous(limits = xlim, expand = expansion(mult = c(0.02, 0.02))) +
    theme_bw() +
    theme(
      panel.background = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = 'none',
      axis.title.y = element_text(size = 12, face = "plain"),
      axis.title.x = element_blank(),
      plot.margin = pmar
    )
  
  # right density (PC2)
  pRight <- ggplot(plot.df, aes(y = !!sym(ycol))) +
    geom_density(
      aes(x = after_stat(density), fill = !!sym(var.color)),
      linewidth = 0.3, alpha = 0.5, orientation = "y", show.legend = FALSE
    ) +
    xlab(NULL) + ylab("Density") +
    scale_fill_manual(values = mbecCols, guide = "none") +
    scale_y_continuous(limits = ylim, expand = expansion(mult = c(0.02, 0.02))) +
    theme_bw() +
    theme(
      panel.background = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      axis.title.x = element_text(size = 12, face = "plain"),
      axis.title.y = element_text(size = 12, face = "plain"),
      plot.margin = margin(10, 16, 10, 16)
    )
  
  # assemble (DON'T collect here; we'll collect once globally)
  design <- "
A#
CB
"
  assembled <- (pTop + pRight + pMain) +
    plot_layout(design = design, widths = c(3, 1), heights = c(1.6, 3.2))

  if (!is.null(label) && nzchar(label)) {
    title_strip <- ggplot() +
      labs(title = label) +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.margin = margin(0, 16, 0, 16)
      )
    assembled <- (title_strip / assembled) +
      plot_layout(heights = c(0, 1))
  }

  assembled
}

# ==== choose covariates (auto-detect shape var) ====
batch_var  <- "batch_id"
model_vars <- c(batch_var)
message(sprintf("Using color=%s (shape: none)", batch_var))

# ==== build + save PCA panels for each matrix (single legend across ALL) ====
pcs_to_plot <- c(1, 2)
frames_cache <- list()   # <--- build this for assessment summaries
plots <- list()

for (nm in names(file_list)) {
  cat("Processing:", nm, "\n")
  df <- read_csv(file_list[[nm]], show_col_types = FALSE)
  
  frames <- compute_pca_frames(df, metadata, model.vars = model_vars, n_pcs = max(pcs_to_plot))
  frames_cache[[nm]] <- frames
  
  plt <- mbecPCAPlot(
    plot.df   = frames$plot.df,
    metric.df = frames$metric.df,
    model.vars = frames$used.vars,
    pca.axes  = pcs_to_plot,
    label     = nm
  )
  
  plots[[nm]] <- plt
}

# ---- Combine ALL panels and keep ONLY ONE legend at the bottom (horizontal) ----
ncol_grid <- 2
if (!is.na(opt_fig_ncol) && opt_fig_ncol >= 1) {
  ncol_grid <- max(1, opt_fig_ncol)
}
n_panels  <- length(plots)
if (n_panels == 0L) stop("No PCA panels to plot.")

panel_cols <- 1L
panel_rows <- 1L
if (n_panels == 1L) {
  combined <- plots[[1]] +
    theme(
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.box       = "vertical",
      plot.margin      = margin(8, 14, 8, 14)
    )
  w <- 9.5; h <- 6
} else {
  panel_cols <- min(ncol_grid, n_panels)
  panel_rows <- ceiling(n_panels / ncol_grid)
  combined <- wrap_plots(plots, ncol = ncol_grid) +
    plot_layout(guides = "collect") &
    theme(
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.box       = "vertical",
      plot.margin      = margin(8, 14, 8, 14)
    )
  w <- 9.5 * panel_cols
  h <- 6   * panel_rows
}

fig_dims <- apply_fig_overrides(w, h, 300, panel_cols, panel_rows)
ggsave(file.path(output_folder, "pca.png"),
       plot = combined, width = fig_dims$width, height = fig_dims$height, dpi = fig_dims$dpi)
ggsave(file.path(output_folder, "pca.tif"),
       plot = combined, width = fig_dims$width, height = fig_dims$height, dpi = fig_dims$dpi, compression = "lzw")

# =========================
# PCA assessment summaries
# =========================

compute_centroids_pca <- function(scores, batch_var = "batch_id") {
  scores %>%
    dplyr::group_by(!!rlang::sym(batch_var)) %>%
    dplyr::summarise(PC1 = mean(PC1), PC2 = mean(PC2), .groups = "drop")
}
compute_centroid_distances <- function(centroids) {
  if (nrow(centroids) < 2) return(NA_real_)
  as.numeric(mean(dist(centroids[, c("PC1","PC2")], method = "euclidean")))
}
only_baseline <- (length(frames_cache) == 1L) && identical(names(frames_cache), "Before correction")
output_name <- if (only_baseline) "pca_raw_assessment_pre.csv" else "pca_raw_assessment_post.csv"

if (only_baseline) {
  # ---- Assess RAW only (no ranking) ----
  m <- "Before correction"
  fr <- frames_cache[[m]]
  stopifnot(!is.null(fr))
  
  # Build scores with batch
  md <- metadata[match(fr$plot.df$sample_id, metadata$sample_id), , drop = FALSE]
  scores <- fr$plot.df %>%
    dplyr::select(sample_id, PC1, PC2) %>%
    dplyr::mutate(batch_id = factor(md$batch_id))

  cents <- compute_centroids_pca(scores, "batch_id")
  D_between <- compute_centroid_distances(cents)
  assess_df <- tibble::tibble(
    Method         = m,
    Batch_Distance = D_between
  )

  print(assess_df)
  readr::write_csv(assess_df, file.path(output_folder, output_name))

} else {
  # ---- Summarise multiple matrices without ranking ----
  rank_tbl <- dplyr::tibble(
    Method = character(),
    Batch_Distance = numeric()
  )
  
  for (m in names(frames_cache)) {
    fr <- frames_cache[[m]]
    if (!all(c("PC1","PC2","sample_id") %in% names(fr$plot.df))) next
    md <- metadata[match(fr$plot.df$sample_id, metadata$sample_id), , drop = FALSE]
    scores <- fr$plot.df %>%
      dplyr::select(sample_id, PC1, PC2) %>%
      dplyr::mutate(batch_id = factor(md$batch_id))
    cents <- compute_centroids_pca(scores, "batch_id")
    D <- compute_centroid_distances(cents)
    rank_tbl <- dplyr::bind_rows(rank_tbl, dplyr::tibble(
      Method = m,
      Batch_Distance = D
    ))
  }

  assessment_tbl <- rank_tbl %>%
    dplyr::arrange(Method)

  print(assessment_tbl, n = nrow(assessment_tbl))
  readr::write_csv(assessment_tbl, file.path(output_folder, output_name))
}
