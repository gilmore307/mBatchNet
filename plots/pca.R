# ==== Load Required Libraries ====
suppressPackageStartupMessages({
  library(ggplot2)
  library(readr)
  library(dplyr)
  library(patchwork)  # legend collecting & layout
  library(rlang)
  library(jsonlite)
  library(magick)
})

# ---- helpers ----
mbecUpperCase <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))

create_png_thumbnail <- function(tif_path, width_px = 2000) {
  png_path <- sub("\\.tif$", ".png", tif_path)
  tryCatch({
    img <- magick::image_read(tif_path)
    img <- magick::image_scale(img, paste0(width_px))
    magick::image_write(img, path = png_path, format = "png")
  }, error = function(e) {
    warning(sprintf("Failed to create PNG thumbnail for %s: %s", tif_path, e$message))
  })
}

# Map method codes from filenames to short display labels for figures
method_short_label <- function(x) {
  map <- c(
    qn = "Quantile Normalization",
    bmc = "BMC",
    limma = "Limma",
    conqur = "ConQuR",
    plsda = "PLSDA-batch",
    combat = "ComBat",
    fsqn = "FSQN",
    mmuphin = "MMUPHin",
    ruv = "RUV-III-NB",
    metadict = "MetaDICT",
    pn = "Percentile Normalization",
    fabatch = "FAbatch",
    combatseq = "ComBat-seq",
    debias = "DEBIAS-M"
  )
  sapply(x, function(v){ lv <- tolower(v); if (lv %in% names(map)) map[[lv]] else v })
}

guess_shape_var <- function(meta, batch_col = "batch") {
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
                               panel_cols = 1, panel_rows = 1,
                               max_total_pixels = 35e6) {
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
  width_px <- w * dpi
  height_px <- h * dpi
  total_px <- width_px * height_px
  if (is.finite(total_px) && total_px > max_total_pixels &&
      is.finite(width_px) && is.finite(height_px) &&
      width_px > 0 && height_px > 0) {
    scale <- sqrt(max_total_pixels / total_px)
    w <- w * scale
    h <- h * scale
    width_px <- w * dpi
    height_px <- h * dpi
    message(sprintf(
      "Requested canvas %.0f×%.0f px exceeds limit %.0f; scaling by %.3f to %.0f×%.0f px",
      width_px / scale, height_px / scale, max_total_pixels, scale, width_px, height_px
    ))
  }
  list(width = w, height = h, dpi = dpi)
}

# ==== Read Metadata ====
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
  fallback <- c("target", "group", "condition", "status", "class", "label")
  cand <- fallback[fallback %in% names(metadata)]
  if (length(cand)) {
    label_col <- cand[1]
  } else {
    guessed <- guess_shape_var(metadata, batch_col = "batch")
    if (!is.na(guessed) && guessed %in% names(metadata)) {
      label_col <- guessed
    } else if ("phenotype" %in% names(metadata)) {
      label_col <- "phenotype"
    } else {
      stop("No label column available for PCA plots.")
    }
  }
}

if (!("batch" %in% names(metadata))) {
  metadata$batch <- NA
}

target_var <- label_col

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
compute_pca_frames <- function(df, metadata, model.vars = c("batch","group"), n_pcs = 5) {
  if (!"sample_id" %in% names(df)) {
    if (nrow(df) == nrow(metadata)) df$sample_id <- metadata$sample_id
    else stop("Input lacks 'sample_id' and row count != metadata; can't align samples.")
  }
  df  <- df %>% mutate(sample_id = as.character(sample_id))
  dfm <- inner_join(df, metadata, by = "sample_id")
  
  feature_cols <- setdiff(names(df), "sample_id")
  numeric_df <- dfm %>% select(all_of(feature_cols)) %>% select(where(is.numeric))
  n_features_total <- ncol(numeric_df)
  if (is.null(n_features_total) || length(n_features_total) == 0) n_features_total <- 0L
  n_features_total <- as.integer(n_features_total)
  keep <- if (n_features_total) {
    vapply(numeric_df, function(x) sd(x, na.rm = TRUE) > 0, logical(1))
  } else {
    logical(0)
  }
  X <- as.matrix(numeric_df[, keep, drop = FALSE])
  n_features_used <- as.integer(ncol(X))
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
  
  list(
    plot.df = plot.df,
    metric.df = metric.df,
    used.vars = present,
    n_features_total = n_features_total,
    n_features_used = n_features_used
  )
}

# ==== panel: scatter + marginal densities; legend kept (not collected here) ====
mbecPCAPlot <- function(plot.df, metric.df, model.vars, pca.axes, label=NULL, palette_name = "Batch") {
  
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
    scale_color_manual(values = mbecCols, name = palette_name) +
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
batch_var  <- "batch"
model_vars_common <- unique(Filter(function(x) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) return(FALSE)
  nzchar(as.character(x))
}, c(batch_var, target_var)))
if (!length(model_vars_common)) model_vars_common <- c(batch_var)
message(sprintf("Using color=%s (shape: none)", batch_var))

# ==== build + save PCA panels for each matrix (single legend across ALL) ====
pcs_to_plot <- c(1, 2)
frames_cache <- list()   # <--- build this for assessment summaries

for (nm in names(file_list)) {
  cat("Processing:", nm, "\n")
  df <- read_csv(file_list[[nm]], show_col_types = FALSE)
  
  frames <- compute_pca_frames(df, metadata, model.vars = model_vars_common, n_pcs = max(pcs_to_plot))
  frames_cache[[nm]] <- frames

}

build_pca_plot_list <- function(frames_cache, color_var, palette_label) {
  if (!length(frames_cache) || is.null(color_var) || !nzchar(color_var)) return(list())
  plots <- lapply(names(frames_cache), function(nm) {
    fr <- frames_cache[[nm]]
    if (is.null(fr) || !nrow(fr$plot.df)) return(NULL)
    if (!(color_var %in% names(fr$plot.df))) return(NULL)
    label_nm <- paste(nm, if (identical(palette_label, "Batch")) "Batch" else "Target", sep = " - ")
    mbecPCAPlot(
      plot.df   = fr$plot.df,
      metric.df = fr$metric.df,
      model.vars = c(color_var),
      pca.axes  = pcs_to_plot,
      label     = label_nm,
      palette_name = palette_label
    )
  })
  Filter(function(x) !is.null(x), plots)
}

save_pca_plot_set <- function(plot_list, filename_stub) {
  plot_list <- Filter(function(x) !is.null(x), plot_list)
  if (!length(plot_list)) return(invisible(NULL))
  ncol_grid <- 3
  if (!is.na(opt_fig_ncol) && opt_fig_ncol >= 1) {
    ncol_grid <- max(1, opt_fig_ncol)
  }
  n_panels <- length(plot_list)
  panel_cols <- 1L
  panel_rows <- 1L
  base_fig_width_in  <- 1800 / 300
  base_fig_height_in <- 1200 / 300
  base_col_width_in  <- base_fig_width_in / 3
  base_row_height_in <- base_fig_height_in
  if (n_panels == 1L) {
    combined <- plot_list[[1]] +
      theme(
        legend.position  = "bottom",
        legend.direction = "horizontal",
        legend.box       = "vertical",
        plot.margin      = margin(8, 14, 8, 14)
      )
    w <- base_fig_width_in; h <- base_fig_height_in
  } else {
    panel_cols <- min(ncol_grid, n_panels)
    panel_rows <- ceiling(n_panels / ncol_grid)
    combined <- wrap_plots(plot_list, ncol = ncol_grid) +
      plot_layout(guides = "collect") &
      theme(
        legend.position  = "bottom",
        legend.direction = "horizontal",
        legend.box       = "vertical",
        plot.margin      = margin(8, 14, 8, 14)
      )
    w <- base_col_width_in * panel_cols
    h <- base_row_height_in * panel_rows
  }
  fig_dims <- apply_fig_overrides(w, h, 300, panel_cols, panel_rows)
  tif_path <- file.path(output_folder, paste0(filename_stub, ".tif"))
  ggsave(tif_path,
         plot = combined, width = fig_dims$width, height = fig_dims$height, dpi = fig_dims$dpi, compression = "lzw")
  create_png_thumbnail(tif_path)
  rm(combined, plot_list)
  gc()
}

batch_plots <- build_pca_plot_list(frames_cache, batch_var, "Batch")
save_pca_plot_set(batch_plots, "pca_batch")
target_plots <- build_pca_plot_list(frames_cache, target_var, "Target")
save_pca_plot_set(target_plots, "pca_target")

# =========================
# PCA assessment summaries
# =========================

compute_centroids_pca <- function(scores, batch_var = "batch") {
  scores %>%
    dplyr::group_by(!!rlang::sym(batch_var)) %>%
    dplyr::summarise(PC1 = mean(PC1), PC2 = mean(PC2), .groups = "drop")
}
compute_centroid_distances <- function(centroids) {
  if (nrow(centroids) < 2) return(NA_real_)
  as.numeric(mean(dist(centroids[, c("PC1","PC2")], method = "euclidean")))
}

prepare_group_scores <- function(plot_df, metadata, axes = c("PC1", "PC2"), group_var = "batch") {
  if (!all(c("sample_id", axes) %in% names(plot_df))) return(NULL)
  if (!group_var %in% names(metadata)) return(NULL)
  idx <- match(plot_df$sample_id, metadata$sample_id)
  group_vals <- metadata[[group_var]][idx]
  coords <- as.matrix(plot_df[, axes, drop = FALSE])
  if (!nrow(coords) || ncol(coords) < 2) return(NULL)
  valid <- apply(coords, 1, function(row) all(is.finite(row))) & !is.na(group_vals)
  if (!any(valid)) return(NULL)
  coords <- coords[valid, , drop = FALSE]
  groups <- droplevels(factor(group_vals[valid]))
  if (!nrow(coords) || !nlevels(groups)) return(NULL)
  list(coords = coords, groups = groups)
}

mean_centroid_distance <- function(coords, groups) {
  if (is.null(coords) || !length(coords) || !nrow(coords)) return(NA_real_)
  if (ncol(coords) < 2) return(NA_real_)
  groups <- droplevels(factor(groups))
  levs <- levels(groups)
  centroids <- lapply(levs, function(lev) {
    idx <- which(groups == lev)
    if (!length(idx)) return(NULL)
    sub <- coords[idx, , drop = FALSE]
    sub <- sub[rowSums(is.finite(sub)) == ncol(coords), , drop = FALSE]
    if (!nrow(sub)) return(NULL)
    colMeans(sub, na.rm = TRUE)
  })
  centroids <- centroids[!vapply(centroids, is.null, logical(1))]
  if (length(centroids) < 2) return(NA_real_)
  centroid_mat <- do.call(rbind, centroids)
  if (!all(is.finite(centroid_mat))) return(NA_real_)
  as.numeric(mean(dist(centroid_mat, method = "euclidean")))
}

circle_overlap_ratio <- function(center_a, radius_a, center_b, radius_b) {
  if (!is.finite(radius_a) || !is.finite(radius_b) ||
      radius_a <= 0 || radius_b <= 0) {
    return(NA_real_)
  }
  diff_vec <- center_a - center_b
  if (any(!is.finite(diff_vec))) return(NA_real_)
  d <- sqrt(sum(diff_vec^2))
  if (!is.finite(d)) return(NA_real_)
  if (d <= 1e-12 && abs(radius_a - radius_b) <= 1e-12) {
    return(1)
  }
  if (d >= radius_a + radius_b) {
    return(0)
  }
  if (d <= abs(radius_a - radius_b)) {
    inter_area <- pi * min(radius_a, radius_b)^2
  } else {
    ang_a <- (d^2 + radius_a^2 - radius_b^2) / (2 * d * radius_a)
    ang_b <- (d^2 + radius_b^2 - radius_a^2) / (2 * d * radius_b)
    ang_a <- max(min(ang_a, 1), -1)
    ang_b <- max(min(ang_b, 1), -1)
    part1 <- radius_a^2 * acos(ang_a)
    part2 <- radius_b^2 * acos(ang_b)
    part3 <- 0.5 * sqrt(max(0, (-d + radius_a + radius_b) *
                              (d + radius_a - radius_b) *
                              (d - radius_a + radius_b) *
                              (d + radius_a + radius_b)))
    inter_area <- part1 + part2 - part3
  }
  union_area <- pi * radius_a^2 + pi * radius_b^2 - inter_area
  if (!is.finite(inter_area) || !is.finite(union_area) || union_area <= 0) {
    return(NA_real_)
  }
  inter_area / union_area
}

group_ellipse_params <- function(coords, groups, level = 0.95) {
  groups <- droplevels(factor(groups))
  levs <- levels(groups)
  if (length(levs) < 2) return(list())
  chi <- sqrt(qchisq(level, df = 2))
  params <- lapply(levs, function(lev) {
    idx <- which(groups == lev)
    if (!length(idx)) return(NULL)
    sub <- coords[idx, , drop = FALSE]
    sub <- sub[rowSums(is.finite(sub)) == ncol(coords), , drop = FALSE]
    if (nrow(sub) < 3) return(NULL)
    covmat <- tryCatch(stats::cov(sub, use = "complete.obs"), error = function(e) NULL)
    if (is.null(covmat)) return(NULL)
    eig <- tryCatch(eigen(covmat, symmetric = TRUE), error = function(e) NULL)
    if (is.null(eig)) return(NULL)
    axis_lengths <- sqrt(pmax(eig$values, 0))
    if (length(axis_lengths) < 2) return(NULL)
    radius <- sqrt(axis_lengths[1] * axis_lengths[2]) * chi
    if (!is.finite(radius) || radius <= 0) return(NULL)
    center <- colMeans(sub, na.rm = TRUE)
    if (any(!is.finite(center))) return(NULL)
    list(center = center, radius = radius)
  })
  params[!vapply(params, is.null, logical(1))]
}

ellipse_overlap_ratio <- function(coords, groups, level = 0.95) {
  if (is.null(coords) || !length(coords) || !nrow(coords) || ncol(coords) < 2) {
    return(NA_real_)
  }
  params <- group_ellipse_params(coords, groups, level = level)
  if (length(params) < 2) return(NA_real_)
  if (length(params) == 2) {
    ratios <- circle_overlap_ratio(params[[1]]$center, params[[1]]$radius,
                                   params[[2]]$center, params[[2]]$radius)
    return(if (is.finite(ratios)) ratios else NA_real_)
  }
  combos <- utils::combn(seq_along(params), 2, simplify = FALSE)
  ratios <- vapply(combos, function(idx) {
    p1 <- params[[idx[1]]]
    p2 <- params[[idx[2]]]
    circle_overlap_ratio(p1$center, p1$radius, p2$center, p2$radius)
  }, numeric(1))
  ratios <- ratios[is.finite(ratios)]
  if (!length(ratios)) return(NA_real_)
  mean(ratios)
}

compute_group_metrics <- function(plot_df, metadata, axes, group_var) {
  prep <- prepare_group_scores(plot_df, metadata, axes = axes, group_var = group_var)
  if (is.null(prep)) {
    return(list(centroid = NA_real_, overlap = NA_real_))
  }
  coords <- prep$coords
  groups <- prep$groups
  list(
    centroid = mean_centroid_distance(coords, groups),
    overlap = ellipse_overlap_ratio(coords, groups)
  )
}

target_vs_batch_dominance <- function(target_distance, batch_distance) {
  if (!is.finite(target_distance) || !is.finite(batch_distance)) return(NA_real_)
  target_distance - batch_distance
}

summarise_pca_method <- function(fr, method_name, metadata, batch_var = "batch", target_var = "target") {
  batch_stats <- compute_group_metrics(fr$plot.df, metadata, axes = c("PC1", "PC2"), group_var = batch_var)
  target_stats <- compute_group_metrics(fr$plot.df, metadata, axes = c("PC1", "PC2"), group_var = target_var)
  tibble::tibble(
    Method = method_name,
    Centroid_Distance_Batch = batch_stats$centroid,
    Centroid_Distance_Target = target_stats$centroid,
    Ellipse_Overlap_Batch = batch_stats$overlap,
    Ellipse_Overlap_Target = target_stats$overlap,
    Target_vs_Batch_Centroid_Delta = target_vs_batch_dominance(target_stats$centroid, batch_stats$centroid)
  )
}
only_baseline <- (length(frames_cache) == 1L) && identical(names(frames_cache), "Before correction")
output_name <- if (only_baseline) "pca_raw_assessment_pre.csv" else "pca_raw_assessment_post.csv"

if (only_baseline) {
  # ---- Assess RAW only (no ranking) ----
  m <- "Before correction"
  fr <- frames_cache[[m]]
  stopifnot(!is.null(fr))
  assess_df <- summarise_pca_method(fr, m, metadata, batch_var = batch_var, target_var = target_var)
  if (is.null(assess_df)) assess_df <- tibble::tibble()

  print(assess_df, n = nrow(assess_df))
  readr::write_csv(assess_df, file.path(output_folder, output_name))

} else {
  # ---- Summarise multiple matrices without ranking ----
  rows <- lapply(names(frames_cache), function(m) {
    fr <- frames_cache[[m]]
    summarise_pca_method(fr, m, metadata, batch_var = batch_var, target_var = target_var)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  assessment_tbl <- if (length(rows)) {
    dplyr::bind_rows(rows) %>% dplyr::arrange(Method)
  } else {
    tibble::tibble()
  }

  print(assessment_tbl, n = nrow(assessment_tbl))
  readr::write_csv(assessment_tbl, file.path(output_folder, output_name))
}
