# ==== Libraries ====
suppressPackageStartupMessages({
  library(ggplot2)
  library(readr)
  library(dplyr)
  library(patchwork)  # layouts + legend collection
  library(rlang)
  library(vegan)      # Bray-Curtis
})

# ---- shared styling ----
source(file.path("plots", "plot_theme.R"))
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

# ==== Helpers (robust ranges/padding) ====
safe_range <- function(v) {
  v <- v[is.finite(v)]
  if (!length(v)) c(0, 0) else range(v, na.rm = TRUE)
}
finite_range <- function(...) {
  v <- unlist(list(...))
  v <- v[is.finite(v)]
  if (!length(v)) c(0, 0) else range(v, na.rm = TRUE)
}
safe_pad <- function(r, frac = 0.06) {
  dx <- diff(r)
  if (!is.finite(dx) || dx <= 0) return(1e-6) else dx * frac
}

# ==== Ellipse union bounds (falls back to point ranges if needed) ====
ellipse_union_bounds <- function(df_scores, group_var, level = 0.95, n = 240) {
  if (!nrow(df_scores)) return(list(x = c(0,0), y = c(0,0)))
  chi <- sqrt(qchisq(level, df = 2))
  xmins <- c(); xmaxs <- c(); ymins <- c(); ymaxs <- c()
  levs <- levels(df_scores[[group_var]])
  if (is.null(levs)) levs <- unique(df_scores[[group_var]])
  for (lev in levs) {
    sub <- df_scores[df_scores[[group_var]] == lev, c("AX1","AX2"), drop = FALSE]
    if (nrow(sub) < 3 || any(!is.finite(as.matrix(sub)))) next
    S  <- tryCatch(stats::cov(sub, use = "complete.obs"), error = function(e) NULL)
    mu <- colMeans(sub, na.rm = TRUE)
    if (is.null(S) || any(!is.finite(S))) next
    eg <- eigen(S, symmetric = TRUE)
    if (any(!is.finite(eg$values))) next
    t  <- seq(0, 2*pi, length.out = n)
    R  <- eg$vectors %*% diag(sqrt(pmax(eg$values, 0)))
    pts <- t(chi * R %*% rbind(cos(t), sin(t)))
    pts <- sweep(pts, 2, mu, FUN = "+")
    xmins <- c(xmins, min(pts[,1], na.rm = TRUE))
    xmaxs <- c(xmaxs, max(pts[,1], na.rm = TRUE))
    ymins <- c(ymins, min(pts[,2], na.rm = TRUE))
    ymaxs <- c(ymaxs, max(pts[,2], na.rm = TRUE))
  }
  if (!length(xmins) || !length(ymins)) {
    return(list(x = safe_range(df_scores$AX1), y = safe_range(df_scores$AX2)))
  }
  list(x = c(min(xmins, na.rm = TRUE), max(xmaxs, na.rm = TRUE)),
       y = c(min(ymins, na.rm = TRUE), max(ymaxs, na.rm = TRUE)))
}

# ==== Compositional helpers ====
safe_closure <- function(X) {
  rs <- rowSums(X, na.rm = TRUE)
  zero_rows <- which(rs == 0 | !is.finite(rs))
  if (length(zero_rows)) {
    X[zero_rows, ] <- 1 / ncol(X)
    rs <- rowSums(X, na.rm = TRUE)
  }
  sweep(X, 1, rs, "/")
}
clr_transform <- function(X) {
  Xc <- safe_closure(X)
  for (i in seq_len(nrow(Xc))) {
    xi <- Xc[i, ]
    pos <- xi > 0 & is.finite(xi)
    if (!any(pos)) {
      xi[] <- 1 / length(xi); pos <- xi > 0
    }
    if (any(!pos)) {
      minpos <- min(xi[pos], na.rm = TRUE)
      repl <- min(minpos * 0.5, 1e-8)
      xi[!pos] <- repl
      xi <- xi / sum(xi)
    }
    Xc[i, ] <- xi
  }
  L <- log(Xc)
  sweep(L, 1, rowMeans(L), "-")
}

# ==== Args / IO ====
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

metadata <- read_csv(file.path(output_folder, "metadata.csv"), show_col_types = FALSE)
if (!("sample_id" %in% names(metadata))) {
  metadata$sample_id <- sprintf("S%03d", seq_len(nrow(metadata)))
}
metadata <- metadata |> mutate(sample_id = as.character(sample_id))

# If batch_id column doesn't exist, create it with NA
if (!("batch_id" %in% names(metadata))) {
  metadata$batch_id <- NA  # or some default value
}

# ---- Find normalized files ----
clr_paths <- list.files(output_folder, pattern = "^normalized_.*_clr\\.csv$", full.names = TRUE)
tss_paths <- list.files(output_folder, pattern = "^normalized_.*_tss\\.csv$", full.names = TRUE)

# Fallback: if no suffix-specific outputs, use any normalized_*.csv for both
if (!length(clr_paths) && !length(tss_paths)) {
  any_paths <- list.files(output_folder, pattern = "^normalized_.*\\.csv$", full.names = TRUE)
  clr_paths <- any_paths
  tss_paths <- any_paths
}

name_from <- function(paths, suffix) gsub(paste0("^normalized_|_", suffix, "\\.csv$"), "", basename(paths))
file_list_clr <- setNames(clr_paths, if (length(clr_paths)) method_short_label(name_from(clr_paths, "clr")) else character())
file_list_tss <- setNames(tss_paths, if (length(tss_paths)) method_short_label(name_from(tss_paths, "tss")) else character())

# Include raw_clr.csv / raw_tss.csv as "Before correction" if present
raw_clr_fp <- file.path(output_folder, "raw_clr.csv")
raw_tss_fp <- file.path(output_folder, "raw_tss.csv")
if (file.exists(raw_clr_fp)) file_list_clr <- c("Before correction" = raw_clr_fp, file_list_clr)
if (file.exists(raw_tss_fp)) file_list_tss <- c("Before correction" = raw_tss_fp, file_list_tss)

if (!length(file_list_clr) && !length(file_list_tss)) {
  stop("No normalized files found (expected raw_clr.csv/raw_tss.csv and/or normalized_*_clr.csv / normalized_*_tss.csv) in ", output_folder)
}

# ==== PCoA frames (Aitchison on CLR) ====
compute_pcoa_frames_aitch <- function(df, metadata, model.vars = c("batch_id","phenotype"),
                                      n_axes = 5) {
  if (!"sample_id" %in% names(df)) {
    if (nrow(df) == nrow(metadata)) df$sample_id <- metadata$sample_id
    else stop("Input lacks 'sample_id' and row count != metadata; can't align samples.")
  }
  df  <- df %>% mutate(sample_id = as.character(sample_id))
  dfm <- inner_join(df, metadata, by = "sample_id")
  feat_cols <- setdiff(names(df), "sample_id")
  X <- dfm %>% select(all_of(feat_cols)) %>% select(where(is.numeric))
  keep <- vapply(X, function(x) sd(x, na.rm = TRUE) > 0, logical(1))
  X <- as.matrix(X[, keep, drop = FALSE])
  if (!ncol(X)) stop("No variable numeric features remain for PCoA (CLR).")
  
  # If negatives exist, assume already CLR; else do CLR from positive data
  has_neg <- any(X < 0, na.rm = TRUE)
  Xclr <- if (has_neg) sweep(X, 1, rowMeans(X, na.rm = TRUE), "-") else clr_transform(X)
  d <- dist(Xclr, method = "euclidean")
  
  # PCoA
  if (requireNamespace("ape", quietly = TRUE)) {
    pc <- ape::pcoa(d)
    k  <- min(n_axes, ncol(pc$vectors))
    sc <- pc$vectors[, 1:k, drop = FALSE]
    vals <- pc$values
    rel  <- if ("Rel_corr_eig" %in% names(vals)) vals$Rel_corr_eig else vals$Relative_eig
    var_expl <- round(100 * rel[1:k], 2)
  } else {
    cm <- cmdscale(d, k = n_axes, eig = TRUE, add = TRUE)
    sc <- cm$points
    k  <- ncol(sc)
    rel_eig <- cm$eig / sum(cm$eig[cm$eig > 0], na.rm = TRUE)
    var_expl <- round(100 * rel_eig[1:k], 2)
  }
  
  axis.min <- apply(sc, 2, min, na.rm = TRUE)
  axis.max <- apply(sc, 2, max, na.rm = TRUE)
  metric.df <- data.frame(axis.min = as.numeric(axis.min),
                          axis.max = as.numeric(axis.max),
                          var.explained = as.numeric(var_expl))
  plot.df <- data.frame(sample_id = dfm$sample_id, as.data.frame(sc), check.names = FALSE)
  colnames(plot.df)[2:(k + 1)] <- paste0("PCo", seq_len(k))
  
  present <- intersect(model.vars, names(dfm))
  for (v in present) {
    levs <- unique(as.character(metadata[[v]]))
    plot.df[[v]] <- factor(as.character(dfm[[v]]), levels = levs)
  }
  list(plot.df = plot.df, metric.df = metric.df, used.vars = present)
}

# ==== PCoA frames (Bray–Curtis on TSS) ====
compute_pcoa_frames_bray <- function(df, metadata, model.vars = c("batch_id","phenotype"),
                                     n_axes = 5) {
  if (!"sample_id" %in% names(df)) {
    if (nrow(df) == nrow(metadata)) df$sample_id <- metadata$sample_id
    else stop("Input lacks 'sample_id' and row count != metadata; can't align samples.")
  }
  df  <- df %>% mutate(sample_id = as.character(sample_id))
  dfm <- inner_join(df, metadata, by = "sample_id")
  feat_cols <- setdiff(names(df), "sample_id")
  X <- dfm %>% select(all_of(feat_cols)) %>% select(where(is.numeric)) %>% as.matrix()
  
  # Ensure non-negative & close rows to TSS (proportions)
  X[!is.finite(X)] <- 0
  X[X < 0] <- 0
  Xtss <- safe_closure(X)
  
  # Bray–Curtis (vegan::vegdist)
  d <- vegan::vegdist(Xtss, method = "bray")
  
  # PCoA with Cailliez correction (robust for non-Euclidean)
  if (requireNamespace("ape", quietly = TRUE)) {
    pc <- ape::pcoa(as.matrix(d), correction = "cailliez")
    k  <- min(n_axes, ncol(pc$vectors))
    sc <- pc$vectors[, 1:k, drop = FALSE]
    vals <- pc$values
    rel  <- if ("Rel_corr_eig" %in% names(vals)) vals$Rel_corr_eig else vals$Relative_eig
    var_expl <- round(100 * rel[1:k], 2)
  } else {
    cm <- cmdscale(as.matrix(d), k = n_axes, eig = TRUE, add = TRUE)
    sc <- cm$points
    k  <- ncol(sc)
    rel_eig <- cm$eig / sum(cm$eig[cm$eig > 0], na.rm = TRUE)
    var_expl <- round(100 * rel_eig[1:k], 2)
  }
  
  axis.min <- apply(sc, 2, min, na.rm = TRUE)
  axis.max <- apply(sc, 2, max, na.rm = TRUE)
  metric.df <- data.frame(axis.min = as.numeric(axis.min),
                          axis.max = as.numeric(axis.max),
                          var.explained = as.numeric(var_expl))
  plot.df <- data.frame(sample_id = dfm$sample_id, as.data.frame(sc), check.names = FALSE)
  colnames(plot.df)[2:(k + 1)] <- paste0("PCo", seq_len(k))
  
  present <- intersect(model.vars, names(dfm))
  for (v in present) {
    levs <- unique(as.character(metadata[[v]]))
    plot.df[[v]] <- factor(as.character(dfm[[v]]), levels = levs)
  }
  list(plot.df = plot.df, metric.df = metric.df, used.vars = present)
}

# ==== PCoA panel (scatter + marginals) with GLOBAL limit overrides ====
pcoa_panel <- function(plot.df, metric.df, model.vars, axes = c(1,2), label = NULL,
                       xlim_override = NULL, ylim_override = NULL, palette_name = "Batch") {
  mbecCols <- c("#9467bd","#BCBD22","#2CA02C","#E377C2","#1F77B4","#FF7F0E",
                "#AEC7E8","#FFBB78","#98DF8A","#D62728","#FF9896","#C5B0D5",
                "#8C564B","#C49C94","#F7B6D2","#7F7F7F","#C7C7C7","#DBDB8D",
                "#17BECF","#9EDAE5")
  var.color <- model.vars[1]
  xcol <- paste0("PCo", axes[1]); ycol <- paste0("PCo", axes[2])
  x.label <- paste0(xcol, ": ", metric.df$var.explained[axes[1]], "% expl.var")
  y.label <- paste0(ycol, ": ", metric.df$var.explained[axes[2]], "% expl.var")

  scores <- data.frame(
    AX1 = plot.df[[xcol]],
    AX2 = plot.df[[ycol]],
    batch = if (var.color %in% names(plot.df)) droplevels(plot.df[[var.color]]) else factor(1)
  )
  if (!is.factor(scores$batch)) scores$batch <- factor(scores$batch)
  eb <- ellipse_union_bounds(scores, "batch", level = 0.95, n = 240)
  xr <- safe_range(scores$AX1); yr <- safe_range(scores$AX2)
  xlim <- finite_range(xr, eb$x); ylim <- finite_range(yr, eb$y)
  pad_x <- safe_pad(xlim, 0.12); pad_y <- safe_pad(ylim, 0.12)
  xlim <- c(xlim[1] - pad_x, xlim[2] + pad_x)
  ylim <- c(ylim[1] - pad_y, ylim[2] + pad_y)
  if (!is.null(xlim_override)) xlim <- xlim_override
  if (!is.null(ylim_override)) ylim <- ylim_override

  pmar <- margin(10, 16, 10, 16)

  pMain <- ggplot(plot.df, aes(x = !!sym(xcol), y = !!sym(ycol), colour = !!sym(var.color))) +
    geom_point(shape = 16, size = 1.3, alpha = 0.85) +
    stat_ellipse(aes(group = !!sym(var.color)),
                 type = "norm", level = 0.95,
                 linewidth = 0.7, linetype = 1, show.legend = FALSE, na.rm = TRUE) +
    scale_color_manual(values = mbecCols, name = palette_name) +
    guides(colour = guide_legend(order = 1, nrow = 1, byrow = TRUE)) +
    labs(title = NULL) +
    scale_x_continuous(limits = xlim, expand = expansion(mult = c(0.02, 0.02))) +
    scale_y_continuous(limits = ylim, expand = expansion(mult = c(0.02, 0.02))) +
    xlab(x.label) + ylab(y.label) + theme_bw() +
    theme_plot_common() +
    theme(
      panel.background = element_blank(),
      legend.position = 'bottom',
      legend.direction = 'horizontal',
      legend.box = 'vertical',
      plot.margin = pmar
    )
  
  pTop <- ggplot(plot.df, aes(x = !!sym(xcol))) +
    geom_density(aes(fill = !!sym(var.color)),
                 linewidth = 0.3, alpha = 0.5, show.legend = FALSE) +
    ylab("Density") +
    scale_fill_manual(values = mbecCols, guide = "none") +
    scale_x_continuous(limits = xlim, expand = expansion(mult = c(0.02, 0.02))) +
    theme_bw() +
    theme_plot_common() +
    theme(
      panel.background = element_blank(),
      legend.position = 'none',
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.margin = pmar
    )
  
  pRight <- ggplot(plot.df, aes(y = !!sym(ycol))) +
    geom_density(
      aes(x = after_stat(density), fill = !!sym(var.color)),
      linewidth = 0.3, alpha = 0.5, orientation = "y", show.legend = FALSE
    ) +
    xlab(NULL) + ylab("Density") +
    scale_fill_manual(values = mbecCols, guide = "none") +
    scale_y_continuous(limits = ylim, expand = expansion(mult = c(0.02, 0.02))) +
    theme_bw() +
    theme_plot_common() +
    theme(
      panel.background = element_blank(),
      legend.position = "none",
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.margin = margin(10, 16, 10, 16)
    )
  
  design <- "
A#
CB
"
  assembled <- (pTop + pRight + pMain) +
    plot_layout(design = design, widths = c(3, 1), heights = c(1.6, 3.2))

  if (!is.null(label)) {
    assembled <- assembled + plot_annotation(
      title = label,
      theme = theme_plot_title()
    )
  }

  assembled
}

# ==== Params ====
batch_var  <- "batch_id"
model_vars <- c(batch_var)
axes_to_plot <- c(1, 2)
ncol_grid <- 2
if (!is.na(opt_fig_ncol) && opt_fig_ncol >= 1) {
  ncol_grid <- max(1, opt_fig_ncol)
}
SYMMETRIC_AXES <- FALSE  # set TRUE to force symmetry about 0 (optional)

# =========================
# Set 1: Aitchison (CLR)
# =========================
message(sprintf("PCoA (Aitchison on CLR): color=%s (shape: none)", batch_var))

frames_cache_clr <- list()
global_x_clr <- NULL; global_y_clr <- NULL

for (nm in names(file_list_clr)) {
  cat("Computing CLR frames:", nm, "\n")
  df <- read_csv(file_list_clr[[nm]], show_col_types = FALSE)
  fr <- compute_pcoa_frames_aitch(df, metadata, model.vars = model_vars, n_axes = max(axes_to_plot))
  frames_cache_clr[[nm]] <- fr
  
  xcol <- paste0("PCo", axes_to_plot[1]); ycol <- paste0("PCo", axes_to_plot[2])
  scores <- data.frame(
    AX1 = fr$plot.df[[xcol]],
    AX2 = fr$plot.df[[ycol]],
    batch = if (batch_var %in% names(fr$plot.df)) droplevels(fr$plot.df[[batch_var]]) else factor(1)
  )
  if (!is.factor(scores$batch)) scores$batch <- factor(scores$batch)
  xr <- safe_range(scores$AX1); yr <- safe_range(scores$AX2)
  eb <- ellipse_union_bounds(scores, "batch", level = 0.95, n = 240)
  
  global_x_clr <- if (is.null(global_x_clr)) finite_range(xr, eb$x) else finite_range(global_x_clr, xr, eb$x)
  global_y_clr <- if (is.null(global_y_clr)) finite_range(yr, eb$y) else finite_range(global_y_clr, yr, eb$y)
}

pad_x <- safe_pad(global_x_clr, 0.12); pad_y <- safe_pad(global_y_clr, 0.12)
xlim_global_clr <- c(global_x_clr[1] - pad_x, global_x_clr[2] + pad_x)
ylim_global_clr <- c(global_y_clr[1] - pad_y, global_y_clr[2] + pad_y)
if (isTRUE(SYMMETRIC_AXES)) {
  x_half <- max(abs(xlim_global_clr)); y_half <- max(abs(ylim_global_clr))
  xlim_global_clr <- c(-x_half, x_half); ylim_global_clr <- c(-y_half, y_half)
}

plots_clr <- lapply(names(file_list_clr), function(nm) {
  fr <- frames_cache_clr[[nm]]
  pcoa_panel(fr$plot.df, fr$metric.df, model_vars,
             axes = axes_to_plot, label = nm,
             xlim_override = xlim_global_clr, ylim_override = ylim_global_clr,
             palette_name = "Batch")
})
names(plots_clr) <- names(file_list_clr)

# ---- Combine & save (CLR) ----
n_panels_clr <- length(plots_clr)
panel_cols_clr <- 1L
panel_rows_clr <- 1L
if (n_panels_clr == 1L) {
  combined_clr <- plots_clr[[1]] +
    theme(
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.box       = "vertical",
      legend.title     = element_text(size = plot_legend_title_size, face = "bold"),
      legend.text      = element_text(size = plot_legend_text_size),
      plot.margin = margin(8, 14, 8, 14)
    )
  w_clr <- 9.5; h_clr <- 6
} else {
  panel_cols_clr <- min(ncol_grid, n_panels_clr)
  panel_rows_clr <- ceiling(n_panels_clr / ncol_grid)
  combined_clr <- wrap_plots(plots_clr, ncol = ncol_grid) +
    plot_layout(guides = "collect") &
    theme(
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.box       = "vertical",
      legend.title     = element_text(size = plot_legend_title_size, face = "bold"),
      legend.text      = element_text(size = plot_legend_text_size),
      plot.margin = margin(8, 14, 8, 14)
    )
  w_clr <- 9.5 * panel_cols_clr
  h_clr <- 6   * panel_rows_clr
}

combined_clr <- combined_clr + plot_annotation(theme = theme_plot_overall_title())
fig_dims_clr <- apply_fig_overrides(w_clr, h_clr, 300, panel_cols_clr, panel_rows_clr)
ggsave(file.path(output_folder, "pcoa_aitchison.png"),
       plot = combined_clr, width = fig_dims_clr$width, height = fig_dims_clr$height, dpi = fig_dims_clr$dpi)
ggsave(file.path(output_folder, "pcoa_aitchison.tif"),
       plot = combined_clr, width = fig_dims_clr$width, height = fig_dims_clr$height, dpi = fig_dims_clr$dpi, compression = "lzw")

# =========================
# Set 2: Bray–Curtis (TSS)
# =========================
message(sprintf("PCoA (Bray–Curtis on TSS): color=%s (shape: none)", batch_var))

frames_cache_tss <- list()
global_x_tss <- NULL; global_y_tss <- NULL

for (nm in names(file_list_tss)) {
  cat("Computing TSS/Bray frames:", nm, "\n")
  df <- read_csv(file_list_tss[[nm]], show_col_types = FALSE)
  fr <- compute_pcoa_frames_bray(df, metadata, model.vars = model_vars, n_axes = max(axes_to_plot))
  frames_cache_tss[[nm]] <- fr
  
  xcol <- paste0("PCo", axes_to_plot[1]); ycol <- paste0("PCo", axes_to_plot[2])
  scores <- data.frame(
    AX1 = fr$plot.df[[xcol]],
    AX2 = fr$plot.df[[ycol]],
    batch = if (batch_var %in% names(fr$plot.df)) droplevels(fr$plot.df[[batch_var]]) else factor(1)
  )
  if (!is.factor(scores$batch)) scores$batch <- factor(scores$batch)
  xr <- safe_range(scores$AX1); yr <- safe_range(scores$AX2)
  eb <- ellipse_union_bounds(scores, "batch", level = 0.95, n = 240)
  
  global_x_tss <- if (is.null(global_x_tss)) finite_range(xr, eb$x) else finite_range(global_x_tss, xr, eb$x)
  global_y_tss <- if (is.null(global_y_tss)) finite_range(yr, eb$y) else finite_range(global_y_tss, yr, eb$y)
}

pad_x <- safe_pad(global_x_tss, 0.12); pad_y <- safe_pad(global_y_tss, 0.12)
xlim_global_tss <- c(global_x_tss[1] - pad_x, global_x_tss[2] + pad_x)
ylim_global_tss <- c(global_y_tss[1] - pad_y, global_y_tss[2] + pad_y)
if (isTRUE(SYMMETRIC_AXES)) {
  x_half <- max(abs(xlim_global_tss)); y_half <- max(abs(ylim_global_tss))
  xlim_global_tss <- c(-x_half, x_half); ylim_global_tss <- c(-y_half, y_half)
}

plots_tss <- lapply(names(file_list_tss), function(nm) {
  fr <- frames_cache_tss[[nm]]
  pcoa_panel(fr$plot.df, fr$metric.df, model_vars,
             axes = axes_to_plot, label = nm,
             xlim_override = xlim_global_tss, ylim_override = ylim_global_tss,
             palette_name = "Batch")
})
names(plots_tss) <- names(file_list_tss)

# ---- Combine & save (TSS) ----
n_panels_tss <- length(plots_tss)
panel_cols_tss <- 1L
panel_rows_tss <- 1L
if (n_panels_tss == 1L) {
  combined_tss <- plots_tss[[1]] +
    theme(
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.box       = "vertical",
      legend.title     = element_text(size = plot_legend_title_size, face = "bold"),
      legend.text      = element_text(size = plot_legend_text_size),
      plot.margin = margin(8, 14, 8, 14)
    )
  w_tss <- 9.5; h_tss <- 6
} else {
  panel_cols_tss <- min(ncol_grid, n_panels_tss)
  panel_rows_tss <- ceiling(n_panels_tss / ncol_grid)
  combined_tss <- wrap_plots(plots_tss, ncol = ncol_grid) +
    plot_layout(guides = "collect") &
    theme(
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.box       = "vertical",
      legend.title     = element_text(size = plot_legend_title_size, face = "bold"),
      legend.text      = element_text(size = plot_legend_text_size),
      plot.margin = margin(8, 14, 8, 14)
    )
  w_tss <- 9.5 * panel_cols_tss
  h_tss <- 6   * panel_rows_tss
}

combined_tss <- combined_tss + plot_annotation(theme = theme_plot_overall_title())
fig_dims_tss <- apply_fig_overrides(w_tss, h_tss, 300, panel_cols_tss, panel_rows_tss)
ggsave(file.path(output_folder, "pcoa_braycurtis.png"),
       plot = combined_tss, width = fig_dims_tss$width, height = fig_dims_tss$height, dpi = fig_dims_tss$dpi)
ggsave(file.path(output_folder, "pcoa_braycurtis.tif"),
       plot = combined_tss, width = fig_dims_tss$width, height = fig_dims_tss$height, dpi = fig_dims_tss$dpi, compression = "lzw")

# =========================
# Unified PCoA ranking (Aitchison + Bray combined)
# =========================

compute_centroids_pcoa <- function(scores, batch_var = "batch_id") {
  scores %>%
    dplyr::group_by(!!rlang::sym(batch_var)) %>%
    dplyr::summarise(PCo1 = mean(PCo1), PCo2 = mean(PCo2), .groups = "drop")
}
compute_centroid_distances <- function(centroids) {
  if (nrow(centroids) < 2) return(NA_real_)
  as.numeric(mean(dist(centroids[, c("PCo1", "PCo2")], method = "euclidean")))
}
# Weighted mean within-batch pairwise distance on PCo1-PCo2
compute_within_dispersion_pcoa <- function(scores, batch_var = "batch_id") {
  if (!all(c("PCo1","PCo2", batch_var) %in% names(scores))) return(NA_real_)
  levs <- levels(scores[[batch_var]])
  if (is.null(levs)) levs <- unique(scores[[batch_var]])
  ws <- c(); ns <- c()
  for (lev in levs) {
    sub <- scores[scores[[batch_var]] == lev, c("PCo1","PCo2"), drop = FALSE]
    n <- nrow(sub)
    if (n < 2) next
    d <- stats::dist(sub, method = "euclidean")
    ws <- c(ws, mean(d))
    ns <- c(ns, n)
  }
  if (!length(ws)) return(NA_real_)
  stats::weighted.mean(ws, w = ns)
}
pcoa_metric_score <- function(batch_distance, coverage, within_dispersion = NA_real_) {
  if (is.na(batch_distance) || is.na(coverage)) return(NA_real_)
  coverage <- max(0, min(1, coverage))  # clamp [0,1]
  mix_term <- NA_real_
  if (!is.na(within_dispersion) && within_dispersion >= 0) {
    if (batch_distance == 0 && within_dispersion == 0) {
      mix_term <- 1
    } else if (!is.finite(batch_distance)) {
      mix_term <- NA_real_
    } else {
      mix_term <- within_dispersion / (within_dispersion + batch_distance)
    }
  } else if (!is.na(batch_distance)) {
    mix_term <- 1 / (1 + batch_distance)
  }
  if (is.na(mix_term)) return(NA_real_)
  coverage * mix_term  # higher = better
}

methods_clr <- names(frames_cache_clr)
methods_tss <- names(frames_cache_tss)
all_methods <- union(methods_clr, methods_tss)

only_baseline <- (length(all_methods) == 1L) && identical(all_methods, "Before correction")

if (only_baseline) {
  # ===== Baseline-only assessment (no ranking) =====
  assess_rows <- list()
  
  if ("Before correction" %in% methods_clr) {
    fr <- frames_cache_clr[["Before correction"]]
    md <- metadata[match(fr$plot.df$sample_id, metadata$sample_id), , drop = FALSE]
    scores <- fr$plot.df %>%
      dplyr::select(sample_id, PCo1, PCo2) %>%
      dplyr::mutate(batch_id = factor(md$batch_id))
    cents <- compute_centroids_pcoa(scores, "batch_id")
    D_between <- compute_centroid_distances(cents)
    W_within  <- compute_within_dispersion_pcoa(scores, "batch_id")
    ve <- fr$metric.df$var.explained
    coverage2 <- sum(ve[1:min(2, length(ve))], na.rm = TRUE) / 100
    score <- pcoa_metric_score(D_between, coverage2, W_within)
    needs_correction <- is.finite(D_between) && is.finite(W_within) && (D_between > W_within)
    
    assess_rows[["CLR"]] <- tibble::tibble(
      Method            = "Before correction",
      Geometry          = "Aitchison (CLR)",
      Batch_Distance    = D_between,
      Within_Dispersion = W_within,
      Coverage_PC1_PC2  = coverage2,
      Score             = score,
      Needs_Correction  = needs_correction
    )
  }
  
  if ("Before correction" %in% methods_tss) {
    fr <- frames_cache_tss[["Before correction"]]
    md <- metadata[match(fr$plot.df$sample_id, metadata$sample_id), , drop = FALSE]
    scores <- fr$plot.df %>%
      dplyr::select(sample_id, PCo1, PCo2) %>%
      dplyr::mutate(batch_id = factor(md$batch_id))
    cents <- compute_centroids_pcoa(scores, "batch_id")
    D_between <- compute_centroid_distances(cents)
    W_within  <- compute_within_dispersion_pcoa(scores, "batch_id")
    ve <- fr$metric.df$var.explained
    coverage2 <- sum(ve[1:min(2, length(ve))], na.rm = TRUE) / 100
    score <- pcoa_metric_score(D_between, coverage2, W_within)
    needs_correction <- is.finite(D_between) && is.finite(W_within) && (D_between > W_within)
    
    assess_rows[["TSS"]] <- tibble::tibble(
      Method            = "Before correction",
      Geometry          = "Bray–Curtis (TSS)",
      Batch_Distance    = D_between,
      Within_Dispersion = W_within,
      Coverage_PC1_PC2  = coverage2,
      Score             = score,
      Needs_Correction  = needs_correction
    )
  }
  
  assess_df <- dplyr::bind_rows(assess_rows)
  
  # Combined view (geometric mean of available scores; OR on correction flags)
  comb_score <- dplyr::case_when(
    nrow(assess_df) >= 2 && all(is.finite(assess_df$Score)) ~ sqrt(prod(assess_df$Score)),
    TRUE                                                    ~ max(assess_df$Score, na.rm = TRUE)
  )
  needs_corr_any <- any(assess_df$Needs_Correction, na.rm = TRUE)
  
  assess_df <- dplyr::bind_rows(
    assess_df,
    tibble::tibble(
      Method            = "Before correction",
      Geometry          = "Combined",
      Batch_Distance    = NA_real_,
      Within_Dispersion = NA_real_,
      Coverage_PC1_PC2  = NA_real_,
      Score             = comb_score,
      Needs_Correction  = needs_corr_any
    )
  )
  
  print(assess_df, n = nrow(assess_df))
  readr::write_csv(assess_df, file.path(output_folder, "pcoa_raw_assessment.csv"))
  
  # No correction recommendation messages
  
} else {
  # ===== Multi-method ranking (as before) =====
  rank_tbl <- dplyr::tibble(
    Method = character(),
    Batch_Distance_CLR = numeric(),
    Within_Dispersion_CLR = numeric(),
    Coverage_CLR       = numeric(),
    Score_CLR          = numeric(),
    Batch_Distance_TSS = numeric(),
    Within_Dispersion_TSS = numeric(),
    Coverage_TSS       = numeric(),
    Score_TSS          = numeric(),
    `Absolute score`   = numeric()
  )
  
  for (m in union(names(frames_cache_clr), names(frames_cache_tss))) {
    # ----- CLR -----
    D_a <- NA_real_; W_a <- NA_real_; Cov_a <- NA_real_; S_a <- NA_real_
    if (m %in% names(frames_cache_clr)) {
      fr_a <- frames_cache_clr[[m]]
      md_a <- metadata[match(fr_a$plot.df$sample_id, metadata$sample_id), , drop = FALSE]
      scores_a <- fr_a$plot.df %>%
        dplyr::select(sample_id, PCo1, PCo2) %>%
        dplyr::mutate(batch_id = factor(md_a$batch_id))
      cents_a <- compute_centroids_pcoa(scores_a, "batch_id")
      D_a <- compute_centroid_distances(cents_a)
      W_a <- compute_within_dispersion_pcoa(scores_a, "batch_id")
      ve_a <- fr_a$metric.df$var.explained
      Cov_a <- sum(ve_a[1:min(2, length(ve_a))], na.rm = TRUE) / 100
      S_a <- pcoa_metric_score(D_a, Cov_a, W_a)
    }

    # ----- TSS -----
    D_b <- NA_real_; W_b <- NA_real_; Cov_b <- NA_real_; S_b <- NA_real_
    if (m %in% names(frames_cache_tss)) {
      fr_b <- frames_cache_tss[[m]]
      md_b <- metadata[match(fr_b$plot.df$sample_id, metadata$sample_id), , drop = FALSE]
      scores_b <- fr_b$plot.df %>%
        dplyr::select(sample_id, PCo1, PCo2) %>%
        dplyr::mutate(batch_id = factor(md_b$batch_id))
      cents_b <- compute_centroids_pcoa(scores_b, "batch_id")
      D_b <- compute_centroid_distances(cents_b)
      W_b <- compute_within_dispersion_pcoa(scores_b, "batch_id")
      ve_b <- fr_b$metric.df$var.explained
      Cov_b <- sum(ve_b[1:min(2, length(ve_b))], na.rm = TRUE) / 100
      S_b <- pcoa_metric_score(D_b, Cov_b, W_b)
    }
    
    Combined <- dplyr::case_when(
      !is.na(S_a) && !is.na(S_b) ~ sqrt(S_a * S_b),
      !is.na(S_a)                ~ S_a,
      !is.na(S_b)                ~ S_b,
      TRUE                       ~ NA_real_
    )

    rank_tbl <- dplyr::bind_rows(rank_tbl, dplyr::tibble(
      Method = m,
      Batch_Distance_CLR = D_a,
      Within_Dispersion_CLR = W_a,
      Coverage_CLR       = Cov_a,
      Score_CLR          = S_a,
      Batch_Distance_TSS = D_b,
      Within_Dispersion_TSS = W_b,
      Coverage_TSS       = Cov_b,
      Score_TSS          = S_b,
      `Absolute score`   = Combined
    ))
  }

  baseline_abs <- rank_tbl$`Absolute score`[rank_tbl$Method == "Before correction"][1]
  rel_divisor <- if (length(baseline_abs) && is.finite(baseline_abs) && baseline_abs != 0) baseline_abs else NA_real_

  ranked_pcoa_unified <- rank_tbl %>%
    dplyr::mutate(
      `Relative score` = if (is.na(rel_divisor)) NA_real_ else `Absolute score` / rel_divisor
    ) %>%
    dplyr::arrange(dplyr::desc(`Absolute score`), Method) %>%
    dplyr::mutate(Rank = dplyr::row_number()) %>%
    dplyr::relocate(`Absolute score`, .after = Method) %>%
    dplyr::relocate(`Relative score`, .after = `Absolute score`) %>%
    dplyr::relocate(Rank, .after = `Relative score`)

  print(ranked_pcoa_unified, n = nrow(ranked_pcoa_unified))
  readr::write_csv(ranked_pcoa_unified, file.path(output_folder, "pcoa_ranking.csv"))
}
