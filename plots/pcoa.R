# ==== Libraries ====
suppressPackageStartupMessages({
  library(ggplot2)
  library(readr)
  library(dplyr)
  library(patchwork)  # layouts + legend collection
  library(rlang)
  library(vegan)      # Bray-Curtis
  library(jsonlite)
})
# Map method codes to short labels for figures
method_short_label <- function(x) {
  map <- c(
    qn = "Quantile Normalization", bmc = "BMC", limma = "Limma", conqur = "ConQuR",
    plsda = "PLSDA-batch", combat = "ComBat", fsqn = "FSQN", mmuphin = "MMUPHin",
    ruv = "RUV-III-NB", metadict = "MetaDICT", pn = "Percentile Normalization",
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
    stop("No label column available for PCoA plots.")
  }
}

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

has_dual_geometries <- length(file_list_clr) > 0 && length(file_list_tss) > 0

# ==== PCoA frames (Aitchison on CLR) ====
compute_pcoa_frames_aitch <- function(df, metadata, model.vars = c("batch_id", label_col),
                                      n_axes = 5) {
  if (!"sample_id" %in% names(df)) {
    if (nrow(df) == nrow(metadata)) df$sample_id <- metadata$sample_id
    else stop("Input lacks 'sample_id' and row count != metadata; can't align samples.")
  }
  df  <- df %>% mutate(sample_id = as.character(sample_id))
  dfm <- inner_join(df, metadata, by = "sample_id")
  feat_cols <- setdiff(names(df), "sample_id")
  numeric_df <- dfm %>% select(all_of(feat_cols)) %>% select(where(is.numeric))
  n_features_total <- as.integer(ncol(numeric_df))
  if (is.null(n_features_total) || length(n_features_total) == 0) n_features_total <- 0L
  keep <- if (n_features_total) {
    vapply(numeric_df, function(x) sd(x, na.rm = TRUE) > 0, logical(1))
  } else {
    logical(0)
  }
  X <- as.matrix(numeric_df[, keep, drop = FALSE])
  if (!ncol(X)) stop("No variable numeric features remain for PCoA (CLR).")
  n_features_used <- as.integer(ncol(X))
  
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
  list(
    plot.df = plot.df,
    metric.df = metric.df,
    used.vars = present,
    n_features_total = n_features_total,
    n_features_used = n_features_used
  )
}

# ==== PCoA frames (Bray–Curtis on TSS) ====
compute_pcoa_frames_bray <- function(df, metadata, model.vars = c("batch_id", label_col),
                                     n_axes = 5) {
  if (!"sample_id" %in% names(df)) {
    if (nrow(df) == nrow(metadata)) df$sample_id <- metadata$sample_id
    else stop("Input lacks 'sample_id' and row count != metadata; can't align samples.")
  }
  df  <- df %>% mutate(sample_id = as.character(sample_id))
  dfm <- inner_join(df, metadata, by = "sample_id")
  feat_cols <- setdiff(names(df), "sample_id")
  numeric_df <- dfm %>% select(all_of(feat_cols)) %>% select(where(is.numeric))
  n_features_total <- as.integer(ncol(numeric_df))
  if (is.null(n_features_total) || length(n_features_total) == 0) n_features_total <- 0L
  X <- as.matrix(numeric_df)
  
  # Ensure non-negative & close rows to TSS (proportions)
  X[!is.finite(X)] <- 0
  X[X < 0] <- 0
  Xtss <- safe_closure(X)
  n_features_used <- as.integer(ncol(Xtss))
  
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
  list(
    plot.df = plot.df,
    metric.df = metric.df,
    used.vars = present,
    n_features_total = n_features_total,
    n_features_used = n_features_used
  )
}

# ==== PCoA panel (scatter + marginals) with per-panel limits ====
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

# ==== Params ====
batch_var  <- "batch_id"
model_vars <- c(batch_var)
axes_to_plot <- c(1, 2)
ncol_grid <- 3
if (!is.na(opt_fig_ncol) && opt_fig_ncol >= 1) {
  ncol_grid <- max(1, opt_fig_ncol)
}
SYMMETRIC_AXES <- FALSE  # set TRUE to force symmetry about 0 (optional)

# =========================
# Set 1: Aitchison (CLR)
# =========================
message(sprintf("PCoA (Aitchison on CLR): color=%s (shape: none)", batch_var))

frames_cache_clr <- list()

for (nm in names(file_list_clr)) {
  cat("Computing CLR frames:", nm, "\n")
  df <- read_csv(file_list_clr[[nm]], show_col_types = FALSE)
  fr <- compute_pcoa_frames_aitch(df, metadata, model.vars = model_vars, n_axes = max(axes_to_plot))
  frames_cache_clr[[nm]] <- fr
}

plots_clr <- lapply(names(file_list_clr), function(nm) {
  fr <- frames_cache_clr[[nm]]
  label_nm <- if (has_dual_geometries) sprintf("%s - Aitchison", nm) else nm
  x_override <- NULL
  y_override <- NULL
  if (isTRUE(SYMMETRIC_AXES)) {
    xcol <- paste0("PCo", axes_to_plot[1])
    ycol <- paste0("PCo", axes_to_plot[2])
    scores <- data.frame(
      AX1 = fr$plot.df[[xcol]],
      AX2 = fr$plot.df[[ycol]],
      batch = if (batch_var %in% names(fr$plot.df)) droplevels(fr$plot.df[[batch_var]]) else factor(1)
    )
    if (!is.factor(scores$batch)) scores$batch <- factor(scores$batch)
    eb <- ellipse_union_bounds(scores, "batch", level = 0.95, n = 240)
    xr <- finite_range(scores$AX1, eb$x)
    yr <- finite_range(scores$AX2, eb$y)
    half <- max(abs(c(xr, yr)))
    x_override <- c(-half, half)
    y_override <- c(-half, half)
  }
  pcoa_panel(fr$plot.df, fr$metric.df, model_vars,
             axes = axes_to_plot, label = label_nm,
             xlim_override = x_override, ylim_override = y_override,
             palette_name = "Batch")
})
names(plots_clr) <- names(file_list_clr)

# ---- Combine & save (CLR) ----
n_panels_clr <- length(plots_clr)
panel_cols_clr <- 1L
panel_rows_clr <- 1L
base_fig_width_in  <- 2800 / 300
base_fig_height_in <- 1800 / 300
base_col_width_in  <- base_fig_width_in / 3
base_row_height_in <- base_fig_height_in
if (n_panels_clr == 1L) {
  combined_clr <- plots_clr[[1]] +
    theme(
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.box       = "vertical",
      plot.margin = margin(8, 14, 8, 14)
    )
  w_clr <- base_fig_width_in; h_clr <- base_fig_height_in
} else {
  panel_cols_clr <- min(ncol_grid, n_panels_clr)
  panel_rows_clr <- ceiling(n_panels_clr / ncol_grid)
  combined_clr <- wrap_plots(plots_clr, ncol = ncol_grid) +
    plot_layout(guides = "collect") &
    theme(
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.box       = "vertical",
      plot.margin = margin(8, 14, 8, 14)
    )
  w_clr <- base_col_width_in * panel_cols_clr
  h_clr <- base_row_height_in * panel_rows_clr
}
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

for (nm in names(file_list_tss)) {
  cat("Computing TSS/Bray frames:", nm, "\n")
  df <- read_csv(file_list_tss[[nm]], show_col_types = FALSE)
  fr <- compute_pcoa_frames_bray(df, metadata, model.vars = model_vars, n_axes = max(axes_to_plot))
  frames_cache_tss[[nm]] <- fr
}

plots_tss <- lapply(names(file_list_tss), function(nm) {
  fr <- frames_cache_tss[[nm]]
  label_nm <- if (has_dual_geometries) sprintf("%s - Bray-Curtis", nm) else nm
  x_override <- NULL
  y_override <- NULL
  if (isTRUE(SYMMETRIC_AXES)) {
    xcol <- paste0("PCo", axes_to_plot[1])
    ycol <- paste0("PCo", axes_to_plot[2])
    scores <- data.frame(
      AX1 = fr$plot.df[[xcol]],
      AX2 = fr$plot.df[[ycol]],
      batch = if (batch_var %in% names(fr$plot.df)) droplevels(fr$plot.df[[batch_var]]) else factor(1)
    )
    if (!is.factor(scores$batch)) scores$batch <- factor(scores$batch)
    eb <- ellipse_union_bounds(scores, "batch", level = 0.95, n = 240)
    xr <- finite_range(scores$AX1, eb$x)
    yr <- finite_range(scores$AX2, eb$y)
    half <- max(abs(c(xr, yr)))
    x_override <- c(-half, half)
    y_override <- c(-half, half)
  }
  pcoa_panel(fr$plot.df, fr$metric.df, model_vars,
             axes = axes_to_plot, label = label_nm,
             xlim_override = x_override, ylim_override = y_override,
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
      plot.margin = margin(8, 14, 8, 14)
    )
  w_tss <- base_fig_width_in; h_tss <- base_fig_height_in
} else {
  panel_cols_tss <- min(ncol_grid, n_panels_tss)
  panel_rows_tss <- ceiling(n_panels_tss / ncol_grid)
  combined_tss <- wrap_plots(plots_tss, ncol = ncol_grid) +
    plot_layout(guides = "collect") &
    theme(
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.box       = "vertical",
      plot.margin = margin(8, 14, 8, 14)
    )
  w_tss <- base_col_width_in * panel_cols_tss
  h_tss <- base_row_height_in * panel_rows_tss
}
fig_dims_tss <- apply_fig_overrides(w_tss, h_tss, 300, panel_cols_tss, panel_rows_tss)
ggsave(file.path(output_folder, "pcoa_braycurtis.png"),
       plot = combined_tss, width = fig_dims_tss$width, height = fig_dims_tss$height, dpi = fig_dims_tss$dpi)
ggsave(file.path(output_folder, "pcoa_braycurtis.tif"),
       plot = combined_tss, width = fig_dims_tss$width, height = fig_dims_tss$height, dpi = fig_dims_tss$dpi, compression = "lzw")

# =========================
# Unified PCoA summaries (Aitchison + Bray combined)
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

prepare_batch_scores <- function(plot_df, metadata, axes = c("PCo1", "PCo2"), batch_var = "batch_id") {
  if (!all(c("sample_id", axes) %in% names(plot_df))) return(NULL)
  if (!batch_var %in% names(metadata)) return(NULL)
  idx <- match(plot_df$sample_id, metadata$sample_id)
  batch <- metadata[[batch_var]][idx]
  coords <- as.matrix(plot_df[, axes, drop = FALSE])
  if (!nrow(coords) || ncol(coords) < 2) return(NULL)
  valid <- apply(coords, 1, function(row) all(is.finite(row))) & !is.na(batch)
  if (!any(valid)) return(NULL)
  coords <- coords[valid, , drop = FALSE]
  batch <- droplevels(factor(batch[valid]))
  if (!nrow(coords) || !nlevels(batch)) return(NULL)
  list(coords = coords, batch = batch)
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

mean_within_radius <- function(coords, groups) {
  if (is.null(coords) || !length(coords) || !nrow(coords)) return(NA_real_)
  if (ncol(coords) < 2) return(NA_real_)
  groups <- droplevels(factor(groups))
  levs <- levels(groups)
  total <- 0
  count <- 0
  for (lev in levs) {
    idx <- which(groups == lev)
    if (!length(idx)) next
    sub <- coords[idx, , drop = FALSE]
    sub <- sub[rowSums(is.finite(sub)) == ncol(coords), , drop = FALSE]
    if (!nrow(sub)) next
    centroid <- colMeans(sub, na.rm = TRUE)
    dists <- sqrt(rowSums((sub - matrix(centroid, nrow = nrow(sub), ncol = ncol(sub), byrow = TRUE))^2))
    dists <- dists[is.finite(dists)]
    total <- total + sum(dists)
    count <- count + length(dists)
  }
  if (!count) return(NA_real_)
  total / count
}

compute_knn_mixing <- function(coords, groups, k = 10L) {
  if (is.null(coords) || !length(coords) || !nrow(coords)) return(NA_real_)
  if (ncol(coords) < 2) return(NA_real_)
  n <- nrow(coords)
  if (n <= 1) return(NA_real_)
  k <- min(as.integer(k), n - 1L)
  if (k <= 0) return(NA_real_)
  groups <- droplevels(factor(groups))
  if (!nlevels(groups)) return(NA_real_)
  dmat <- as.matrix(dist(coords, method = "euclidean", diag = TRUE, upper = TRUE))
  diag(dmat) <- Inf
  mixing <- numeric(n)
  valid_rows <- rep(FALSE, n)
  group_vals <- as.character(groups)
  for (i in seq_len(n)) {
    row <- dmat[i, ]
    if (all(!is.finite(row))) next
    ord <- order(row, na.last = NA)
    ord <- ord[ord != i]
    if (!length(ord)) next
    take <- ord[seq_len(min(k, length(ord)))]
    if (!length(take)) next
    neigh <- group_vals[take]
    if (!length(neigh)) next
    mixing[i] <- mean(neigh != group_vals[i])
    valid_rows[i] <- TRUE
  }
  if (!any(valid_rows)) return(NA_real_)
  mean(mixing[valid_rows])
}

summarise_pcoa_method <- function(fr, method_name, geometry_label, metadata,
                                   axes = c("PCo1", "PCo2"), batch_var = "batch_id") {
  prep <- prepare_batch_scores(fr$plot.df, metadata, axes = axes, batch_var = batch_var)
  if (is.null(prep)) return(NULL)
  coords <- prep$coords
  batch <- prep$batch
  var_vals <- fr$metric.df$var.explained
  top_two <- min(2L, length(var_vals))
  var12 <- if (top_two) sum(var_vals[seq_len(top_two)], na.rm = TRUE) else NA_real_
  tibble::tibble(
    Method = method_name,
    Geometry = geometry_label,
    Var12_PCoA = var12,
    Batch_Distance_PCoA = mean_centroid_distance(coords, batch),
    R_within_PCoA = mean_within_radius(coords, batch),
    Mixing_k10_PCoA = compute_knn_mixing(coords, batch, k = 10L),
    n_features_used_PCoA = if (is.null(fr$n_features_used)) NA_integer_ else fr$n_features_used,
    n_features_total_PCoA = if (is.null(fr$n_features_total)) NA_integer_ else fr$n_features_total
  )
}

methods_clr <- names(frames_cache_clr)
methods_tss <- names(frames_cache_tss)
all_methods <- union(methods_clr, methods_tss)

only_baseline <- (length(all_methods) == 1L) && identical(all_methods, "Before correction")
output_name <- if (only_baseline) "pcoa_raw_assessment_pre.csv" else "pcoa_raw_assessment_post.csv"

if (only_baseline) {
  # ===== Baseline-only assessment (no ranking) =====
  assess_rows <- list()

  if ("Before correction" %in% methods_clr) {
    fr <- frames_cache_clr[["Before correction"]]
    row <- summarise_pcoa_method(fr, "Before correction", "Ait", metadata,
                                 axes = c("PCo1", "PCo2"), batch_var = batch_var)
    if (!is.null(row)) assess_rows[[length(assess_rows) + 1L]] <- row
  }

  if ("Before correction" %in% methods_tss) {
    fr <- frames_cache_tss[["Before correction"]]
    row <- summarise_pcoa_method(fr, "Before correction", "BC", metadata,
                                 axes = c("PCo1", "PCo2"), batch_var = batch_var)
    if (!is.null(row)) assess_rows[[length(assess_rows) + 1L]] <- row
  }

  assess_df <- if (length(assess_rows)) dplyr::bind_rows(assess_rows) else tibble::tibble()

  print(assess_df, n = nrow(assess_df))
  readr::write_csv(assess_df, file.path(output_folder, output_name))

  # No correction recommendation messages

} else {
  # ===== Multi-method assessment without ranking =====
  methods_all <- union(names(frames_cache_clr), names(frames_cache_tss))
  rows <- list()
  for (m in methods_all) {
    if (m %in% names(frames_cache_clr)) {
      fr_a <- frames_cache_clr[[m]]
      row_a <- summarise_pcoa_method(fr_a, m, "Ait", metadata,
                                     axes = c("PCo1", "PCo2"), batch_var = batch_var)
      if (!is.null(row_a)) rows[[length(rows) + 1L]] <- row_a
    }
    if (m %in% names(frames_cache_tss)) {
      fr_b <- frames_cache_tss[[m]]
      row_b <- summarise_pcoa_method(fr_b, m, "BC", metadata,
                                     axes = c("PCo1", "PCo2"), batch_var = batch_var)
      if (!is.null(row_b)) rows[[length(rows) + 1L]] <- row_b
    }
  }

  assessment_tbl <- if (length(rows)) {
    dplyr::bind_rows(rows) %>% dplyr::arrange(Geometry, Method)
  } else {
    tibble::tibble()
  }

  print(assessment_tbl, n = nrow(assessment_tbl))
  readr::write_csv(assessment_tbl, file.path(output_folder, output_name))
}
