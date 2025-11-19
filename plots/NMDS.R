# =========================
# NMDS figures (Aitchison on CLR; Bray-Curtis on TSS)
# =========================

# ==== Libraries ====
suppressPackageStartupMessages({
  library(ggplot2)
  library(readr)
  library(dplyr)
  library(patchwork)   # layouts + legend collection

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

  library(rlang)
  library(vegan)       # distances + NMDS
  library(jsonlite)
})

# ==== Helpers (robust ranges/padding) ====
safe_range <- function(v) { v <- v[is.finite(v)]; if (!length(v)) c(0,0) else range(v, na.rm = TRUE) }
finite_range <- function(...) { v <- unlist(list(...)); v <- v[is.finite(v)]; if (!length(v)) c(0,0) else range(v, na.rm = TRUE) }
safe_pad <- function(r, frac = 0.06) { dx <- diff(r); if (!is.finite(dx) || dx <= 0) 1e-6 else dx * frac }

# ==== Ellipse union bounds (falls back to point ranges if needed) ====
ellipse_union_bounds <- function(df_scores, group_var, level = 0.95, n = 240) {
  if (!nrow(df_scores)) return(list(x = c(0,0), y = c(0,0)))
  chi <- sqrt(qchisq(level, df = 2))
  xmins <- c(); xmaxs <- c(); ymins <- c(); ymaxs <- c()
  levs <- levels(df_scores[[group_var]]); if (is.null(levs)) levs <- unique(df_scores[[group_var]])
  for (lev in levs) {
    sub <- df_scores[df_scores[[group_var]] == lev, c("AX1","AX2"), drop = FALSE]
    if (nrow(sub) < 3 || any(!is.finite(as.matrix(sub)))) next
    S  <- tryCatch(stats::cov(sub, use = "complete.obs"), error = function(e) NULL)
    mu <- colMeans(sub, na.rm = TRUE)
    if (is.null(S) || any(!is.finite(S))) next
    eg <- eigen(S, symmetric = TRUE); if (any(!is.finite(eg$values))) next
    t  <- seq(0, 2*pi, length.out = n)
    R  <- eg$vectors %*% diag(sqrt(pmax(eg$values, 0)))
    pts <- t(chi * R %*% rbind(cos(t), sin(t))); pts <- sweep(pts, 2, mu, "+")
    xmins <- c(xmins, min(pts[,1], na.rm = TRUE)); xmaxs <- c(xmaxs, max(pts[,1], na.rm = TRUE))
    ymins <- c(ymins, min(pts[,2], na.rm = TRUE)); ymaxs <- c(ymaxs, max(pts[,2], na.rm = TRUE))
  }
  if (!length(xmins) || !length(ymins)) {
    return(list(x = safe_range(df_scores$AX1), y = safe_range(df_scores$AX2)))
  }
  list(x = c(min(xmins, na.rm = TRUE), max(xmaxs, na.rm = TRUE)),
       y = c(min(ymins, na.rm = TRUE), max(ymaxs, na.rm = TRUE)))
}

# ==== Compositional helpers ====
safe_closure <- function(X) {
  X[!is.finite(X)] <- 0; X[X < 0] <- 0
  rs <- rowSums(X); rs[!is.finite(rs) | rs == 0] <- 1
  sweep(X, 1, rs, "/")
}
clr_transform <- function(X) {
  Xt <- safe_closure(X)
  # replace zeros with a small replacement then renormalize
  for (i in seq_len(nrow(Xt))) {
    xi <- Xt[i, ]; pos <- xi > 0
    if (!any(pos)) { xi[] <- 1/length(xi); pos <- xi > 0 }
    if (any(!pos)) {
      eps <- min(xi[pos]) * 0.5
      xi[!pos] <- eps; xi <- xi / sum(xi)
    }
    Xt[i, ] <- xi
  }
  L <- log(Xt); sweep(L, 1, rowMeans(L), "-")
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
    stop("No label column available for NMDS plots.")
  }
}

if (!("batch" %in% names(metadata))) {
  metadata$batch <- NA
}

target_var <- label_col

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
file_list_clr <- setNames(clr_paths, method_short_label(name_from(clr_paths, "clr")))
file_list_tss <- setNames(tss_paths, method_short_label(name_from(tss_paths, "tss")))
has_dual_geometries <- length(file_list_clr) > 0 && length(file_list_tss) > 0

# Include raw_clr.csv / raw_tss.csv as "Before correction" if present
raw_clr_fp <- file.path(output_folder, "raw_clr.csv")
raw_tss_fp <- file.path(output_folder, "raw_tss.csv")
if (file.exists(raw_clr_fp)) file_list_clr <- c("Before correction" = raw_clr_fp, file_list_clr)
if (file.exists(raw_tss_fp)) file_list_tss <- c("Before correction" = raw_tss_fp, file_list_tss)

if (!length(file_list_clr) && !length(file_list_tss)) {
  stop("No normalized files found (expected raw_clr.csv/raw_tss.csv and/or normalized_*_clr.csv / normalized_*_tss.csv) in ", output_folder)
}

# ==== NMDS frames: Aitchison on CLR ====
compute_nmds_frames_aitch <- function(df, metadata, model.vars = c("batch", label_col), k = 2) {
  if (!"sample_id" %in% names(df)) {
    if (nrow(df) == nrow(metadata)) df$sample_id <- metadata$sample_id
    else stop("Input lacks 'sample_id' and row count != metadata.")
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
  if (!ncol(X)) stop("No numeric features for NMDS (CLR).")
  n_features_used <- as.integer(ncol(X))
  # If negatives exist, assume CLR; else build CLR from non-negative data
  Xclr <- if (any(X < 0, na.rm = TRUE)) {
    sweep(X, 1, rowMeans(X, na.rm = TRUE), "-")
  } else {
    clr_transform(X)
  }
  Da <- dist(Xclr, method = "euclidean")
  
  set.seed(123)
  fit <- vegan::monoMDS(Da, k = k, maxit = 500, smin = 1e-12, trace = FALSE)
  sc  <- fit$points; colnames(sc) <- paste0("NMDS", seq_len(ncol(sc)))
  plot.df <- data.frame(sample_id = dfm$sample_id, as.data.frame(sc), check.names = FALSE)
  present <- intersect(model.vars, names(dfm))
  for (v in present) plot.df[[v]] <- factor(as.character(dfm[[v]]), levels = unique(as.character(metadata[[v]])))
  ord_dist <- stats::dist(sc)
  shepard_r2 <- tryCatch({
    orig <- as.numeric(Da)
    fitted <- as.numeric(ord_dist)
    valid <- is.finite(orig) & is.finite(fitted)
    if (!any(valid)) NA_real_ else stats::cor(orig[valid], fitted[valid], method = "pearson")^2
  }, error = function(e) NA_real_)
  site_gof_vals <- tryCatch(vegan::goodness(fit), error = function(e) rep(NA_real_, nrow(sc)))
  site_gof_vals <- as.numeric(site_gof_vals)
  site_gof_median <- suppressWarnings(stats::median(site_gof_vals, na.rm = TRUE))
  if (!is.finite(site_gof_median)) site_gof_median <- NA_real_
  list(
    plot.df = plot.df,
    stress = fit$stress,
    used.vars = present,
    n_features_total = n_features_total,
    n_features_used = n_features_used,
    shepard_r2 = shepard_r2,
    site_gof_median = site_gof_median
  )
}

# ==== NMDS frames: Bray-Curtis on TSS ====
compute_nmds_frames_bray <- function(df, metadata, model.vars = c("batch", label_col), k = 2) {
  if (!"sample_id" %in% names(df)) {
    if (nrow(df) == nrow(metadata)) df$sample_id <- metadata$sample_id
    else stop("Input lacks 'sample_id' and row count != metadata.")
  }
  df  <- df %>% mutate(sample_id = as.character(sample_id))
  dfm <- inner_join(df, metadata, by = "sample_id")
  
  feat_cols <- setdiff(names(df), "sample_id")
  numeric_df <- dfm %>% select(all_of(feat_cols)) %>% select(where(is.numeric))
  n_features_total <- as.integer(ncol(numeric_df))
  if (is.null(n_features_total) || length(n_features_total) == 0) n_features_total <- 0L
  X <- as.matrix(numeric_df)
  Xtss <- safe_closure(X)
  n_features_used <- as.integer(ncol(Xtss))
  Db <- vegan::vegdist(Xtss, method = "bray")
  
  set.seed(123)
  fit <- vegan::monoMDS(Db, k = k, maxit = 500, smin = 1e-12, trace = FALSE)
  sc  <- fit$points; colnames(sc) <- paste0("NMDS", seq_len(ncol(sc)))
  plot.df <- data.frame(sample_id = dfm$sample_id, as.data.frame(sc), check.names = FALSE)
  present <- intersect(model.vars, names(dfm))
  for (v in present) plot.df[[v]] <- factor(as.character(dfm[[v]]), levels = unique(as.character(metadata[[v]])))
  ord_dist <- stats::dist(sc)
  shepard_r2 <- tryCatch({
    orig <- as.numeric(Db)
    fitted <- as.numeric(ord_dist)
    valid <- is.finite(orig) & is.finite(fitted)
    if (!any(valid)) NA_real_ else stats::cor(orig[valid], fitted[valid], method = "pearson")^2
  }, error = function(e) NA_real_)
  site_gof_vals <- tryCatch(vegan::goodness(fit), error = function(e) rep(NA_real_, nrow(sc)))
  site_gof_vals <- as.numeric(site_gof_vals)
  site_gof_median <- suppressWarnings(stats::median(site_gof_vals, na.rm = TRUE))
  if (!is.finite(site_gof_median)) site_gof_median <- NA_real_
  list(
    plot.df = plot.df,
    stress = fit$stress,
    used.vars = present,
    n_features_total = n_features_total,
    n_features_used = n_features_used,
    shepard_r2 = shepard_r2,
    site_gof_median = site_gof_median
  )
}

# ==== NMDS panel (scatter with ellipses) with per-panel limits ====
nmds_panel <- function(plot.df, model.vars, axes = c(1,2),
                       label = NULL, xlim_override = NULL, ylim_override = NULL,
                       palette_name = "Batch") {
  mbecCols <- c("#9467bd","#BCBD22","#2CA02C","#E377C2","#1F77B4","#FF7F0E",
                "#AEC7E8","#FFBB78","#98DF8A","#D62728","#FF9896","#C5B0D5",
                "#8C564B","#C49C94","#F7B6D2","#7F7F7F","#C7C7C7","#DBDB8D",
                "#17BECF","#9EDAE5")
  var.color <- model.vars[1]
  xcol <- paste0("NMDS", axes[1]); ycol <- paste0("NMDS", axes[2])
  
  # per-panel: ranges via 95% ellipses
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
  
  title_text <- if (is.null(label)) "NMDS" else label

  p <- ggplot(plot.df, aes(x = !!sym(xcol), y = !!sym(ycol), colour = !!sym(var.color))) +
    geom_point(shape = 16, size = 1.3, alpha = 0.85) +
    stat_ellipse(aes(group = !!sym(var.color)),
                 type = "norm", level = 0.95,
                 linewidth = 0.7, linetype = 1, show.legend = FALSE, na.rm = TRUE) +
    scale_color_manual(values = mbecCols, name = palette_name) +
    guides(colour = guide_legend(order = 1, nrow = 1, byrow = TRUE)) +
    labs(title = title_text,
         x = paste0("NMDS", axes[1]), y = paste0("NMDS", axes[2])) +
    scale_x_continuous(limits = xlim, expand = expansion(mult = c(0.02, 0.02))) +
    scale_y_continuous(limits = ylim, expand = expansion(mult = c(0.02, 0.02))) +
    theme_bw() +
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
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold")
    )
  p
}

# ==== Params ====
batch_var  <- "batch"
axes_to_plot <- c(1, 2)
ncol_grid <- 3
if (!is.na(opt_fig_ncol) && opt_fig_ncol >= 1) {
  ncol_grid <- max(1, opt_fig_ncol)
}
SYMMETRIC_AXES <- FALSE  # set TRUE to force symmetry about 0 (optional)
model_vars_common <- unique(Filter(function(x) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) return(FALSE)
  nzchar(as.character(x))
}, c(batch_var, target_var)))
if (!length(model_vars_common)) model_vars_common <- c(batch_var)

# =========================
# Set 1: NMDS - Aitchison (CLR)
# =========================
message("NMDS (Aitchison on CLR)")

frames_cache_clr <- list()
for (nm in names(file_list_clr)) {
  cat("Computing CLR NMDS:", nm, "\n")
  df <- read_csv(file_list_clr[[nm]], show_col_types = FALSE)
  fr <- compute_nmds_frames_aitch(df, metadata, model.vars = model_vars_common, k = max(axes_to_plot))
  frames_cache_clr[[nm]] <- fr
}

message("NMDS (Bray-Curtis on TSS)")
frames_cache_tss <- list()
for (nm in names(file_list_tss)) {
  cat("Computing TSS NMDS:", nm, "\n")
  df <- read_csv(file_list_tss[[nm]], show_col_types = FALSE)
  fr <- compute_nmds_frames_bray(df, metadata, model.vars = model_vars_common, k = max(axes_to_plot))
  frames_cache_tss[[nm]] <- fr
}

build_nmds_plot_list <- function(frames_cache, geometry_label, color_var, palette_label) {
  if (!length(frames_cache) || is.null(color_var) || !nzchar(color_var)) return(list())
  plots <- lapply(names(frames_cache), function(nm) {
    fr <- frames_cache[[nm]]
    if (is.null(fr) || !nrow(fr$plot.df)) return(NULL)
    if (!(color_var %in% names(fr$plot.df))) return(NULL)
    label_parts <- c(nm)
    if (nzchar(geometry_label)) label_parts <- c(label_parts, geometry_label)
    grouping_label <- if (identical(palette_label, "Batch")) "Batch" else "Target"
    label_parts <- c(label_parts, grouping_label)
    label_nm <- paste(label_parts, collapse = " - ")
    x_override <- NULL
    y_override <- NULL
    if (isTRUE(SYMMETRIC_AXES)) {
      xcol <- paste0("NMDS", axes_to_plot[1])
      ycol <- paste0("NMDS", axes_to_plot[2])
      scores <- data.frame(
        AX1 = fr$plot.df[[xcol]],
        AX2 = fr$plot.df[[ycol]],
        batch = if (color_var %in% names(fr$plot.df)) droplevels(fr$plot.df[[color_var]]) else factor(1)
      )
      if (!is.factor(scores$batch)) scores$batch <- factor(scores$batch)
      eb <- ellipse_union_bounds(scores, "batch", level = 0.95, n = 240)
      xr <- finite_range(scores$AX1, eb$x)
      yr <- finite_range(scores$AX2, eb$y)
      half <- max(abs(c(xr, yr)))
      x_override <- c(-half, half)
      y_override <- c(-half, half)
    }
    nmds_panel(fr$plot.df, c(color_var),
               axes = axes_to_plot, label = label_nm,
               xlim_override = x_override, ylim_override = y_override,
               palette_name = palette_label)
  })
  Filter(function(x) !is.null(x), plots)
}

save_nmds_plot_set <- function(plot_list, filename_stub) {
  plot_list <- Filter(function(x) !is.null(x), plot_list)
  if (!length(plot_list)) return(invisible(NULL))
  n_panels <- length(plot_list)
  panel_cols <- 1L
  panel_rows <- 1L
  base_fig_width_in  <- 2800 / 300
  base_fig_height_in <- 1800 / 300
  base_col_width_in  <- base_fig_width_in / 3
  base_row_height_in <- base_fig_height_in
  if (n_panels == 1L) {
    combined <- plot_list[[1]] +
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "vertical",
        plot.margin = margin(8, 14, 8, 14)
      )
    w <- base_fig_width_in; h <- base_fig_height_in
  } else {
    panel_cols <- min(ncol_grid, n_panels)
    panel_rows <- ceiling(n_panels / ncol_grid)
    combined <- wrap_plots(plot_list, ncol = ncol_grid) +
      plot_layout(guides = "collect") &
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "vertical",
        plot.margin = margin(8, 14, 8, 14)
      )
    w <- base_col_width_in * panel_cols
    h <- base_row_height_in * panel_rows
  }
  fig_dims <- apply_fig_overrides(w, h, 300, panel_cols, panel_rows)
  ggsave(file.path(output_folder, paste0(filename_stub, ".png")),
         plot = combined, width = fig_dims$width, height = fig_dims$height, dpi = fig_dims$dpi)
  ggsave(file.path(output_folder, paste0(filename_stub, ".tif")),
         plot = combined, width = fig_dims$width, height = fig_dims$height, dpi = fig_dims$dpi, compression = "lzw")
}

render_geometry_plots <- function(frames_cache, geometry_label, filename_prefix) {
  if (!length(frames_cache)) return()
  message(sprintf("Rendering NMDS plots for %s geometry", geometry_label))
  batch_plots <- build_nmds_plot_list(frames_cache, geometry_label, batch_var, "Batch")
  save_nmds_plot_set(batch_plots, paste0("nmds_", filename_prefix, "_batch"))
  target_plots <- build_nmds_plot_list(frames_cache, geometry_label, target_var, "Target")
  save_nmds_plot_set(target_plots, paste0("nmds_", filename_prefix, "_target"))
}

render_geometry_plots(frames_cache_clr, "Aitchison", "aitchison")
render_geometry_plots(frames_cache_tss, "Bray-Curtis", "braycurtis")

if (!length(frames_cache_clr) && !length(frames_cache_tss)) {
  message("No NMDS frames available for plotting.")
}

# =========================
# NMDS assessment (with baseline-only option)
# =========================

prepare_group_scores <- function(plot_df, metadata, axes = c("NMDS1", "NMDS2"), group_var = "batch") {
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
  if (!is.finite(radius_a) || !is.finite(radius_b) || radius_a <= 0 || radius_b <= 0) {
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

summarise_nmds_method <- function(fr, method_name, geometry_label, metadata,
                                   axes = c("NMDS1", "NMDS2"), batch_var = "batch",
                                   target_var = "target") {
  batch_stats <- compute_group_metrics(fr$plot.df, metadata, axes = axes, group_var = batch_var)
  target_stats <- compute_group_metrics(fr$plot.df, metadata, axes = axes, group_var = target_var)
  tibble::tibble(
    Method = method_name,
    Geometry = geometry_label,
    NMDS_Stress = fr$stress,
    Centroid_Distance_Batch = batch_stats$centroid,
    Centroid_Distance_Target = target_stats$centroid,
    Ellipse_Overlap_Batch = batch_stats$overlap,
    Ellipse_Overlap_Target = target_stats$overlap,
    Target_vs_Batch_Centroid_Delta = target_vs_batch_dominance(target_stats$centroid, batch_stats$centroid)
  )
}

methods_clr <- names(frames_cache_clr)
methods_tss <- names(frames_cache_tss)
all_methods <- union(methods_clr, methods_tss)

only_baseline <- (length(all_methods) == 1L) && identical(all_methods, "Before correction")
output_name <- if (only_baseline) "nmds_raw_assessment_pre.csv" else "nmds_raw_assessment_post.csv"

if (only_baseline) {
  # ===== Baseline-only assessment (no ranking) =====
  assess_rows <- list()

  if ("Before correction" %in% methods_clr) {
    fr <- frames_cache_clr[["Before correction"]]
    row <- summarise_nmds_method(fr, "Before correction", "Ait", metadata,
                                 axes = c("NMDS1", "NMDS2"), batch_var = batch_var,
                                 target_var = target_var)
    if (!is.null(row)) assess_rows[[length(assess_rows) + 1L]] <- row
  }

  if ("Before correction" %in% methods_tss) {
    fr <- frames_cache_tss[["Before correction"]]
    row <- summarise_nmds_method(fr, "Before correction", "BC", metadata,
                                 axes = c("NMDS1", "NMDS2"), batch_var = batch_var,
                                 target_var = target_var)
    if (!is.null(row)) assess_rows[[length(assess_rows) + 1L]] <- row
  }

  assess_df <- if (length(assess_rows)) dplyr::bind_rows(assess_rows) else tibble::tibble()

  print(assess_df, n = nrow(assess_df))
  readr::write_csv(assess_df, file.path(output_folder, output_name))

  # No correction recommendation messages

} else {
  # ===== Multi-method assessment without ranking =====
  rows <- list()
  for (m in all_methods) {
    if (m %in% methods_clr) {
      fr_a <- frames_cache_clr[[m]]
      row_a <- summarise_nmds_method(fr_a, m, "Ait", metadata,
                                     axes = c("NMDS1", "NMDS2"), batch_var = batch_var,
                                     target_var = target_var)
      if (!is.null(row_a)) rows[[length(rows) + 1L]] <- row_a
    }
    if (m %in% methods_tss) {
      fr_b <- frames_cache_tss[[m]]
      row_b <- summarise_nmds_method(fr_b, m, "BC", metadata,
                                     axes = c("NMDS1", "NMDS2"), batch_var = batch_var,
                                     target_var = target_var)
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
