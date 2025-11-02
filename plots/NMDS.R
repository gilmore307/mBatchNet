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
    qn = "QN", bmc = "BMC", limma = "Limma", conqur = "ConQuR",
    plsda = "PLSDA-batch", combat = "ComBat", fsqn = "FSQN", mmuphin = "MMUPHin",
    ruv = "RUV-III-NB", metadict = "MetaDICT", svd = "SVD", pn = "PN",
    fabatch = "FAbatch", combatseq = "ComBat-seq", debias = "DEBIAS-M"
  )
  sapply(x, function(v){ lv <- tolower(v); if (lv %in% names(map)) map[[lv]] else v })
}

  library(rlang)
  library(vegan)       # distances + NMDS
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

if (!("batch_id" %in% names(metadata)) && ("batch_id" %in% names(metadata))) {
  metadata$batch_id <- metadata$batch_id
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
compute_nmds_frames_aitch <- function(df, metadata, model.vars = c("batch_id","phenotype"), k = 2) {
  if (!"sample_id" %in% names(df)) {
    if (nrow(df) == nrow(metadata)) df$sample_id <- metadata$sample_id
    else stop("Input lacks 'sample_id' and row count != metadata.")
  }
  df  <- df %>% mutate(sample_id = as.character(sample_id))
  dfm <- inner_join(df, metadata, by = "sample_id")
  
  feat_cols <- setdiff(names(df), "sample_id")
  X <- dfm %>% select(all_of(feat_cols)) %>% select(where(is.numeric)) %>% as.matrix()
  if (!ncol(X)) stop("No numeric features for NMDS (CLR).")
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
  list(plot.df = plot.df, stress = fit$stress, used.vars = present)
}

# ==== NMDS frames: Bray-Curtis on TSS ====
compute_nmds_frames_bray <- function(df, metadata, model.vars = c("batch_id","phenotype"), k = 2) {
  if (!"sample_id" %in% names(df)) {
    if (nrow(df) == nrow(metadata)) df$sample_id <- metadata$sample_id
    else stop("Input lacks 'sample_id' and row count != metadata.")
  }
  df  <- df %>% mutate(sample_id = as.character(sample_id))
  dfm <- inner_join(df, metadata, by = "sample_id")
  
  feat_cols <- setdiff(names(df), "sample_id")
  X <- dfm %>% select(all_of(feat_cols)) %>% select(where(is.numeric)) %>% as.matrix()
  Xtss <- safe_closure(X)
  Db <- vegan::vegdist(Xtss, method = "bray")
  
  set.seed(123)
  fit <- vegan::monoMDS(Db, k = k, maxit = 500, smin = 1e-12, trace = FALSE)
  sc  <- fit$points; colnames(sc) <- paste0("NMDS", seq_len(ncol(sc)))
  plot.df <- data.frame(sample_id = dfm$sample_id, as.data.frame(sc), check.names = FALSE)
  present <- intersect(model.vars, names(dfm))
  for (v in present) plot.df[[v]] <- factor(as.character(dfm[[v]]), levels = unique(as.character(metadata[[v]])))
  list(plot.df = plot.df, stress = fit$stress, used.vars = present)
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
batch_var  <- "batch_id"
model_vars <- c(batch_var)
axes_to_plot <- c(1, 2)
ncol_grid <- 2
if (!is.na(opt_fig_ncol) && opt_fig_ncol >= 1) {
  ncol_grid <- max(1, opt_fig_ncol)
}
SYMMETRIC_AXES <- FALSE  # set TRUE to force symmetry about 0 (optional)

# =========================
# Set 1: NMDS - Aitchison (CLR)
# =========================
message("NMDS (Aitchison on CLR)")

frames_cache_clr <- list()
for (nm in names(file_list_clr)) {
  cat("Computing CLR NMDS:", nm, "\n")
  df <- read_csv(file_list_clr[[nm]], show_col_types = FALSE)
  fr <- compute_nmds_frames_aitch(df, metadata, model.vars = model_vars, k = max(axes_to_plot))
  frames_cache_clr[[nm]] <- fr
}

plots_clr <- lapply(names(file_list_clr), function(nm) {
  fr <- frames_cache_clr[[nm]]
  label_nm <- if (has_dual_geometries) sprintf("%s - Aitchison", nm) else nm
  x_override <- NULL
  y_override <- NULL
  if (isTRUE(SYMMETRIC_AXES)) {
    xcol <- paste0("NMDS", axes_to_plot[1])
    ycol <- paste0("NMDS", axes_to_plot[2])
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
  nmds_panel(fr$plot.df, model_vars,
             axes = axes_to_plot, label = label_nm,
             xlim_override = x_override, ylim_override = y_override, palette_name = "Batch")
})
names(plots_clr) <- names(file_list_clr)

# ---- Combine & save (CLR) ----
n_panels_clr <- length(plots_clr)
panel_cols_clr <- 1L
panel_rows_clr <- 1L
if (n_panels_clr == 1L) {
  combined_clr <- plots_clr[[1]] +
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.box = "vertical",
          plot.margin = margin(8, 14, 8, 14))
  w_clr <- 9.5; h_clr <- 6
} else {
  panel_cols_clr <- min(ncol_grid, n_panels_clr)
  panel_rows_clr <- ceiling(n_panels_clr / ncol_grid)
  combined_clr <- wrap_plots(plots_clr, ncol = ncol_grid) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.box = "vertical",
          plot.margin = margin(8, 14, 8, 14))
  w_clr <- 9.5 * panel_cols_clr
  h_clr <- 6   * panel_rows_clr
}

fig_dims_clr <- apply_fig_overrides(w_clr, h_clr, 300, panel_cols_clr, panel_rows_clr)
ggsave(file.path(output_folder, "nmds_aitchison.png"),
       plot = combined_clr, width = fig_dims_clr$width, height = fig_dims_clr$height, dpi = fig_dims_clr$dpi)
ggsave(file.path(output_folder, "nmds_aitchison.tif"),
       plot = combined_clr, width = fig_dims_clr$width, height = fig_dims_clr$height, dpi = fig_dims_clr$dpi, compression = "lzw")

# =========================
# Set 2: NMDS - Bray-Curtis (TSS)
# =========================
message("NMDS (Bray-Curtis on TSS)")

frames_cache_tss <- list()
for (nm in names(file_list_tss)) {
  cat("Computing TSS NMDS:", nm, "\n")
  df <- read_csv(file_list_tss[[nm]], show_col_types = FALSE)
  fr <- compute_nmds_frames_bray(df, metadata, model.vars = model_vars, k = max(axes_to_plot))
  frames_cache_tss[[nm]] <- fr
}

plots_tss <- lapply(names(file_list_tss), function(nm) {
  fr <- frames_cache_tss[[nm]]
  label_nm <- if (has_dual_geometries) sprintf("%s - Bray-Curtis", nm) else nm
  x_override <- NULL
  y_override <- NULL
  if (isTRUE(SYMMETRIC_AXES)) {
    xcol <- paste0("NMDS", axes_to_plot[1])
    ycol <- paste0("NMDS", axes_to_plot[2])
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
  nmds_panel(fr$plot.df, model_vars,
             axes = axes_to_plot, label = label_nm,
             xlim_override = x_override, ylim_override = y_override, palette_name = "Batch")
})
names(plots_tss) <- names(file_list_tss)

# ---- Combine & save (TSS) ----
n_panels_tss <- length(plots_tss)
panel_cols_tss <- 1L
panel_rows_tss <- 1L
if (n_panels_tss == 1L) {
  combined_tss <- plots_tss[[1]] +
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.box = "vertical",
          plot.margin = margin(8, 14, 8, 14))
  w_tss <- 9.5; h_tss <- 6
} else {
  panel_cols_tss <- min(ncol_grid, n_panels_tss)
  panel_rows_tss <- ceiling(n_panels_tss / ncol_grid)
  combined_tss <- wrap_plots(plots_tss, ncol = ncol_grid) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.box = "vertical",
          plot.margin = margin(8, 14, 8, 14))
  w_tss <- 9.5 * panel_cols_tss
  h_tss <- 6   * panel_rows_tss
}

fig_dims_tss <- apply_fig_overrides(w_tss, h_tss, 300, panel_cols_tss, panel_rows_tss)
ggsave(file.path(output_folder, "nmds_braycurtis.png"),
       plot = combined_tss, width = fig_dims_tss$width, height = fig_dims_tss$height, dpi = fig_dims_tss$dpi)
ggsave(file.path(output_folder, "nmds_braycurtis.tif"),
       plot = combined_tss, width = fig_dims_tss$width, height = fig_dims_tss$height, dpi = fig_dims_tss$dpi, compression = "lzw")

# =========================
# NMDS assessment (with baseline-only option)
# =========================

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
    stress <- fr$stress

    assess_rows[["Ait"]] <- tibble::tibble(
      Method            = "Before correction",
      Geometry          = "Ait",
      NMDS_Stress       = stress
    )
  }

  if ("Before correction" %in% methods_tss) {
    fr <- frames_cache_tss[["Before correction"]]
    stress <- fr$stress

    assess_rows[["BC"]] <- tibble::tibble(
      Method            = "Before correction",
      Geometry          = "BC",
      NMDS_Stress       = stress
    )
  }

  assess_df <- dplyr::bind_rows(assess_rows)

  print(assess_df, n = nrow(assess_df))
  readr::write_csv(assess_df, file.path(output_folder, output_name))

  # No correction recommendation messages

} else {
  # ===== Multi-method assessment without ranking =====
  rank_tbl <- dplyr::tibble(
    Method = character(),
    NMDS_Stress_Ait = numeric(),
    NMDS_Stress_BC  = numeric()
  )

  for (m in all_methods) {
    # CLR NMDS
    stress_a <- NA_real_
    if (m %in% methods_clr) {
      fr_a <- frames_cache_clr[[m]]
      stress_a <- fr_a$stress
    }

    # TSS NMDS
    stress_b <- NA_real_
    if (m %in% methods_tss) {
      fr_b <- frames_cache_tss[[m]]
      stress_b <- fr_b$stress
    }

    rank_tbl <- dplyr::bind_rows(rank_tbl, dplyr::tibble(
      Method = m,
      NMDS_Stress_Ait = stress_a,
      NMDS_Stress_BC  = stress_b
    ))
  }

  assessment_rows <- list()
  if (any(is.finite(rank_tbl$NMDS_Stress_Ait))) {
    assessment_rows[["Ait"]] <- tibble::tibble(
      Method = rank_tbl$Method,
      Geometry = "Ait",
      NMDS_Stress = rank_tbl$NMDS_Stress_Ait
    )
  }
  if (any(is.finite(rank_tbl$NMDS_Stress_BC))) {
    assessment_rows[["BC"]] <- tibble::tibble(
      Method = rank_tbl$Method,
      Geometry = "BC",
      NMDS_Stress = rank_tbl$NMDS_Stress_BC
    )
  }

  assessment_tbl <- dplyr::bind_rows(assessment_rows)
  assessment_tbl <- assessment_tbl %>%
    dplyr::arrange(Geometry, Method)

  print(assessment_tbl, n = nrow(assessment_tbl))
  readr::write_csv(assessment_tbl, file.path(output_folder, output_name))
}
