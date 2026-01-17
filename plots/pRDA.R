# ================= pRDA variance partition (Aitchison geometry) =================
# Uses normalized_*_clr.csv matrices (and raw_clr.csv baseline when available)
suppressPackageStartupMessages({
  library(ggplot2)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(gridExtra)   # tableGrob + arrangeGrob
  library(grid)        # grobs
  library(jsonlite)

  library(gtable)      # table tweaks
  library(vegan)       # rda, capscale, RsquareAdj
})

source("plots/helper.R")

# --------- Args / IO ---------
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
  fallback <- c("group", "condition", "status", "class", "label")
  cand <- fallback[fallback %in% names(metadata)]
  if (length(cand)) {
    label_col <- cand[1]
  } else if ("phenotype" %in% names(metadata)) {
    label_col <- "phenotype"
  } else {
    stop("metadata file lacks a label column for pRDA plots.")
  }
}

if (!("batch" %in% names(metadata)) && ("batch" %in% names(metadata))) {
  metadata$batch <- metadata$batch
}

# ---- Find normalized files ----
clr_paths <- list.files(output_folder, pattern = "^normalized_.*_clr\\.csv$", full.names = TRUE)

# Fallback: if no suffix-specific outputs, use any normalized_*.csv as CLR
if (!length(clr_paths)) {
  clr_paths <- list.files(output_folder, pattern = "^normalized_.*\\.csv$", full.names = TRUE)
}

file_list_clr <- setNames(clr_paths, method_short_label(name_from(clr_paths, "clr")))

# Include raw_clr.csv as "Before correction" if present
raw_clr_fp <- file.path(output_folder, "raw_clr.csv")
if (file.exists(raw_clr_fp)) file_list_clr <- c("Before correction" = raw_clr_fp, file_list_clr)

if (!length(file_list_clr)) {
  stop("No normalized CLR files found (expected raw_clr.csv and/or normalized_*_clr.csv) in ", output_folder)
}

# --------- Helpers (CLR + safe R^2) ---------
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
safe_adjR2 <- function(fit) {
  out <- tryCatch(vegan::RsquareAdj(fit), error = function(e) list(r.squared=NA_real_, adj.r.squared=NA_real_))
  adj <- suppressWarnings(out$adj.r.squared)
  if (is.finite(adj)) return(max(0, adj))
  r <- suppressWarnings(out$r.squared)
  if (is.finite(r)) return(max(0, r))
  0
}

# --------- Core calculators (return named numeric: Target, Intersection, Batch, Residuals) ---------
compute_prda_parts_aitch <- function(df, meta, batch_col = "batch", treat_col = label_col) {
  fmt_col <- function(name) sprintf("`%s`", gsub("`", "``", name))
  # ensure sample_id present & align
  if (!"sample_id" %in% names(df)) {
    if (nrow(df) == nrow(meta)) df$sample_id <- meta$sample_id
    else stop("Input lacks 'sample_id' and row count != metadata; can't align samples.")
  }
  df  <- df %>% mutate(sample_id = as.character(sample_id))
  dfx <- inner_join(df, meta, by = "sample_id")

  if (!(batch_col %in% names(dfx))) stop(sprintf("Batch column '%s' not in metadata/joined data.", batch_col))
  if (!(treat_col %in% names(dfx))) stop(sprintf("Treatment column '%s' not in metadata/joined data.", treat_col))

  dfx <- dfx %>%
    filter(!is.na(.data[[batch_col]]), !is.na(.data[[treat_col]]))
  
  # features
  feat_cols <- setdiff(names(df), "sample_id")
  X <- dfx %>% select(all_of(feat_cols)) %>% select(where(is.numeric)) %>% as.matrix()
  keep <- apply(X, 2, function(z) all(is.finite(z)) && sd(z, na.rm = TRUE) > 0)
  X <- X[, keep, drop = FALSE]
  if (!ncol(X)) return(c(Target=0, Intersection=0, Batch=0, Residuals=1))
  
  # Aitchison response (CLR; if already log-ratio, row-center)
  Y <- if (any(X < 0, na.rm = TRUE)) sweep(X, 1, rowMeans(X, na.rm = TRUE), "-") else clr_transform(X)
  
  dfx[[batch_col]] <- factor(dfx[[batch_col]])
  dfx[[treat_col]] <- factor(dfx[[treat_col]])
  if (nlevels(dfx[[treat_col]]) < 2 || nlevels(dfx[[batch_col]]) < 2) {
    return(c(Target=0, Intersection=0, Batch=0, Residuals=1))
  }
  
  Ymat <- Y
  treat_term <- fmt_col(treat_col)
  batch_term <- fmt_col(batch_col)
  f_both   <- as.formula(sprintf("Ymat ~ %s + %s", treat_term, batch_term))
  f_t_pure <- as.formula(sprintf("Ymat ~ %s + Condition(%s)", treat_term, batch_term))
  f_b_pure <- as.formula(sprintf("Ymat ~ %s + Condition(%s)", batch_term, treat_term))
  
  fit_both  <- vegan::rda(f_both,   data = dfx)
  fit_t     <- vegan::rda(f_t_pure, data = dfx)
  fit_b     <- vegan::rda(f_b_pure, data = dfx)
  
  r2_both   <- safe_adjR2(fit_both)
  r2_t_pure <- safe_adjR2(fit_t)
  r2_b_pure <- safe_adjR2(fit_b)
  
  r2_inter  <- max(0, r2_both - r2_t_pure - r2_b_pure)
  r2_resid  <- max(0, 1 - (r2_t_pure + r2_b_pure + r2_inter))
  
  parts <- c(Target = r2_t_pure, Intersection = r2_inter,
             Batch = r2_b_pure, Residuals = r2_resid)
  parts[!is.finite(parts)] <- 0
  parts <- pmax(0, parts)
  s <- sum(parts)
  if (s > 0) parts <- parts / s
  parts
}

# --------- Plot + styled table (reusable) ---------
plot_prda_with_table <- function(parts_df, file_list, title_prefix, outfile_prefix) {
  component_order <- c("Residuals","Batch","Intersection","Target")
  stopifnot(all(dplyr::count(parts_df, Method)$n == 4))
  
  parts_df <- parts_df %>%
    arrange(Method) %>%
    group_by(Method) %>%
    mutate(Component = factor(component_order[row_number()], levels = component_order)) %>%
    ungroup() %>%
    mutate(
      Method   = factor(Method, levels = names(file_list)),
      Fraction = pmin(pmax(Fraction, 0), 1)
    )
  
  cols <- c(
    "Residuals"    = "#BDBDBD",
    "Batch"        = "#FF7F0E",
    "Intersection" = "#FFD54F",
    "Target"       = "#1F77B4"
  )
  
  p <- ggplot(parts_df, aes(x = Method, y = Fraction, fill = Component)) +
    geom_col(width = 0.72, color = "white", linewidth = 0.4,
             position = position_stack(reverse = TRUE)) +
    scale_fill_manual(
      values = cols,
      breaks = rev(component_order),   # c("Target","Intersection","Batch","Residuals")
      limits = component_order,   # lock the scale order
      name   = "Variance Components"
    )+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1.05), expand = expansion(mult = c(0, 0))) +
    labs(x = "Methods", y = "Explained variance (%)",
         title = title_prefix) +
    theme_bw() +
    theme(
      legend.position    = "right",
      legend.title       = element_text(size = 10),
      legend.text        = element_text(size = 9),
      axis.text.x        = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(hjust = 0.5, face = "bold")
    )
  
  # Values table
  tbl <- parts_df %>%
    dplyr::select(Method, Component, Fraction) %>%
    dplyr::mutate(`%` = scales::percent(Fraction, accuracy = 1)) %>%
    dplyr::select(-Fraction) %>%
    tidyr::pivot_wider(names_from = Component, values_from = `%`) %>%
    dplyr::arrange(Method)
  
  nr <- nrow(tbl); nc <- ncol(tbl)
  stripe_rows <- rep(c("#FBFCFF", "#F7F8FC"), length.out = nr)
  fill_core <- matrix(rep(stripe_rows, each = nc), nrow = nr, ncol = nc)
  
  tbl_theme <- gridExtra::ttheme_minimal(
    core = list(
      fg_params = list(fontsize = 9),
      bg_params = list(fill = fill_core, col = "#D0D7DE"),
      padding   = grid::unit(c(6, 4), "mm")
    ),
    colhead = list(
      fg_params = list(fontsize = 10, fontface = 2),
      bg_params = list(fill = "#ECEFF4", col = "#D0D7DE"),
      padding   = grid::unit(c(7, 4), "mm")
    )
  )
  
  tbl_grob <- gridExtra::tableGrob(tbl, rows = NULL, theme = tbl_theme)
  tbl_grob$widths <- grid::unit(rep(1, length(tbl_grob$widths)), "null")
  
  header_rows <- grep("^colhead", tbl_grob$layout$name)
  if (length(header_rows)) {
    header_bottom <- max(tbl_grob$layout$b[header_rows])
    tbl_grob <- gtable::gtable_add_grob(
      tbl_grob,
      grobs = grid::segmentsGrob(
        x0 = grid::unit(0, "npc"), x1 = grid::unit(1, "npc"),
        y0 = grid::unit(1, "npc"), y1 = grid::unit(1, "npc"),
        gp = grid::gpar(col = "#9AA0A6", lwd = 1.2)
      ),
      t = header_bottom, l = 1, r = ncol(tbl_grob), name = "header-sep"
    )
  }
  
  
  # Add a rule under the header
  header_rows <- which(tbl_grob$layout$name %in% c("colhead-fg", "colhead-bg"))
  if (length(header_rows)) {
    header_bottom <- max(tbl_grob$layout$b[header_rows])
    tbl_grob <- gtable::gtable_add_grob(
      tbl_grob,
      grobs = grid::segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"),
                                 gp = grid::gpar(col = "#9AA0A6", lwd = 1.2)),
      t = header_bottom, l = 1, r = ncol(tbl_grob), name = "header-sep"
    )
  }
  
  spacer <- grid::nullGrob()
  spacer_height <- grid::unit(0.18, "in")

  combined <- gridExtra::arrangeGrob(
    p, spacer, tbl_grob, ncol = 1,
    heights = grid::unit.c(grid::unit(1, "null"), spacer_height, sum(tbl_grob$heights))
  )

  hist_dims <- apply_fig_overrides(10, 5, 300)
  table_height_in <- grid::convertHeight(sum(tbl_grob$heights), "in", valueOnly = TRUE)
  final_width  <- hist_dims$width
  final_height <- hist_dims$height + table_height_in +
    grid::convertHeight(spacer_height, "in", valueOnly = TRUE)

  ggsave(file.path(output_folder, paste0(outfile_prefix, ".tif")),
         plot = combined, width = final_width, height = final_height, dpi = hist_dims$dpi, compression = "lzw")
  
  invisible(list(plot = p, table = tbl))
}

# --------- Compute & plot: A) Aitchison (CLR + RDA) ---------
if (length(file_list_clr)) {
  batch_col <- "batch"; treat_col <- label_col
  if (!(batch_col %in% names(metadata))) stop("metadata file lacks 'batch'.")
  if (dplyr::n_distinct(metadata[[batch_col]]) < 2) stop("'batch' needs >= 2 levels.")
  if (!(treat_col %in% names(metadata))) stop("metadata file lacks the label column.")
  if (dplyr::n_distinct(metadata[[treat_col]]) < 2) stop("Label column needs at least 2 levels.")
  
  parts_df_aitch <- lapply(names(file_list_clr), function(nm) {
    message("pRDA (Aitchison) for: ", nm)
    df <- read_csv(file_list_clr[[nm]], show_col_types = FALSE)
    v  <- compute_prda_parts_aitch(df, metadata, batch_col, treat_col)
    tibble::tibble(Method = nm, Fraction = as.numeric(v))
  }) |> bind_rows()
}

# ==== pRDA scoring (Aitchison) =========================
# Per-method score (higher = better):
#   Proportion = Target / (Target + Batch + 1e-12)

component_order <- c("Residuals","Batch","Intersection","Target")

ensure_components <- function(parts_df) {
  if (!"Component" %in% names(parts_df)) {
    parts_df <- parts_df %>%
      group_by(Method) %>%
      mutate(Component = factor(component_order[row_number()],
                                levels = component_order)) %>%
      ungroup()
  } else {
    parts_df <- parts_df %>%
      mutate(Component = factor(as.character(Component),
                                levels = component_order))
  }
  parts_df
}

component_breakdown <- function(parts_df) {
  parts_df %>%
    ensure_components() %>%
    group_by(Method) %>%
    summarise(
      Target      = sum(Fraction[Component == "Target"],   na.rm = TRUE),
      Batch       = sum(Fraction[Component == "Batch"],    na.rm = TRUE),
      Intersection = sum(Fraction[Component == "Intersection"], na.rm = TRUE),
      Residuals    = sum(Fraction[Component == "Residuals"],    na.rm = TRUE),
      .groups     = "drop"
    )
}

tb_clr <- if (exists("parts_df_aitch")) component_breakdown(parts_df_aitch) else NULL

# Determine if we only have the baseline ("Before correction")
avail_methods <- if (exists("parts_df_aitch")) unique(parts_df_aitch$Method) else character()
only_baseline <- length(avail_methods) == 1L && identical(avail_methods, "Before correction")
output_name <- if (only_baseline) "pRDA_raw_assessment_pre.csv" else "pRDA_raw_assessment_post.csv"

if (only_baseline) {
  # ---- Baseline-only assessment (no ranking) ----
  assess_rows <- list()

  if (exists("parts_df_aitch")) {
    pf <- ensure_components(parts_df_aitch) %>% filter(Method == "Before correction")
    tr <- sum(pf$Fraction[pf$Component == "Target"], na.rm = TRUE)
    bt <- sum(pf$Fraction[pf$Component == "Batch"],     na.rm = TRUE)
    it <- sum(pf$Fraction[pf$Component == "Intersection"], na.rm = TRUE)
    rs <- sum(pf$Fraction[pf$Component == "Residuals"],    na.rm = TRUE)
    assess_rows[["Ait"]] <- tibble::tibble(
      Geometry = "Ait",
      Target = tr, Batch = bt, Intersection = it, Residuals = rs
    )
  }

  assess_df <- dplyr::bind_rows(assess_rows)
  print(assess_df, n = nrow(assess_df))
  readr::write_csv(assess_df, file.path(output_folder, output_name))

} else {
  # ---- Normal multi-method summary ----
  unified <- tibble::as_tibble(tb_clr)

  print(unified, n = nrow(unified))
  readr::write_csv(unified, file.path(output_folder, output_name))
}

# ========= Plot after CSVs are written =========
if (exists("parts_df_aitch") && nrow(parts_df_aitch)) {
  plot_prda_with_table(
    parts_df_aitch, file_list_clr,
    title_prefix  = "Partial Redundant Analysis: Variance Partition",
    outfile_prefix = "pRDA_aitchison"
  )
} else {
  message("No pRDA components available for plotting.")
}
