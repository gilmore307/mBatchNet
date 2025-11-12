# ===================== PERMANOVA (Aitchison & Bray-Curtis) =====================
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(vegan)   # adonis2 / betadisper / vegdist
})

# --------- Args / config ---------

# Map method codes to short labels for figures
method_short_label <- function(x) {
  map <- c(
    qn = "QN", bmc = "BMC", limma = "Limma", conqur = "ConQuR",
    plsda = "PLSDA-batch", combat = "ComBat", fsqn = "FSQN", mmuphin = "MMUPHin",
    ruv = "RUV-III-NB", metadict = "MetaDICT", pn = "PN",
    fabatch = "FAbatch", combatseq = "ComBat-seq", debias = "DEBIAS-M"
  )
  sapply(x, function(v){ lv <- tolower(v); if (lv %in% names(map)) map[[lv]] else v })
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  args <- "output/example"  # default folder
}
output_folder <- args[1]
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

# Figure overrides (like the EBM script)
opt_fig_width_px  <- NA_real_
opt_fig_height_px <- NA_real_
opt_fig_dpi       <- NA_real_
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
}
apply_fig_overrides <- function(width_in, height_in, default_dpi = 300) {
  dpi <- if (is.na(opt_fig_dpi) || opt_fig_dpi <= 0) default_dpi else opt_fig_dpi
  w <- width_in; h <- height_in
  if (!is.na(opt_fig_width_px)  && opt_fig_width_px  > 0 && dpi > 0) w <- opt_fig_width_px  / dpi
  if (!is.na(opt_fig_height_px) && opt_fig_height_px > 0 && dpi > 0) h <- opt_fig_height_px / dpi
  list(width = w, height = h, dpi = dpi)
}

# --------- Load metadata ---------
metadata <- read_csv(file.path(output_folder, "metadata.csv"), show_col_types = FALSE)
if (!("batch_id" %in% names(metadata))) stop("metadata.csv must contain 'batch_id'.")
if (!("sample_id" %in% names(metadata))) {
  metadata$sample_id <- sprintf("S%03d", seq_len(nrow(metadata)))
}
metadata <- metadata |> mutate(sample_id = as.character(sample_id))

# --------- Collect CLR / TSS files (baseline first) ---------
clr_paths <- list.files(output_folder, pattern = "^normalized_.*_clr\\.csv$", full.names = TRUE)
tss_paths <- list.files(output_folder, pattern = "^normalized_.*_tss\\.csv$", full.names = TRUE)

if (!length(clr_paths) && !length(tss_paths)) {
  any_paths <- list.files(output_folder, pattern = "^normalized_.*\\.csv$", full.names = TRUE)
  clr_paths <- any_paths
  tss_paths <- any_paths
}

name_from <- function(paths, suffix) gsub(paste0("^normalized_|_", suffix, "\\.csv$"), "", basename(paths))

file_list_clr <- setNames(clr_paths,
                          if (length(clr_paths)) method_short_label(name_from(clr_paths, "clr")) else character())
file_list_tss <- setNames(tss_paths,
                          if (length(tss_paths)) method_short_label(name_from(tss_paths, "tss")) else character())

raw_clr_fp <- file.path(output_folder, "raw_clr.csv")
raw_tss_fp <- file.path(output_folder, "raw_tss.csv")
if (file.exists(raw_clr_fp)) file_list_clr <- c("Before correction" = raw_clr_fp, file_list_clr)
if (file.exists(raw_tss_fp)) file_list_tss <- c("Before correction" = raw_tss_fp, file_list_tss)

if (!length(file_list_clr) && !length(file_list_tss)) {
  stop("No CLR/TSS matrices found (expected raw_clr/raw_tss and/or normalized_*_clr / normalized_*_tss).")
}

method_levels <- unique(c(names(file_list_clr), names(file_list_tss)))
has_dual_geometries <- length(file_list_clr) > 0 && length(file_list_tss) > 0

# --------- Helpers (CLR + distance) ---------
safe_numeric_matrix <- function(df) {
  num <- dplyr::select(df, where(is.numeric))
  if (!ncol(num)) return(matrix(numeric(0), nrow = nrow(df)))
  keep <- vapply(num, function(z) all(is.finite(z)) && sd(z, na.rm = TRUE) > 0, logical(1))
  as.matrix(num[, keep, drop = FALSE])
}
align_samples_1to1 <- function(df, metadata) {
  if (!"sample_id" %in% names(df)) {
    if (nrow(df) == nrow(metadata)) df$sample_id <- metadata$sample_id else stop("need sample_id")
  }
  df <- df %>% mutate(sample_id = as.character(sample_id))
  md <- metadata %>% mutate(sample_id = as.character(sample_id))

  df <- df %>% group_by(sample_id) %>% mutate(.dup_idx = dplyr::row_number()) %>% ungroup()
  md <- md %>% group_by(sample_id) %>% mutate(.dup_idx = dplyr::row_number()) %>% ungroup()

  out <- suppressWarnings(dplyr::inner_join(df, md, by = c("sample_id", ".dup_idx")))
  out <- dplyr::select(out, -".dup_idx")
  out
}
safe_closure <- function(X) {
  X <- as.matrix(X)
  X[!is.finite(X)] <- 0
  X[X < 0] <- 0
  rs <- rowSums(X)
  rs[rs == 0 | !is.finite(rs)] <- 1
  sweep(X, 1, rs, "/")
}
clr_transform <- function(X, pseudocount = 1) {
  Xp <- safe_closure(X) + (pseudocount / max(1, ncol(X)))
  L  <- log(Xp)
  sweep(L, 1, rowMeans(L), "-")
}
permanova_one <- function(df, meta, geometry = c("Ait", "BC"), batch_col = "batch_id", permutations = 999) {
  geometry <- match.arg(geometry)
  dfx <- align_samples_1to1(df, meta)
  if (!nrow(dfx)) return(NA_real_)

  data_cols <- setdiff(names(df), "sample_id")
  X <- safe_numeric_matrix(dfx[, data_cols, drop = FALSE])
  if (nrow(X) < 3 || ncol(X) < 2) return(NA_real_)

  g <- factor(dfx[[batch_col]])
  if (nlevels(g) < 2) return(0)

  D <- switch(geometry,
              Ait = {
                has_neg <- any(X < 0, na.rm = TRUE)
                Y <- if (has_neg) sweep(X, 1, rowMeans(X, na.rm = TRUE), "-") else clr_transform(X)
                stats::dist(Y, method = "euclidean")
              },
              BC = {
                Xb <- safe_closure(X)
                suppressWarnings(vegan::vegdist(Xb, method = "bray"))
              })

  if (!length(D)) return(NA_real_)
  if (any(!is.finite(as.vector(D)))) return(NA_real_)

  ad <- vegan::adonis2(D ~ g, permutations = permutations, by = "terms")
  R2 <- as.data.frame(ad)["g", "R2"]
  unname(R2)
}

# --------- Compute PERMANOVA per method / geometry ---------
res_rows <- list()

if (length(file_list_clr)) {
  for (nm in names(file_list_clr)) {
    cat("PERMANOVA (Ait): ", nm, "\n")
    df <- read_csv(file_list_clr[[nm]], show_col_types = FALSE)
    r2 <- permanova_one(df, metadata, geometry = "Ait", batch_col = "batch_id", permutations = 999)
    res_rows[[length(res_rows) + 1L]] <- tibble(Method = nm, Geometry = "Ait", R2 = r2)
  }
}

if (length(file_list_tss)) {
  for (nm in names(file_list_tss)) {
    cat("PERMANOVA (BC): ", nm, "\n")
    df <- read_csv(file_list_tss[[nm]], show_col_types = FALSE)
    r2 <- permanova_one(df, metadata, geometry = "BC", batch_col = "batch_id", permutations = 999)
    res_rows[[length(res_rows) + 1L]] <- tibble(Method = nm, Geometry = "BC", R2 = r2)
  }
}

res_tbl <- if (length(res_rows)) bind_rows(res_rows) else tibble(Method = character(), Geometry = character(), R2 = numeric())

# --------- Save & plot (series naming like EBM) ---------
only_baseline <- (length(method_levels) == 1L && identical(method_levels, "Before correction"))
output_name <- if (only_baseline) "permanova_raw_assessment_pre.csv" else "permanova_raw_assessment_post.csv"

geometry_levels <- c("Ait", "BC")
summary_tbl <- res_tbl %>%
  mutate(
    Method = factor(Method, levels = method_levels),
    Geometry = factor(Geometry, levels = geometry_levels[geometry_levels %in% unique(Geometry)])
  ) %>%
  arrange(Geometry, Method)

l10n <- l10n_info()
r2_label <- if (isTRUE(l10n[["UTF-8"]])) "R\u00B2" else "R^2"

summary_tbl_out <- summary_tbl %>%
  mutate(
    Method = as.character(Method),
    Geometry = as.character(Geometry)
  ) %>%
  rename(!!r2_label := R2)

readr::write_csv(summary_tbl_out, file.path(output_folder, output_name))
print(summary_tbl_out, n = nrow(summary_tbl_out))

# simple bar chart (R²; lower is better)
plot_df <- summary_tbl %>%
  mutate(Method = factor(as.character(Method), levels = method_levels))

geometry_pretty <- c(Ait = "Aitchison", BC = "Bray–Curtis")
plot_dims_single <- if (only_baseline) apply_fig_overrides(6.5, 4.6, 300) else apply_fig_overrides(8.5, 5.2, 300)

build_permanova_plot <- function(df, geom_key) {
  if (!nrow(df)) return(NULL)
  geom_label <- geometry_pretty[[geom_key]]
  if (is.null(geom_label)) geom_label <- geom_key
  plot_title <- if (isTRUE(has_dual_geometries)) {
    sprintf("PERMANOVA %s (%s)", geom_label, r2_label)
  } else {
    sprintf("PERMANOVA %s", r2_label)
  }

  ggplot(df, aes(x = Method, y = R2, fill = Method)) +
    geom_col(width = 0.72, color = "white", linewidth = 0.4, na.rm = TRUE, show.legend = FALSE) +
    geom_text(aes(label = ifelse(is.finite(R2), sprintf("%.3f", R2), "")),
              vjust = -0.35, size = 3.2, na.rm = FALSE) +
    scale_y_continuous(limits = c(0, 1.05), expand = expansion(mult = c(0, 0.02))) +
    labs(
      title = plot_title,
      subtitle = "Lower is less batch effect",
      x = "Method", y = r2_label
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.title = element_text(hjust = 0.5)
    )
}

permanova_plots <- lapply(levels(plot_df$Geometry), function(geom_key) {
  df_geom <- dplyr::filter(plot_df, Geometry == geom_key)
  list(key = geom_key, plot = build_permanova_plot(df_geom, geom_key))
})

for (item in permanova_plots) {
  geom_key <- item$key
  p <- item$plot
  if (is.null(p)) next
  file_stub <- switch(geom_key,
                      Ait = "permanova_aitchison",
                      BC  = "permanova_braycurtis",
                      paste0("permanova_", tolower(geom_key)))
  ggsave(file.path(output_folder, paste0(file_stub, ".png")), p,
         width = plot_dims_single$width, height = plot_dims_single$height, dpi = plot_dims_single$dpi)
  ggsave(file.path(output_folder, paste0(file_stub, ".tif")), p,
         width = plot_dims_single$width, height = plot_dims_single$height, dpi = plot_dims_single$dpi, compression = "lzw")
}

plots_available <- Filter(function(x) !is.null(x$plot), permanova_plots)
if (length(plots_available) >= 2) {
  combined <- wrap_plots(lapply(plots_available, `[[`, "plot"), ncol = length(plots_available)) +
    plot_layout(guides = "collect")
  combined_dims <- apply_fig_overrides(12, 5.5, 300)
  ggsave(file.path(output_folder, "permanova.png"), combined,
         width = combined_dims$width, height = combined_dims$height, dpi = combined_dims$dpi)
  ggsave(file.path(output_folder, "permanova.tif"), combined,
         width = combined_dims$width, height = combined_dims$height, dpi = combined_dims$dpi, compression = "lzw")
}
