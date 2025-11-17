# ===================== PERMANOVA (Aitchison & Bray-Curtis) =====================
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(vegan)   # adonis2 / betadisper
})

# --------- Args / config ---------

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
apply_fig_overrides <- function(width_in, height_in, default_dpi = 600) {
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

# --------- Collect CLR files (baseline first) ---------
clr_paths <- list.files(output_folder, pattern = "^normalized_.*_clr\\.csv$", full.names = TRUE)
if (!length(clr_paths)) {
  clr_paths <- list.files(output_folder, pattern = "^normalized_.*\\.csv$", full.names = TRUE)
}
raw_clr_fp <- file.path(output_folder, "raw_clr.csv")
if (file.exists(raw_clr_fp)) clr_paths <- c(raw_clr_fp, clr_paths)
if (!length(clr_paths)) stop("No CLR matrices found (expected 'raw_clr.csv' or 'normalized_*_clr.csv').")

name_from <- function(paths, suffix) gsub(paste0("^normalized_|_", suffix, "\\.csv$"), "", basename(paths))
method_names <- ifelse(basename(clr_paths) == "raw_clr.csv",
                       "Before correction",
                       method_short_label(name_from(clr_paths, "clr")))
file_list <- setNames(clr_paths, method_names)
method_levels <- names(file_list)  # keep plotting order

# --------- Helpers (CLR + distance) ---------
safe_numeric_matrix <- function(df) {
  num <- dplyr::select(df, where(is.numeric))
  if (!ncol(num)) return(matrix(numeric(0), nrow = nrow(df)))
  keep <- vapply(num, function(z) all(is.finite(z)) && sd(z, na.rm = TRUE) > 0, logical(1))
  as.matrix(num[, keep, drop = FALSE])
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
permanova_one <- function(df, meta, geometry = c("aitchison", "bray-curtis"),
                          batch_col = "batch_id", permutations = 999) {
  geometry <- match.arg(tolower(geometry), c("aitchison", "bray-curtis"))
  if (!("sample_id" %in% names(df))) {
    if (nrow(df) == nrow(meta)) df$sample_id <- meta$sample_id else stop("need sample_id")
  }
  dfx <- dplyr::inner_join(df |> mutate(sample_id = as.character(sample_id)), meta, by = "sample_id")
  if (!nrow(dfx)) return(c(`R²` = NA_real_))

  X <- safe_numeric_matrix(dfx[, setdiff(names(df), "sample_id"), drop = FALSE])
  if (nrow(X) < 3 || ncol(X) < 2) return(c(`R²` = NA_real_))

  g <- factor(dfx[[batch_col]])
  if (nlevels(g) < 2) return(c(`R²` = 0))

  if (geometry == "aitchison") {
    has_neg <- any(X < 0, na.rm = TRUE)
    Y <- if (has_neg) sweep(X, 1, rowMeans(X, na.rm = TRUE), "-") else clr_transform(X)
    D <- stats::dist(Y, method = "euclidean")
  } else {
    Xbc <- X
    Xbc[!is.finite(Xbc)] <- 0
    Xbc[Xbc < 0] <- 0
    Xtss <- safe_closure(Xbc)
    if (!ncol(Xtss)) return(c(`R²` = NA_real_))
    D <- tryCatch(vegan::vegdist(Xtss, method = "bray"), error = function(e) NULL)
    if (is.null(D)) return(c(`R²` = NA_real_))
  }

  ad <- vegan::adonis2(D ~ g, permutations = permutations, by = "terms")
  R2 <- as.data.frame(ad)["g", "R2"]
  c(`R²` = unname(R2))
}

# --------- Compute PERMANOVA per method (both geometries) ---------
only_baseline <- (length(method_levels) == 1L && identical(method_levels, "Before correction"))
output_name <- if (only_baseline) "permanova_raw_assessment_pre.csv" else "permanova_raw_assessment_post.csv"

geometry_specs <- tibble::tibble(
  geometry_label = c("Aitchison", "Bray-Curtis"),
  geometry_key   = c("aitchison", "braycurtis"),
  arg_value      = c("aitchison", "bray-curtis")
)

results_by_geom <- list()
for (idx in seq_len(nrow(geometry_specs))) {
  geom_label <- geometry_specs$geometry_label[[idx]]
  geom_key   <- geometry_specs$geometry_key[[idx]]
  geom_arg   <- geometry_specs$arg_value[[idx]]

  geom_tbl <- tibble(Method = character(), Geometry = character(), `R²` = numeric())

  for (nm in names(file_list)) {
    cat("PERMANOVA (", geom_label, "): ", nm, "\n", sep = "")
    df <- read_csv(file_list[[nm]], show_col_types = FALSE)
    v <- permanova_one(df, metadata, geometry = geom_arg,
                       batch_col = "batch_id", permutations = 999)
    geom_tbl <- bind_rows(
      geom_tbl,
      tibble(Method = nm,
             Geometry = geom_label,
             `R²` = v[["R²"]])
    )
  }

  geom_tbl <- geom_tbl %>%
    mutate(Method = factor(Method, levels = method_levels)) %>%
    arrange(Method)

  results_by_geom[[geom_key]] <- geom_tbl

  plot_df <- geom_tbl %>% mutate(Method = factor(as.character(Method), levels = method_levels))
  p <- ggplot(plot_df, aes(x = Method, y = `R²`, fill = Method)) +
    geom_col(width = 0.72, color = "white", linewidth = 0.4, show.legend = FALSE) +
    geom_text(aes(label = sprintf("%.3f", `R²`)), vjust = -0.4, size = 3.2) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.02))) +
    labs(
      title = sprintf("PERMANOVA R\u00B2 (Batch, %s)", geom_label),
      x = "Method", y = "Score"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(hjust = 0.5, face = "plain")
    )

  fig_dims <- apply_fig_overrides(2800 / 600, 1800 / 600, 600)
  ggsave(file.path(output_folder, sprintf("permanova_%s.png", geom_key)), p,
         width = fig_dims$width, height = fig_dims$height, dpi = fig_dims$dpi)
  ggsave(file.path(output_folder, sprintf("permanova_%s.tif", geom_key)), p,
         width = fig_dims$width, height = fig_dims$height, dpi = fig_dims$dpi, compression = "lzw")
}

# --------- Save combined table ---------
summary_tbl <- dplyr::bind_rows(results_by_geom) %>%
  mutate(
    Method = factor(Method, levels = method_levels),
    Geometry = factor(Geometry, levels = geometry_specs$geometry_label)
  ) %>%
  arrange(Geometry, Method)

readr::write_csv(summary_tbl, file.path(output_folder, output_name))
print(summary_tbl, n = nrow(summary_tbl))
