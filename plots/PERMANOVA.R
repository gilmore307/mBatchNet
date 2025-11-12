# ===================== PERMANOVA (CLR-only, Aitchison) =====================
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
permanova_one <- function(df, meta, batch_col = "batch_id", permutations = 999) {
  if (!("sample_id" %in% names(df))) {
    if (nrow(df) == nrow(meta)) df$sample_id <- meta$sample_id else stop("need sample_id")
  }
  dfx <- dplyr::inner_join(df |> mutate(sample_id = as.character(sample_id)), meta, by = "sample_id")
  if (!nrow(dfx)) return(c(R2 = NA_real_, P_value = NA_real_, Dispersion_P = NA_real_))

  # numeric features -> CLR if needed
  X <- safe_numeric_matrix(dfx[, setdiff(names(df), "sample_id"), drop = FALSE])
  if (nrow(X) < 3 || ncol(X) < 2) return(c(R2 = NA_real_, P_value = NA_real_, Dispersion_P = NA_real_))
  has_neg <- any(X < 0, na.rm = TRUE)
  Y <- if (has_neg) sweep(X, 1, rowMeans(X, na.rm = TRUE), "-") else clr_transform(X)

  g <- factor(dfx[[batch_col]])
  if (nlevels(g) < 2) return(c(R2 = 0, P_value = NA_real_, Dispersion_P = NA_real_))

  D <- dist(Y, method = "euclidean")  # Aitchison via CLR+Euclidean
  ad <- vegan::adonis2(D ~ g, permutations = permutations, by = "terms")
  R2 <- as.data.frame(ad)["g", "R2"]
  P  <- as.data.frame(ad)["g", "Pr(>F)"]

  # dispersion check (optional)
  bd <- tryCatch(vegan::betadisper(D, group = g), error = function(e) NULL)
  Pd <- if (!is.null(bd)) {
    as.data.frame(vegan::permutest(bd, permutations = permutations)$tab)[1, "Pr(>F)"]
  } else NA_real_

  c(R2 = unname(R2), P_value = unname(P), Dispersion_P = unname(Pd))
}

# --------- Compute PERMANOVA per method ---------
res_tbl <- tibble(Method = character(), R2 = numeric(), P_value = numeric(), Dispersion_P = numeric())
for (nm in names(file_list)) {
  cat("PERMANOVA (Ait): ", nm, "\n")
  df <- read_csv(file_list[[nm]], show_col_types = FALSE)
  v <- permanova_one(df, metadata, batch_col = "batch_id", permutations = 999)
  res_tbl <- bind_rows(res_tbl, tibble(Method = nm, R2 = v["R2"], P_value = v["P_value"], Dispersion_P = v["Dispersion_P"]))
}

# --------- Save & plot (series naming like EBM) ---------
only_baseline <- (length(method_levels) == 1L && identical(method_levels, "Before correction"))
output_name <- if (only_baseline) "permanova_raw_assessment_pre.csv" else "permanova_raw_assessment_post.csv"

# write CSV (keep original order)
summary_tbl <- res_tbl %>% mutate(Method = factor(Method, levels = method_levels)) %>% arrange(Method)
readr::write_csv(summary_tbl, file.path(output_folder, output_name))
print(summary_tbl, n = nrow(summary_tbl))

# simple bar chart (R²; lower is better)
plot_df <- summary_tbl %>% mutate(Method = factor(as.character(Method), levels = method_levels))
p <- ggplot(plot_df, aes(x = Method, y = R2, fill = Method)) +
  geom_col(width = 0.72, color = "white", linewidth = 0.4, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.3f", R2)), vjust = -0.4, size = 3.2) +
  scale_y_continuous(limits = c(0, 1.05), expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = "PERMANOVA R\u00B2",
    subtitle = "Lower is less batch effect",
    x = "Method", y = "R\u00B2"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank()
  )

# figure export (same size style as EBM)
fig_dims <- if (only_baseline) apply_fig_overrides(6.5, 4.6, 300) else apply_fig_overrides(8.5, 5.2, 300)
ggsave(file.path(output_folder, "permanova.png"), p,
       width = fig_dims$width, height = fig_dims$height, dpi = fig_dims$dpi)
ggsave(file.path(output_folder, "permanova.tif"), p,
       width = fig_dims$width, height = fig_dims$height, dpi = fig_dims$dpi, compression = "lzw")