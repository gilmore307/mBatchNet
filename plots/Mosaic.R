# ==== Libraries ====
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)      # for pivot_wider()
library(gridExtra)
library(grid)       # for unit()
library(jsonlite)

# ==== Args / config (for input and output folder) ====
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  args <- "output/example"  # default folder for quick runs
}
output_folder <- args[1]
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

PHENO_COL <- NULL

# Optional study settings from session_config.json (in output_folder)
CONTROL_LABEL <- NA_character_
try({
  cfg_path <- file.path(output_folder, "session_config.json")
  if (file.exists(cfg_path)) {
    cfg <- tryCatch(jsonlite::fromJSON(cfg_path), error = function(e) NULL)
    if (!is.null(cfg) && !is.null(cfg$control_label)) {
      CONTROL_LABEL <- as.character(cfg$control_label)
    }
  }
}, silent = TRUE)

# ==== Read metadata (metadata.csv) ====
metadata <- read_csv(file.path(output_folder, "metadata.csv"), show_col_types = FALSE)
if (!("sample_id" %in% names(metadata))) {
  metadata$sample_id <- sprintf("S%03d", seq_len(nrow(metadata)))
}
metadata <- metadata %>% mutate(sample_id = as.character(sample_id))

# Resolve the label column: prefer session config, fallback to phenotype
if (is.null(PHENO_COL)) {
  try({
    cfg_path <- file.path(output_folder, "session_config.json")
    if (file.exists(cfg_path)) {
      cfg <- jsonlite::fromJSON(cfg_path)
      if (!is.null(cfg$label_column)) PHENO_COL <<- cfg$label_column
    }
  }, silent = TRUE)
}
if (is.null(PHENO_COL) || !(PHENO_COL %in% names(metadata))) {
  if ("phenotype" %in% names(metadata)) {
    PHENO_COL <- "phenotype"
  }
}

# Check if the phenotype/label column exists
if (is.null(PHENO_COL) || !PHENO_COL %in% names(metadata))
  stop("Label column not found in metadata.csv; cannot build mosaic plot.")

# Prepare .outcome as a two-class factor, keeping the user's original labels
vals <- as.character(metadata[[PHENO_COL]])
if (!is.na(CONTROL_LABEL) && CONTROL_LABEL %in% vals) {
  outcome_levels <- c(CONTROL_LABEL, setdiff(unique(vals), CONTROL_LABEL))
} else {
  outcome_levels <- unique(vals)
}

if (length(outcome_levels) != 2) {
  stop(sprintf("Label column '%s' must contain exactly 2 classes for the mosaic plot.", PHENO_COL))
}

metadata <- metadata %>% mutate(.outcome = factor(.data[[PHENO_COL]], levels = outcome_levels))
OUTCOME_TITLE <- if (!is.null(PHENO_COL)) PHENO_COL else "Outcome"

# ==== Collect files (e.g., normalized files) ====
file_paths <- list.files(output_folder, pattern = "^normalized_.*\\.csv$", full.names = TRUE)
raw_path <- file.path(output_folder, "raw.csv")

# Choose primary feature file (prefer raw.csv, else first normalized_*.csv)
primary_file <- if (file.exists(raw_path)) {
  raw_path
} else if (length(file_paths) > 0) {
  file_paths[1]
} else {
  stop("No feature files found: expected 'raw.csv' or at least one 'normalized_*.csv' in ", output_folder)
}

# Keep a named list (optional; first element is the one we read)
file_list <- c(Raw = raw_path, setNames(file_paths, gsub("^normalized_|\\.csv$", "", basename(file_paths))))

# ==== Merge feature data with metadata (df_merged) ====
df_raw <- if (basename(primary_file) == "raw.csv") {
  read_csv(primary_file, show_col_types = FALSE, col_names = FALSE)
} else {
  read_csv(primary_file, show_col_types = FALSE)
}

# Ensure sample_id is included and matches metadata
if (!"sample_id" %in% names(df_raw)) {
  if (nrow(df_raw) == nrow(metadata)) {
    df_raw <- df_raw %>% mutate(sample_id = metadata$sample_id)
  } else {
    warning(sprintf("File %s has no sample_id and row count doesn't match metadata; skipping.", primary_file))
  }
}

# Merge metadata and feature data
batch_levels <- sort(unique(metadata$batch_id))

df_merged <- df_raw %>%
  mutate(sample_id = as.character(sample_id)) %>%
  inner_join(metadata, by = "sample_id")

# Ensure that batch_id and .outcome are factors for proper plotting
df_merged <- df_merged %>%
  mutate(batch_id = factor(batch_id, levels = batch_levels),
         .outcome = factor(.outcome))

# ==== Prepare the mosaic data ====
mosaic_data <- df_merged %>%
  group_by(batch_id, .outcome) %>%
  summarise(count = n(), .groups = "drop")

# Calculate the total count to get proportions
total_count <- sum(mosaic_data$count)

# Add proportion column
mosaic_data <- mosaic_data %>%
  mutate(proportion = count / total_count)

# ==== Define the Mosaic Plot Function (separate palettes for batch vs outcome) ====
mbecMosaicPlot <- function(study.summary, model.vars) {
  
  # Color pools
  mbecCols <- c("#9467bd", "#BCBD22", "#2CA02C", "#E377C2", "#1F77B4", "#FF7F0E",
                "#AEC7E8", "#FFBB78", "#98DF8A", "#D62728", "#FF9896", "#C5B0D5",
                "#8C564B", "#C49C94", "#F7B6D2", "#7F7F7F", "#C7C7C7", "#DBDB8D",
                "#17BECF", "#9EDAE5")
  
  batchCols   <- mbecCols
  outcome_levels <- levels(study.summary$.outcome)
  outcome_palette <- c("#222222", "#BBBBBB")
  outcomeCols <- stats::setNames(outcome_palette[seq_along(outcome_levels)], outcome_levels)  # distinct from batch pool
  
  # Safety check: no overlap allowed
  if (length(intersect(toupper(batchCols), toupper(outcomeCols))) > 0) {
    stop("Outcome colors overlap with batch color pool; choose different outcome colors.")
  }
  
  main_color <- "#004B5A"
  legend.cex <- 0.7
  legend.title.cex <- 0.75
  
  # ----- Facet 1: bars colored by batch (legend = Batch) -----
  plot.v2 <- ggplot(
    study.summary, aes(x = batch_id, y = proportion, group = .outcome, fill = batch_id)
  ) +
    facet_grid(cols = vars(.outcome), scales = "free", space = "free_x", drop = TRUE) +
    geom_bar(stat = "identity", width = 0.9) +
    guides(fill = guide_legend(title = "Batch", keywidth = 1, keyheight = 1)) +
    scale_fill_manual(values = batchCols) +
    ylab("Proportion of all observations") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(colour = main_color, size = 12),
          axis.ticks = element_blank(),
          axis.line = element_line(color = "#7F7F7F"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = rel(1), angle = 90),
          legend.position = 'bottom', legend.box = 'horizontal',
          legend.direction = 'horizontal',
          legend.key.height = unit(0.2, 'cm'),
          legend.key.width  = unit(0.1, 'cm'),
          legend.title = element_text(size = rel(legend.title.cex)),
          legend.spacing.x = unit(0.1, 'cm'),
          legend.spacing.y = unit(0.1, 'cm'),
          legend.text  = element_text(size = rel(legend.cex)),
          plot.margin = unit(c(0.2, 0.2, 0.05, 0.2), "cm"))
  
  # ----- Facet 2: bars colored by outcome (legend = Outcome) -----
  plot.v1 <- ggplot(
    study.summary, aes(x = .outcome, y = proportion, fill = .outcome)
  ) +
    facet_grid(cols = vars(batch_id), scales = "free", space = "free_x", drop = TRUE) +
    geom_bar(stat = "identity", width = 0.9) +
    guides(fill = guide_legend(title = OUTCOME_TITLE, keywidth = 1, keyheight = 1)) +
    scale_fill_manual(values = outcomeCols, breaks = names(outcomeCols)) +
    ylab("Proportion of all observations") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(colour = main_color, size = 12),
          axis.ticks = element_blank(),
          axis.line = element_line(color = "#7F7F7F"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = rel(1), angle = 90),
          legend.position = 'bottom', legend.box = 'horizontal',
          legend.direction = 'horizontal',
          legend.key.height = unit(0.2, 'cm'),
          legend.key.width  = unit(0.1, 'cm'),
          legend.title = element_text(size = rel(legend.title.cex)),
          legend.spacing.x = unit(0.1, 'cm'),
          legend.spacing.y = unit(0.1, 'cm'),
          legend.text  = element_text(size = rel(legend.cex)),
          plot.margin = unit(c(0.05, 0.2, 0.2, 0.2), "cm"))
  
  # Legend extractor
  g_legend <- function(a.gplot) {
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    tmp$grobs[[which(vapply(tmp$grobs, function(x) x$name, FUN.VALUE = character(1)) == "guide-box")]]
  }
  legend.v2 <- g_legend(plot.v2)
  legend.v1 <- g_legend(plot.v1)
  
  gridExtra::arrangeGrob(
    plot.v2 + theme(legend.position = "none"),
    plot.v1 + theme(legend.position = "none"),
    gridExtra::arrangeGrob(legend.v1, legend.v2, ncol = 2, nrow = 1),
    ncol = 1, nrow = 3, widths = c(1), heights = c(1, 1, 0.2), padding = -10
  )
}

# ==== Plot the Mosaic Plot ====
plot.mosaic <- mbecMosaicPlot(study.summary = mosaic_data, model.vars = c('batch_id', '.outcome'))
ggsave(file.path(output_folder, "mosaic_plot.png"), plot = plot.mosaic, width = 12, height = 8, dpi = 300)
ggsave(file.path(output_folder, "mosaic_plot.tif"), plot = plot.mosaic, width = 12, height = 8, dpi = 300, compression = "lzw")
