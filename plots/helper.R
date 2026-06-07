# Shared helper functions for plotting scripts

extract_plot_legend <- function(plot) {
  grob <- ggplotGrob(plot)
  guide_idx <- which(vapply(grob$grobs, function(x) x$name, character(1)) == "guide-box")
  if (length(guide_idx)) grob$grobs[[guide_idx[1]]] else grid::nullGrob()
}

assemble_panel_grid <- function(plot_list, panel_cols, panel_rows, fig_dims, title, legend_grob) {
  panel_gap_x_in <- fig_dims$width / 100
  panel_gap_y_in <- fig_dims$height / 100
  total_cols <- panel_cols * 2L - 1L
  total_rows <- panel_rows * 2L - 1L
  widths <- rep(grid::unit(1, "null"), total_cols)
  heights <- rep(grid::unit(1, "null"), total_rows)
  if (total_cols > 1L) {
    widths[seq(2, total_cols - 1L, by = 2L)] <- grid::unit(panel_gap_x_in, "in")
  }
  if (total_rows > 1L) {
    heights[seq(2, total_rows - 1L, by = 2L)] <- grid::unit(panel_gap_y_in, "in")
  }
  grid_plots <- vector("list", total_cols * total_rows)
  plot_idx <- 1L
  for (row in seq_len(total_rows)) {
    for (col in seq_len(total_cols)) {
      list_idx <- (row - 1L) * total_cols + col
      if (row %% 2L == 0L || col %% 2L == 0L) {
        grid_plots[[list_idx]] <- plot_spacer()
      } else if (plot_idx <= length(plot_list)) {
        grid_plots[[list_idx]] <- plot_list[[plot_idx]]
        plot_idx <- plot_idx + 1L
      } else {
        grid_plots[[list_idx]] <- plot_spacer()
      }
    }
  }
  panel_grob <- patchwork::patchworkGrob(
    wrap_plots(grid_plots, ncol = total_cols) +
      plot_layout(widths = widths, heights = heights)
  )
  title_grob <- grid::textGrob(title, gp = grid::gpar(fontsize = 20, fontface = "bold"))
  if (is.null(legend_grob)) legend_grob <- grid::nullGrob()
  gridExtra::arrangeGrob(
    title_grob,
    panel_grob,
    legend_grob,
    ncol = 1,
    heights = grid::unit.c(grid::unit(0.35, "in"), grid::unit(1, "null"), grid::unit(0.45, "in"))
  )
}

# Create a PNG thumbnail from a TIFF figure at the desired width (in pixels).
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

# Map method codes from filenames to short display labels for figures.
method_short_label <- function(x) {
  map <- c(
    bmc = "BMC",
    limma = "Limma",
    conqur = "ConQuR",
    plsda = "PLSDA-batch",
    combat = "ComBat",
    fsqn = "FSQN",
    mmuphin = "MMUPHin",
    ruv = "RUV-III-NB",
    metadict = "MetaDICT",
    fabatch = "FAbatch",
    combatseq = "ComBat-seq",
    debias = "DEBIAS-M"
  )
  sapply(x, function(v) {
    lv <- tolower(v)
    if (lv %in% names(map)) map[[lv]] else v
  })
}

# Derive method names from normalized filenames with a suffix (e.g., _clr.csv).
name_from <- function(paths, suffix) {
  gsub(paste0("^normalized_|_", suffix, "\\.csv$"), "", basename(paths))
}
