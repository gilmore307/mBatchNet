# Shared typography settings for project plots

# define typography constants in a single place so every script can apply
# consistent sizing and weights for axes, titles, and legends.
plot_axis_text_size     <- 9
plot_axis_title_size    <- 11
plot_subplot_title_size <- 12
plot_overall_title_size <- 14
plot_legend_title_size  <- 10
plot_legend_text_size   <- 9

# helper that returns the directory of the currently running script
plot_script_dir <- function() {
  args_full <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  file_idx <- grep(file_arg, args_full)
  if (length(file_idx)) {
    return(dirname(normalizePath(sub(file_arg, "", args_full[file_idx[1]]))))
  }
  # fallback for interactive sessions (e.g. RStudio)
  frame_files <- vapply(sys.frames(), function(fr) fr$ofile %||% NA_character_, character(1))
  frame_files <- frame_files[!is.na(frame_files)]
  if (length(frame_files)) {
    return(dirname(normalizePath(frame_files[[length(frame_files)]])))
  }
  # final fallback: assume working directory already set to repository root
  return(normalizePath("."))
}

`%||%` <- function(lhs, rhs) if (!is.null(lhs)) lhs else rhs

# reusable themes -------------------------------------------------------------

theme_plot_axes <- function() {
  ggplot2::theme(
    axis.title = ggplot2::element_text(size = plot_axis_title_size, face = "bold"),
    axis.text  = ggplot2::element_text(size = plot_axis_text_size)
  )
}

theme_plot_legend <- function() {
  ggplot2::theme(
    legend.title = ggplot2::element_text(size = plot_legend_title_size, face = "bold"),
    legend.text  = ggplot2::element_text(size = plot_legend_text_size)
  )
}

theme_plot_title <- function(hjust = 0.5) {
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      size = plot_subplot_title_size,
      face = "bold",
      hjust = hjust
    ),
    plot.subtitle = ggplot2::element_text(
      size = plot_axis_title_size,
      face = "plain",
      hjust = hjust
    )
  )
}

theme_plot_strip <- function() {
  ggplot2::theme(
    strip.text = ggplot2::element_text(size = plot_subplot_title_size, face = "bold"),
    strip.background = ggplot2::element_rect(fill = "#f5f5f5", colour = NA)
  )
}

theme_plot_overall_title <- function(hjust = 0.5) {
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      size = plot_overall_title_size,
      face = "bold",
      hjust = hjust
    )
  )
}

theme_plot_common <- function() {
  theme_plot_axes() + theme_plot_legend() + theme_plot_title()
}

