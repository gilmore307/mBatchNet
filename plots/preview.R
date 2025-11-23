suppressPackageStartupMessages({
  library(magick)
  library(tools)
})

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) {
  stop("Usage: preview.R <results_dir> [preview_dir]")
}

results_dir <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
preview_dir <- if (length(args) >= 2) args[[2]] else file.path(dirname(results_dir), "preview")

dir.create(preview_dir, recursive = TRUE, showWarnings = FALSE)

tifs <- list.files(results_dir, pattern = "\\.(tif|tiff)$", ignore.case = TRUE, full.names = TRUE)
if (!length(tifs)) {
  message("No TIFF files to convert.")
  quit(save = "no", status = 0)
}

max_side <- 1400

scale_image <- function(img, max_side) {
  info <- image_info(img)
  if (!NROW(info)) return(img)
  w <- info$width[1]
  h <- info$height[1]
  if (is.na(w) || is.na(h)) return(img)
  longest <- max(w, h)
  if (longest <= max_side) return(img)
  if (w >= h) {
    return(image_scale(img, paste0(max_side)))
  }
  new_width <- round(w * (max_side / h))
  image_scale(img, paste0(new_width, "x", max_side, "!"))
}

for (tif_path in tifs) {
  png_path <- file.path(preview_dir, paste0(file_path_sans_ext(basename(tif_path)), ".png"))
  needs_update <- TRUE
  if (file.exists(png_path)) {
    tif_m <- file.info(tif_path)$mtime
    png_m <- file.info(png_path)$mtime
    if (!is.na(png_m) && !is.na(tif_m) && png_m >= tif_m) {
      needs_update <- FALSE
    }
  }
  if (!needs_update) next

  tryCatch({
    img <- image_read(tif_path)
    img <- scale_image(img, max_side)
    image_write(img, path = png_path, format = "png")
  }, error = function(e) {
    warning(sprintf("Failed to create preview for %s: %s", tif_path, e$message))
  })
}
