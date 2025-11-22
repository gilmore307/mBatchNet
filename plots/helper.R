# Shared helper functions for plotting scripts

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
    qn = "Quantile Normalization",
    bmc = "BMC",
    limma = "Limma",
    conqur = "ConQuR",
    plsda = "PLSDA-batch",
    combat = "ComBat",
    fsqn = "FSQN",
    mmuphin = "MMUPHin",
    ruv = "RUV-III-NB",
    metadict = "MetaDICT",
    pn = "Percentile Normalization",
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
