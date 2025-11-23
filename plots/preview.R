# Generate PNG previews for all TIFF figures in the given directory.
suppressPackageStartupMessages({
  library(magick)
})

source("plots/helper.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  args <- "output/example"  # default folder for quick runs
}

output_folder <- args[1]
if (!dir.exists(output_folder)) {
  stop(sprintf("Output directory does not exist: %s", output_folder))
}

width_px <- 2000
for (a in args[-1]) {
  if (grepl("^--width=", a)) {
    parsed <- suppressWarnings(as.numeric(sub("^--width=", "", a)))
    if (is.finite(parsed) && parsed > 0) {
      width_px <- parsed
    }
  }
}

message(sprintf("Creating PNG previews in %s (width %d px)...", output_folder, width_px))

tif_files <- list.files(
  output_folder,
  pattern = "\\.tif{1,2}$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (!length(tif_files)) {
  message("No TIFF figures found; nothing to preview.")
} else {
  for (tif in tif_files) {
    create_png_thumbnail(tif, width_px = width_px)
  }
  message(sprintf("Generated %d previews.", length(tif_files)))
}
