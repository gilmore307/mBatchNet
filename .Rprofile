## Guard against accidental Rplots.pdf in non-interactive runs
if (!interactive()) {
  nullfile <- if (.Platform$OS.type == "windows") "NUL" else "/dev/null"
  options(device = function(...) grDevices::pdf(file = nullfile))
  # Close any pre-opened default devices
  try({
    while (grDevices::dev.cur() > 1L) grDevices::dev.off()
  }, silent = TRUE)
}

