# Shared plotting helpers

# Map method codes to short labels for figures
method_short_label <- function(x) {
  map <- c(
    qn = "Quantile Normalization", bmc = "BMC", limma = "Limma", conqur = "ConQuR",
    plsda = "PLSDA-batch", combat = "ComBat", fsqn = "FSQN", mmuphin = "MMUPHin",
    ruv = "RUV-III-NB", metadict = "MetaDICT", pn = "Percentile Normalization",
    fabatch = "FAbatch", combatseq = "ComBat-seq", debias = "DEBIAS-M"
  )
  sapply(
    x,
    function(v) {
      lv <- tolower(v)
      if (lv %in% names(map)) map[[lv]] else v
    },
    USE.NAMES = FALSE
  )
}

apply_fig_overrides <- function(width_in, height_in, default_dpi = 300,
                               panel_cols = 1, panel_rows = 1,
                               max_total_pixels = 35e6,
                               opt_fig_width_px = get0("opt_fig_width_px", envir = parent.frame(), ifnotfound = NA_real_),
                               opt_fig_height_px = get0("opt_fig_height_px", envir = parent.frame(), ifnotfound = NA_real_),
                               opt_fig_dpi = get0("opt_fig_dpi", envir = parent.frame(), ifnotfound = NA_real_),
                               opt_fig_per_panel = get0("opt_fig_per_panel", envir = parent.frame(), ifnotfound = FALSE)) {
  dpi <- if (is.na(opt_fig_dpi) || opt_fig_dpi <= 0) default_dpi else opt_fig_dpi
  w <- width_in
  h <- height_in
  panel_cols <- max(1L, as.integer(panel_cols))
  panel_rows <- max(1L, as.integer(panel_rows))

  if (!is.na(opt_fig_width_px) && opt_fig_width_px > 0 && dpi > 0) {
    per_panel <- opt_fig_width_px / dpi
    if (isTRUE(opt_fig_per_panel)) {
      w <- per_panel * panel_cols
    } else {
      w <- per_panel
    }
  }
  if (!is.na(opt_fig_height_px) && opt_fig_height_px > 0 && dpi > 0) {
    per_panel <- opt_fig_height_px / dpi
    if (isTRUE(opt_fig_per_panel)) {
      h <- per_panel * panel_rows
    } else {
      h <- per_panel
    }
  }

  width_px <- w * dpi
  height_px <- h * dpi
  total_px <- width_px * height_px
  if (is.finite(total_px) && is.finite(max_total_pixels) && max_total_pixels > 0 &&
      total_px > max_total_pixels && is.finite(width_px) && is.finite(height_px) &&
      width_px > 0 && height_px > 0) {
    scale <- sqrt(max_total_pixels / total_px)
    w <- w * scale
    h <- h * scale
    width_px <- w * dpi
    height_px <- h * dpi
    message(sprintf(
      "Requested canvas %.0f×%.0f px exceeds limit %.0f; scaling by %.3f to %.0f×%.0f px",
      width_px / scale, height_px / scale, max_total_pixels, scale, width_px, height_px
    ))
  }
  list(width = w, height = h, dpi = dpi)
}

compute_png_dims <- function(fig_dims, max_width_px = 1400) {
  width_px <- fig_dims$width * fig_dims$dpi
  height_px <- fig_dims$height * fig_dims$dpi
  if (!is.finite(width_px) || width_px <= 0 ||
      !is.finite(height_px) || height_px <= 0) {
    return(list(width = max_width_px, height = max_width_px))
  }
  target_width <- min(max_width_px, width_px)
  scale <- target_width / width_px
  list(width = target_width, height = height_px * scale)
}
