# Shared helper functions for plotting scripts

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
