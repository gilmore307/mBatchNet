#!/usr/bin/env Rscript
# Install the R package stack required by the Batch Effect Correction app.

options(repos = c(CRAN = "https://cloud.r-project.org"))

cat("\n===============================================\n")
cat("Batch Effect Correction R dependency installer\n")
cat("===============================================\n\n")

install_if_missing <- function(pkgs, install_fun, ..., quiet = FALSE) {
  pkgs <- unique(pkgs)
  pkgs <- pkgs[nzchar(pkgs)]
  installed <- rownames(installed.packages())
  missing <- setdiff(pkgs, installed)
  if (!length(missing)) {
    if (!quiet) cat("✔ All", length(pkgs), "packages already installed.\n")
    return(invisible(FALSE))
  }
  cat("→ Installing", length(missing), "package(s):", paste(missing, collapse = ", "), "\n")
  install_fun(missing, ...)
  invisible(TRUE)
}

cran_pkgs <- c(
  "jsonlite",       # session summaries
  "doParallel",     # ConQuR parallel backend
  "reticulate",     # Python bridge for DEBIAS
  "pamr",           # Batch mean centering
  "vegan",          # MetaDICT distance metrics
  "bapred",         # FAbatch implementation
  "ggplot2",        # plotting backbone
  "readr",          # CSV ingest helpers
  "dplyr",          # data wrangling
  "tidyr",          # reshaping utilities
  "tibble",         # tidy tabular frames
  "gridExtra",      # tableGrob utilities
  "gtable",         # table layout tweaks
  "patchwork",      # multi-plot layouts
  "rlang",          # tidy evaluation helpers
  "scales",         # axis formatting
  "FNN",            # nearest neighbour metrics
  "purrr",          # functional helpers
  "uwot",           # UMAP embeddings
  "cluster",        # silhouette widths
  "caret",          # AUC benchmarking
  "randomForest",   # AUC benchmarking
  "pROC",           # ROC calculations
  "forcats",        # factor releveling
  "lme4"            # PVCA mixed models
)

bioc_pkgs <- c(
  "preprocessCore",  # Quantile normalisation
  "limma",           # limma/ComBat
  "sva",             # ComBat-Seq + sva utils
  "MMUPHin",         # MMUPHin correction
  "TreeSummarizedExperiment", # example dataset container
  "mixOmics"         # example preprocessing helpers
)

github_pkgs <- c(
  "wdl2459/ConQuR",       # ConQuR implementation
  "jenniferfranks/FSQN",  # FSQN normalisation
  "limfuxing/ruvIIInb",   # fast RUV-III-NB
  "EvaYiwenWang/PLSDAbatch", # PLSDA-batch
  "BoYuan07/MetaDICT"     # MetaDICT toolkit
)

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

install_if_missing(cran_pkgs, install.packages)
install_if_missing(bioc_pkgs, BiocManager::install, ask = FALSE, update = FALSE)

for (repo in github_pkgs) {
  pkg <- sub(".*/", "", repo)
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("→ Installing GitHub package", repo, "\n")
    remotes::install_github(repo, upgrade = "never")
  }
}

cat("\nR package installation attempted. Review messages above for any warnings.\n")
cat("Missing system libraries? Install build tools (e.g. libgit2, BLAS/LAPACK, Fortran).\n")
