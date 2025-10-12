#!/usr/bin/env Rscript
# Install the R package stack required by the Batch Effect Correction app.

options(repos = c(CRAN = "https://cloud.r-project.org"))
# Try to speed up source builds
try({ ncpu <- parallel::detectCores(logical = TRUE); if (!is.na(ncpu) && ncpu > 1) Sys.setenv(MAKEFLAGS = paste0("-j", ncpu)) }, silent = TRUE)

msg <- function(...) cat(sprintf(...), "\n")
rule <- function(t) { cat("\n", paste(rep("=", 55), collapse=""), "\n", t, "\n", paste(rep("=", 55), collapse=""), "\n\n", sep="") }

rule("Batch Effect Correction R dependency installer")

install_if_missing <- function(pkgs, install_fun, ..., quiet = FALSE) {
  pkgs <- unique(pkgs)
  pkgs <- pkgs[nzchar(pkgs)]
  installed <- rownames(installed.packages())
  missing <- setdiff(pkgs, installed)
  if (!length(missing)) {
    if (!quiet) msg("✔ All %d packages already installed.", length(pkgs))
    return(invisible(character(0)))
  }
  msg("→ Installing %d package(s): %s", length(missing), paste(missing, collapse = ", "))
  install_fun(missing, ...)
  invisible(missing)
}

# ---- CRAN packages (core + graphics stack) ----
cran_core <- c(
  "jsonlite","doParallel","reticulate","pamr","vegan","bapred",
  "ggplot2","readr","dplyr","tidyr","tibble","gridExtra","gtable",
  "patchwork","rlang","scales","FNN","purrr","uwot","cluster",
  "caret","randomForest","pROC","forcats","lme4"
)

# graphics/plotting toolchain needed by ragg/Cairo/ruvIIInb deps
cran_graphics <- c("systemfonts","textshaping","ragg","Cairo","tidyverse","hrbrthemes")

cran_pkgs <- unique(c(cran_core, cran_graphics))

# ---- Bioconductor packages ----
bioc_pkgs <- c(
  "preprocessCore","limma","sva","MMUPHin",
  "TreeSummarizedExperiment","mixOmics",
  "scater","SummarizedExperiment","SingleCellExperiment","S4Vectors"
)

# ---- GitHub packages ----
github_pkgs <- c(
  "wdl2459/ConQuR",
  "jenniferfranks/FSQN",
  "limfuxing/ruvIIInb",
  "EvaYiwenWang/PLSDAbatch",
  "BoYuan07/MetaDICT"
)

# Ensure helpers
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("remotes",     quietly = TRUE)) install.packages("remotes")

rule("Installing CRAN packages")
miss_cran <- install_if_missing(cran_pkgs, install.packages, dependencies = TRUE)

rule("Installing Bioconductor packages")
miss_bioc <- install_if_missing(bioc_pkgs, BiocManager::install, ask = FALSE, update = FALSE)

rule("Installing GitHub packages")
for (repo in github_pkgs) {
  pkg <- sub(".*/", "", repo)
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg("→ Installing GitHub package %s", repo)
    remotes::install_github(repo, upgrade = "never", build_vignettes = FALSE)
  } else {
    msg("✔ %s already installed", pkg)
  }
}

# Summary
rule("Summary")
installed <- rownames(installed.packages())
wanted <- unique(c(cran_pkgs, bioc_pkgs, sub(".*/", "", github_pkgs)))
missing <- setdiff(wanted, installed)

if (length(missing)) {
  msg("⚠ The following packages are still missing (likely system libs needed):")
  msg("  %s", paste(missing, collapse = ", "))
  msg("\nIf you see build errors about cairo/fonts, ensure these system libs are present:")
  msg("  libcairo2-dev libxt-dev pkg-config libfontconfig1-dev libfreetype6-dev")
  msg("  libharfbuzz-dev libfribidi-dev libpng-dev libjpeg-dev libtiff5-dev")
} else {
  msg("✅ All requested packages installed.")
}

invisible(NULL)
