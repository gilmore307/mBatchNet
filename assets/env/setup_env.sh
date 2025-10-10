#!/usr/bin/env bash
# Usage: bash assets/setup_env.sh [/path/to/python] [/path/to/Rscript]
set -euo pipefail

PY_BIN="${1:-python3}"
R_BIN="${2:-Rscript}"

echo "[Python] Using interpreter: $PY_BIN"
echo "[R]      Using Rscript   : $R_BIN"

# ---- Python packages (Dash app + reticulate side) ----
"$PY_BIN" -m pip install --upgrade pip
if [ -f "assets/source/requirements.txt" ]; then
  "$PY_BIN" -m pip install -r assets/source/requirements.txt
else
  # Fallback minimal install if requirements.txt is missing
  "$PY_BIN" -m pip install dash dash-bootstrap-components gunicorn numpy DEBIAS-M
fi

echo "[R] Installing CRAN/Bioconductor/GitHub packages ..."
"$R_BIN" - <<'RSCRIPT'
options(repos = c(CRAN = "https://cloud.r-project.org"))

install_if_missing <- function(pkgs, install_fun, ...) {
  pkgs <- unique(pkgs)
  pkgs <- pkgs[nzchar(pkgs)]
  missing <- setdiff(pkgs, rownames(installed.packages()))
  if (length(missing)) install_fun(missing, ...)
}

cran_pkgs <- c(
  # core
  "dplyr","Matrix","ggplot2","readr","tidyr","rlang","tibble",
  # plotting/utilities
  "gridExtra","gtable","patchwork","ggrepel","scales","purrr","forcats",
  # algorithms
  "FNN","caret","randomForest","pROC","uwot","cluster","huge",
  # misc
  "doParallel","reticulate","GUniFrac","pamr","lme4","vegan"
)

bioc_pkgs <- c(
  "SummarizedExperiment","S4Vectors",
  "sva","edgeR","DESeq2","metagenomeSeq",
  "preprocessCore","limma","MMUPHin"
)

# GitHub-only (or more reliable from GitHub)
# GitHub-only per your mapping
github_pkgs <- c(
  # ConQuR
  "wdl2459/ConQuR",
  # FSQN
  "jenniferfranks/FSQN",
  # RUV-III-NB
  "limfuxing/ruvIIInb",
  # PLSDA-batch
  "EvaYiwenWang/PLSDAbatch",
  # MetaDICT
  "BoYuan07/MetaDICT"
)

if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")

install_if_missing(cran_pkgs, install.packages)
install_if_missing(bioc_pkgs, BiocManager::install, ask = FALSE, update = FALSE)

for (repo in github_pkgs) {
  pkg <- sub(".*/", "", repo)
  if (!requireNamespace(pkg, quietly = TRUE)) {
    remotes::install_github(repo, upgrade = "never")
  }
}

cat("\n[R] Package installation attempted. If any package failed, install system libs and retry.\n")
RSCRIPT

cat <<'NOTE'

Done. Notes:
- If running on Linux, some packages may require system dependencies. Example (Ubuntu):
  sudo apt-get update && sudo apt-get install -y \
    build-essential gfortran libxml2-dev libcurl4-openssl-dev libssl-dev \
    libgit2-dev libgsl0-dev libfftw3-dev libopenblas-dev liblapack-dev \
    libharfbuzz-dev libfribidi-dev

- To let R reticulate use this Python, export RETICULATE_PYTHON to the same interpreter:
  export RETICULATE_PYTHON="$($PY_BIN -c 'import sys; print(sys.executable)')"

NOTE

echo "Environment setup complete."
