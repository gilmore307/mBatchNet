#!/usr/bin/env bash
# Bootstrap the Batch Effect Correction app environment.
# Usage:
#   ./env/setup.sh [python-bin] [rscript-bin]
# Example:
#   ./env/setup.sh python3.11 Rscript

set -euo pipefail

# Resolve repository root (directory containing this script resides in env/).
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. && pwd)"
PY_BIN="${1:-python3}"
R_BIN="${2:-Rscript}"
REQ_FILE="$ROOT_DIR/env/requirements.txt"
R_SCRIPT="$ROOT_DIR/env/r-packages.R"

if [ ! -f "$REQ_FILE" ]; then
  echo "[error] Cannot find Python requirements at $REQ_FILE" >&2
  exit 1
fi
if [ ! -f "$R_SCRIPT" ]; then
  echo "[error] Cannot find R package installer at $R_SCRIPT" >&2
  exit 1
fi

cat <<INFO
────────────────────────────────────────────
Batch Effect Correction environment bootstrap
Python interpreter : $PY_BIN
Rscript executable : $R_BIN
Requirements file  : $REQ_FILE
R installer script : $R_SCRIPT
────────────────────────────────────────────
INFO

# ---- Python packages ------------------------------------------------------
"$PY_BIN" -m pip install --upgrade pip
"$PY_BIN" -m pip install -r "$REQ_FILE"

# ---- R packages -----------------------------------------------------------
"$R_BIN" "$R_SCRIPT"