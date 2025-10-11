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

missing_python_requirements() {
  "$PY_BIN" - <<'PY' "$REQ_FILE"
import sys
from pathlib import Path

try:
    import pkg_resources
except ModuleNotFoundError:
    sys.exit(1)

req_path = Path(sys.argv[1])
lines = []
for raw in req_path.read_text().splitlines():
    stripped = raw.strip()
    if stripped and not stripped.startswith("#"):
        lines.append(stripped)

if not lines:
    sys.exit(0)

ws = pkg_resources.WorkingSet()
missing = []
for req in pkg_resources.parse_requirements(lines):
    if ws.find(req) is None:
        missing.append(str(req))

if missing:
    print("\n".join(missing))
PY
}

if ! missing_pkgs=$(missing_python_requirements); then
  echo "→ pkg_resources unavailable; installing all Python requirements" >&2
  missing_pkgs=$( (grep -v '^#' "$REQ_FILE" | sed '/^$/d') || true )
fi

if [ -n "$missing_pkgs" ]; then
  echo "→ Installing Python packages:" >&2
  while IFS= read -r line; do
    echo "   $line" >&2
  done <<< "$missing_pkgs"
  tmp_req="$(mktemp)"
  printf '%s\n' "$missing_pkgs" > "$tmp_req"
  "$PY_BIN" -m pip install -r "$tmp_req"
  rm -f "$tmp_req"
else
  echo "✔ All Python requirements already satisfied."
fi

# ---- R packages -----------------------------------------------------------
"$R_BIN" "$R_SCRIPT"

cat <<'NOTE'

Environment setup complete.
Next steps:
  • Ensure RETICULATE_PYTHON points at the Python interpreter used above, e.g.
      export RETICULATE_PYTHON="$($PY_BIN -c 'import sys; print(sys.executable)')"
  • Review env/README.md for platform-specific system library prerequisites.
NOTE
