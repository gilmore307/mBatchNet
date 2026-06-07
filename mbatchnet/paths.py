from pathlib import Path


BASE_DIR = Path(__file__).resolve().parents[1]
ASSETS_DIR = BASE_DIR / "assets"
EXAMPLE_DIR = ASSETS_DIR / "example"
OUTPUT_ROOT = BASE_DIR / "output"
PLOTS_DIR = BASE_DIR / "plots"
CORRECTION_DIR = BASE_DIR / "correction"
METHODS_DIR = CORRECTION_DIR / "methods"
METHODS_REFERENCE_PATH = ASSETS_DIR / "doc" / "methods.csv"
PREPROCESS_SCRIPT = CORRECTION_DIR / "preprocess.R"

