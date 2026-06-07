# mBatchNet

## Project Overview
mBatchNet is a browser-based application for batch-effect correction and post-correction evaluation in microbiome/omics count or abundance analysis workflows.

The app supports an end-to-end pipeline:
1. Upload count matrix and metadata
2. Run preprocessing and selected batch-correction methods
3. Run pre-/post-correction visual and statistical assessments
4. Download a packaged result bundle

The current server entry point is `server.py`. It uses Flask/Jinja for the web workflow, keeps the Bootstrap visual style, and runs analysis through explicit backend services under `mbatchnet/`.

## Installation

### 1) Prerequisites
- Python 3.10+ (3.11/3.12 recommended)
- R 4.2+
- `Rscript` available in PATH

### 2) Install Python dependencies
From the repository root:

```bash
python3 -m venv .venv
.venv/bin/python -m pip install -r requirements.txt
```

### 3) Install R dependencies (recommended)
Use the provided bootstrap script:

```bash
bash assets/env/setup.sh .venv/bin/python Rscript
```

This script will:
- Install Python packages from `assets/env/requirements.txt`
- Install R packages via `assets/env/r-packages.R`

If Python dependencies are already installed from the root `requirements.txt`, you can install only R dependencies with:

```bash
Rscript assets/env/r-packages.R
```

### 4) Optional DEBIAS-M Python dependency
The DEBIAS-M method depends on PyTorch and can install a large machine-learning stack. Install it only when that method is needed:

```bash
.venv/bin/python -m pip install -r assets/env/requirements-debias.txt
```

## Dependency Notes

### Python
Core packages:
- `flask`
- `gunicorn`
- `numpy`
- `pandas`
- `Pillow`

See full package list in `requirements.txt`.

### R
Batch-correction and plotting scripts require multiple R packages. Install them with:

```bash
Rscript assets/env/r-packages.R
```

## Quick Start
1. Install dependencies (sections above).
2. Start the app:

```bash
.venv/bin/python server.py
```

3. Open the app in your browser:
   - `http://127.0.0.1:8050` (default on Windows)
   - On Linux/macOS, the app binds to `0.0.0.0:8050` by default.

4. Follow the navigation flow:
   - Upload Data
   - Pre-assessment
   - Batch Correction
   - Post-assessment
   - Download results

## Input / Output Specification

### Inputs
The Upload page expects two CSV files:

1. **Count matrix (CSV)**
   - Rows: samples
   - Columns: features (e.g., OTU/ASV/genes)

2. **Metadata (CSV)**
   - Must include at least:
     - batch column (Batch)
     - target column (target/phenotype/group)
   - Optional:
     - covariate columns

After upload, map batch/target/covariate columns in the UI.

### Outputs
For each session, intermediate and final outputs are generated under `output/<session_id>/`, including:
- preprocessed and corrected matrices
- assessment figures (PCA/PCoA/NMDS/PERMANOVA/PVCA, etc.)
- run log (`run.log`)
- output summary (`output_summary.json`)
- validation report (`validation_report.json`)
- downloadable output and reproducibility bundles

## Test Data Links
Bundled example datasets are available in `assets/example/` and can be loaded directly from the **Example Dataset** tab.

- AD example:
  - Count matrix: [`assets/example/raw_ad.csv`](assets/example/raw_ad.csv)
  - Metadata: [`assets/example/metadata_ad.csv`](assets/example/metadata_ad.csv)

Additional bundled RData examples:
- [`assets/example/data_bladderbatch.R`](assets/example/data_bladderbatch.R)
- [`assets/example/data_cmgd.R`](assets/example/data_cmgd.R)
- [`assets/example/data_conqur.R`](assets/example/data_conqur.R)
- [`assets/example/data_metadict.R`](assets/example/data_metadict.R)
- [`assets/example/data_plsda.R`](assets/example/data_plsda.R)
