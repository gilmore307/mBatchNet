# mBatchNet

## Project Overview
mBatchNet is a browser-based application for batch-effect correction and post-correction evaluation in microbiome/omics count or abundance analysis workflows.

The app supports an end-to-end pipeline:
1. Upload count matrix and metadata
2. Run preprocessing and selected batch-correction methods
3. Run pre-/post-correction visual and statistical assessments
4. Download an output bundle or a reproducibility bundle

The current app is the Dash interface. The primary implementation lives in `_0_main.py` and the numbered page modules (`_1_components.py` through `_7_description.py`). `server.py` is a compatibility entry point that imports and serves the same Dash app.

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
- `dash`
- `dash-bootstrap-components`
- `dash-ag-grid`
- `flask-sock`
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
   - Download outputs or Repro bundle

The Dash app can also be started directly with:

```bash
.venv/bin/python _0_main.py
```

## Input / Output Specification

### Inputs
The Upload page expects two CSV files:

1. **Feature table / count matrix (CSV)**
   - Rows: samples
   - Columns: profiled features (for example OTU/ASV/gene/pathway columns)
   - Raw sequencing reads are not accepted; upload a numeric sample-by-feature table that has already been generated upstream.

2. **Metadata (CSV)**
   - Must include at least:
     - batch column (Batch)
     - target column (target/phenotype/group)
   - Optional:
     - covariate columns

After upload, map batch/target/covariate columns in the UI.

Public server limits:
- CSV size: 10.0 MB per uploaded CSV
- Samples: 1,000
- Features: 1,000
- Matrix cells: 1,000,000
- Metadata columns: 5 or fewer, including batch, target, and optional covariates

## Correction Methods and Parameters

Method descriptions in the app are loaded from `assets/doc/methods.csv`, which also stores package/source links, citation text, and reference URLs. The exposed parameter list below mirrors the current Correction page controls.

Methods without method-specific controls still use the uploaded matrix, metadata mapping, selected covariates, reference batch, and target/control settings from the session.

### DEBIAS-M
DEBIAS-M treats processing protocols, studies, or batches as domains for microbiome count or relative-abundance profiles. It learns taxon- and batch-specific multiplicative coefficients together with phenotype-prediction parameters and returns corrected count-derived TSS and CLR outputs.

Exposed parameters: none.

### MetaDICT
MetaDICT performs microbiome data integration through initial batch-effect estimation by covariate balancing and refinement by shared dictionary learning. In mBatchNet it receives non-negative feature counts or abundances, metadata batch labels, optional covariates, and returns a corrected feature table aligned to the uploaded samples and features.

Exposed parameters:
- `alpha` (default: `0.05`): significance threshold for detecting batch-driven structure.
- `beta` (default: `0.2`): effect-size threshold for adjustment strength.
- `normalization` (default: `uq`): normalization strategy inside MetaDICT; options are upper quartile, TMM, total sum scaling, or none.
- `max_iter` (default: `2000`): maximum optimization iterations.

### PLSDA-batch
PLSDA-batch uses partial least-squares discriminant analysis latent components to estimate treatment-associated and batch-associated variation in microbiome data. The correction subtracts latent components associated with batch from the feature matrix.

Exposed parameters:
- `ncomp.trt` (default: `1`): number of latent treatment components retained.
- `ncomp.bat` (default: `5`): number of latent batch components modeled.
- `keepX.trt` (default: `50`): number of variables retained for treatment/target discrimination.
- `near.zero.var` (default: `False`): near-zero variance feature filtering before model fitting.
- `balance` (default: `False`): class balancing during model fitting.

### ConQuR
ConQuR removes microbiome batch effects with conditional quantile regression for zero-inflated read-count data. Its cited model separates zero occurrence from non-zero abundance and generates batch-removed zero-inflated counts.

Exposed parameters:
- `logistic_lasso` (default: `False`): lasso logistic regression in the batch-modeling step.
- `quantile_type` (default: `standard`): quantile-regression mode passed to the ConQuR wrapper.
- `lambda_quantile` (default: `2p/n`): penalty expression for the quantile model.
- `interplt` (default: `False`): interpolation in the quantile adjustment.
- `delta` (default: `0.4999`): quantile clipping offset.
- `taus` (default: `seq(0.05,0.95,0.05)`): quantile grid.

### MMUPHin
MMUPHin is a statistical framework for meta-analysis of microbial community studies using taxonomic, functional, or other abundance profiles. In mBatchNet it uses the package batch-adjustment component on a feature-abundance table with batch labels and selected covariates.

Exposed parameters:
- `zero_inflation` (default: `False`): MMUPHin zero-inflation control path.
- `conv` (default: `0.000001`): convergence tolerance.

### RUV-III-NB
RUV-III-NB normalizes sequencing count data by modeling counts with negative binomial or zero-inflated negative binomial distributions while estimating unwanted factors. In mBatchNet it receives rounded non-negative count data with a batch design matrix.

Exposed parameters:
- `k` (default: `2`): number of unwanted factors estimated.
- `use.pseudosample` (default: `False`): pseudo-sample controls.
- `batch.disp` (default: `False`): batch-specific dispersion estimation.
- `zeroinf` (default: `False`): zero-inflation handling.

### ComBat-seq
ComBat-seq uses negative binomial regression to adjust batch effects in count matrices while retaining integer count structure. It estimates batch effects from count data and returns batch-adjusted counts.

Exposed parameters:
- `full_mod` (default: `True`): inclusion of the biological group term in the model.
- `shrink` (default: `False`): empirical Bayes shrinkage for batch-effect estimates.
- `shrink.disp` (default: `False`): shrinkage for dispersion estimates.
- `gene.subset.n` (default: `1000`): feature subset size for shrinkage estimation when shrinkage is enabled.

### FSQN
FSQN applies feature-specific quantile normalization by mapping each feature distribution to a reference distribution. In mBatchNet it uses the selected reference batch as the feature-wise reference distribution for the TSS feature table.

Exposed parameters: none.

### FAbatch
FAbatch performs model-based batch-effect adjustment for high-dimensional data analyses involving a binary target variable. In mBatchNet it receives CLR-transformed data with binary target and batch labels.

Exposed parameters:
- `minerr` (default: `0.000001`): convergence tolerance.
- `probcrossbatch` (default: `False`): probabilistic cross-batch class assignment.
- `maxnbf` (default: `8`): maximum number of batch factors estimated.
- `maxiter` (default: `100`): maximum optimization iterations.

### Limma
limma provides linear-model methods for expression and omics data analysis, including `removeBatchEffect` for removing batch and covariate components from a numeric matrix. In mBatchNet it receives a log-scale feature matrix with batch labels and optional covariates.

Exposed parameters: none.

### BMC
The cited BMC method addresses dataset-specific multiplicative systematic bias so gene-expression datasets can be combined for meta-analysis and prognosis modeling. In mBatchNet it calls `pamr` batch adjustment on a log-scale feature matrix with batch labels.

Exposed parameters: none.

### ComBat
ComBat uses parametric or non-parametric empirical Bayes frameworks for adjusting known batch effects in genomic data. It estimates batch-specific location and scale effects and borrows information across features through empirical Bayes shrinkage.

Exposed parameters:
- `par.prior` (default: `False`): parametric empirical Bayes prior; `False` uses the non-parametric prior.
- `mean.only` (default: `False`): mean-only adjustment; `False` adjusts both means and variances.

### Outputs
For each session, intermediate and final outputs are generated under `output/<session_id>/`, including:
- preprocessed and corrected matrices
- assessment figures (PCA/PCoA/NMDS/PERMANOVA/PVCA, etc.)
- run log (`run.log`)
- output summary (`output_summary.json`)
- validation report (`validation_report.json`)
- runtime summary (`runtime_summary.json`)
- parameter manifest (`parameter_manifest.json`)
- reproducibility manifest (`reproducibility_manifest.json`)
- two download entries:
  - output bundle for corrected matrices, assessment outputs, and summaries
  - reproducibility bundle for inputs, manifests, configuration, and logs

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
