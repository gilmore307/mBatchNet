# Representative Input Runtime and Memory Benchmark

Purpose: record reference runtime and peak memory for four mBatchNet-compatible example inputs with different matrix shapes. These values are environment-specific reference measurements only. Actual runtime depends on input size, selected methods, method parameters, optional dependencies, deployment hardware, and server load.

## Environment

- Date: 2026-06-12
- Host CPU: Intel(R) Xeon(R) E-2278G CPU @ 3.40GHz
- CPU topology: 8 cores / 16 threads
- System memory: 31 GiB RAM, 8 GiB swap
- R: 4.3.3
- DEBIAS-M Python environment: project `.venv` with `debiasm` available
- Measurement tool: `/usr/bin/time -v`

## Inputs

| Dataset | Source | Samples | Features | Matrix cells | Raw CSV | Metadata CSV | Validation result |
|---|---|---:|---:|---:|---:|---:|---|
| bundled_ad | `assets/example/raw_ad.csv` | 75 | 231 | 17,325 | 40.00 KB | 4.00 KB | valid |
| conqur_sample | `assets/example/data_conqur.R` | 273 | 100 | 27,300 | 60.86 KB | 7.41 KB | valid; 2 all-zero feature warning |
| plsda_full | `assets/example/data_plsda.R` | 75 | 567 | 42,525 | 88.40 KB | 1.50 KB | valid |
| metadict_full | `assets/example/data_metadict.R` | 400 | 131 | 52,400 | 110.68 KB | 13.30 KB | valid; 6 all-zero features and high-sparsity warnings |

The ConQuR-derived, PLSDAbatch FullData, and MetaDICT full inputs were generated from the example-data scripts/object layouts in `assets/example` and then validated against the current public upload contract.

## Batch Summary

| Dataset | Preprocess time (s) | Preprocess peak RSS (MiB) | 12-method batch time (s) | 12-method batch peak RSS (MiB) | Successful methods |
|---|---:|---:|---:|---:|---:|
| bundled_ad | 0.46 | 71.85 | 71.47 | 1096.80 | 12/12 |
| conqur_sample | 0.33 | 72.89 | 51.42 | 1099.71 | 12/12 |
| plsda_full | 0.39 | 77.66 | 142.55 | 1104.91 | 12/12 |
| metadict_full | 0.39 | 79.42 | 52.30 | 1107.13 | 11/12 |

## Method-Level Highlights

- Peak memory was consistently driven by DEBIAS-M at about 1.10 GiB across the tested inputs.
- The wider PLSDAbatch FullData input increased feature-sensitive method runtime, especially MetaDICT, which took 86.91 s.
- ConQuR took 8.93 s on the bundled AD input, 7.80 s on the 273 x 100 ConQuR-derived input, and 15.94 s on the 75 x 567 PLSDAbatch FullData input.
- The MetaDICT full input was the largest tested matrix by cell count (400 x 131 = 52,400 cells). Eleven of twelve methods completed successfully; FAbatch failed because its model-specific requirement that the number of retained features exceed the maximum batch size was not met after filtering (p = 125, max batch size = 200).

Detailed measurements are in `representative_input_memory_benchmark_20260612.csv`.

## Notes

- The original `assets/example/data_plsda.R` script assumes an older `PLSDAbatch::AD_data` object layout. For this benchmark, the current package object's `FullData$X.count` matrix was used directly with public-server-compatible metadata columns.
- The original `assets/example/data_metadict.R` script includes package-installation logic. For this benchmark, the already installed package object's `exampleData$O` matrix was used directly with public-server-compatible metadata columns.
- `assets/example/data_cmgd.R` depends on `curatedMetagenomicData`, which was not installed locally and may require larger external downloads.
