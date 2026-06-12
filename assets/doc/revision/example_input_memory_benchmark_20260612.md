# Example Input Runtime and Memory Benchmark

Date: 2026-06-12

Purpose: record reference runtime and peak memory for the bundled AD example input used by the mBatchNet correction workflow. These values are reference measurements for this input and environment only. Actual user runtime depends on input size, selected methods, method parameters, optional dependencies, and server load.

## Environment

- CPU: Intel Xeon E-2278G, 8 cores / 16 threads
- System memory: 31 GiB RAM, 8 GiB swap
- R: 4.3.3
- Measurement tool: `/usr/bin/time -v`

## Input

- Matrix: `assets/example/raw_ad.csv`
- Metadata: `assets/example/metadata_ad.csv`
- Samples: 75
- Features: 231
- Matrix cells: 17,325
- Matrix CSV size: 40 KB
- Metadata CSV size: 4 KB

## Commands

The benchmark session directory was `output/benchmark_example_memory_20260612`.

Preprocessing:

```bash
/usr/bin/time -v Rscript correction/preprocess.R \
  output/benchmark_example_memory_20260612 \
  output/benchmark_example_memory_20260612/raw.csv
```

Corrections:

```bash
/usr/bin/time -v bash -lc '
methods=(ComBat limma ComBatSeq FAbatch BMC FSQN RUV MMUPHin PLSDA ConQuR DEBIAS MetaDICT)
for method in "${methods[@]}"; do
  /usr/bin/time -v Rscript "correction/methods/${method}.R" \
    output/benchmark_example_memory_20260612
done
'
```

## Summary

- Preprocessing succeeded in 0.46 s with 73,576 KiB peak RSS.
- Correction batch elapsed time was 60.58 s with 632,308 KiB peak RSS.
- 11 of 12 supported method scripts completed successfully.
- DEBIAS-M failed before correction because the optional Python `debiasm` dependency is not installed in the configured project environment.
- The highest successful method peak RSS was FAbatch at 632,308 KiB.
- The slowest successful method was MetaDICT at 24.56 s.

Detailed per-method measurements are in `example_input_memory_benchmark_20260612.csv`.

