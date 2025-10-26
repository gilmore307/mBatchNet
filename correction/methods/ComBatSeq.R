source(file.path("correction", "correction.R"))

prepare_method("ComBatSeq")

run_method("ComBat-Seq", {
  require(sva)
  if (!("phenotype" %in% colnames(metadata))) fail_step("ComBat-Seq", "'phenotype' is required.")
  counts <- get_input_for("ComBatSeq", base_M, base_form)
  libsz <- rowSums(counts); keep <- libsz > 0
  if (any(!keep)) {
    say("ComBat-Seq: removing ", sum(!keep), " samples with zero library size.")
    counts  <- counts[keep, , drop = FALSE]
    metadata <- metadata[keep, , drop = FALSE]
  }
  if (!all(rownames(counts) == rownames(metadata))) fail_step("ComBat-Seq", "Sample IDs mismatch after filtering.")
  adj <- ComBat_seq(counts = t(counts), batch = metadata$batch_id, group = metadata$phenotype)
  out_counts <- t(adj)
  write_tss_clr("ComBat-Seq", out_counts, "counts", "normalized_combatseq.csv")
})

finalize_method()
