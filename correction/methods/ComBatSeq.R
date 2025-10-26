if (!exists(".CORRECTION_BOOTSTRAPPED", envir = .GlobalEnv, inherits = FALSE)) {
  source(file.path("correction", "correction.R"))
}

METHOD_CODE  <- "ComBatSeq"
METHOD_LABEL <- "ComBat-Seq"

run_correction_method <- function() {
  prepare_method(METHOD_CODE)

  run_method(METHOD_LABEL, {
    require(sva)
    if (!("phenotype" %in% colnames(metadata))) fail_step(METHOD_LABEL, "'phenotype' is required.")
    counts <- get_input_for(METHOD_CODE, base_M, base_form)
    libsz <- rowSums(counts); keep <- libsz > 0
    if (any(!keep)) {
      say("ComBat-Seq: removing ", sum(!keep), " samples with zero library size.")
      counts  <- counts[keep, , drop = FALSE]
      metadata <- metadata[keep, , drop = FALSE]
    }
    if (!all(rownames(counts) == rownames(metadata))) fail_step(METHOD_LABEL, "Sample IDs mismatch after filtering.")
    adj <- ComBat_seq(counts = t(counts), batch = metadata$batch_id, group = metadata$phenotype)
    out_counts <- t(adj)
    write_tss_clr(METHOD_LABEL, out_counts, "counts", "normalized_combatseq.csv")
  })

  finalize_method()
}

if (!exists(".CORRECTION_DISPATCH_ACTIVE", envir = .GlobalEnv, inherits = FALSE)) {
  run_correction_method()
}
