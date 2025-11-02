# Shared setup for individual correction method scripts
# This file bootstraps the session (arguments, preprocessing helpers,
# dataset loading) and exposes utility helpers that each method script can use.

# ---------------------------
# Handle Arguments & session paths
# ---------------------------
if (!interactive()) {
  nullfile <- if (.Platform$OS.type == "windows") "NUL" else "/dev/null"
  options(device = function(...) grDevices::pdf(file = nullfile))
  try({
    while (grDevices::dev.cur() > 1L) grDevices::dev.off()
  }, silent = TRUE)
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  args <- c("output/example")
}

output_folder <- args[[1]]
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

# ---------------------------
# Session summary helpers
# ---------------------------
summary_file <- file.path(output_folder, "session_summary.json")
.can_json <- suppressWarnings(requireNamespace("jsonlite", quietly = TRUE))
.session_summary <- list(
  session   = basename(output_folder),
  started   = as.character(Sys.time()),
  n_samples = NA_integer_,
  n_features= NA_integer_,
  methods   = list()
)

if (.can_json && file.exists(summary_file)) {
  try({
    prev <- jsonlite::fromJSON(summary_file, simplifyVector = FALSE)
    if (is.list(prev) && length(prev)) {
      if (!is.null(prev$methods) && is.list(prev$methods)) {
        .session_summary$methods <- prev$methods
      }
      for (nm in setdiff(names(prev), "methods")) {
        val <- prev[[nm]]
        if (!is.null(val)) {
          .session_summary[[nm]] <- val
        }
      }
      if (!is.null(prev$started) && nzchar(as.character(prev$started))) {
        .session_summary$started <- prev$started
      }
    }
  }, silent = TRUE)
}

record_methods <- c(
  "QN","BMC","limma","ConQuR","PLSDAbatch","ComBat","FSQN","MMUPHin",
  "fastRUV-III-NB","MetaDICT","PN","FAbatch","ComBat-Seq","DEBIAS"
)

save_summary <- function(){
  if (.can_json) {
    try(jsonlite::write_json(.session_summary, summary_file, auto_unbox = TRUE, pretty = TRUE), silent = TRUE)
  }
}

# ---------------------------
# Optional session configuration (control label, reference batch)
# ---------------------------
CONTROL_LABEL <- NA_character_
REFERENCE_BATCH <- NA_character_
try({
  if (.can_json) {
    cfg_path <- file.path(output_folder, "session_config.json")
    if (file.exists(cfg_path)) {
      cfg <- jsonlite::fromJSON(cfg_path)
      if (!is.null(cfg$control_label) && nzchar(as.character(cfg$control_label))) {
        CONTROL_LABEL <- as.character(cfg$control_label)
      }
      if (!is.null(cfg$reference_batch) && nzchar(as.character(cfg$reference_batch))) {
        REFERENCE_BATCH <- as.character(cfg$reference_batch)
      }
    }
  }
}, silent = TRUE)

# ---------------------------
# Load preprocess helpers (write_tss_clr, detect_input_form, ...)
# ---------------------------
assign("PREPROC_OUTPUT_DIR", output_folder, envir = .GlobalEnv)
assign("PREPROC_MATRIX_PATH", file.path(output_folder, "raw.csv"), envir = .GlobalEnv)
assign("PREPROCESS_SKIP_MAIN", TRUE, envir = .GlobalEnv)
source(file.path("correction", "preprocess.R"))

if (!exists("say", mode = "function")) {
  .ts <- function() format(Sys.time(), "%H:%M:%S")
  say <- function(...){ cat(sprintf("[%s] ", .ts()), paste0(..., collapse=""), "\n") }
  start_step <- function(name){ say("▶️ START: ", name); proc.time() }
  ok_step    <- function(name, t0){ dt <- proc.time()-t0; say("✅ DONE:  ", name, sprintf(" (%.2fs)", dt["elapsed"])) }
  warn_step  <- function(name, msg){ say("⚠️ WARN:  ", name, " — ", msg) }
  fail_step  <- function(name, msg){ say("❌ FAIL:  ", name, " — ", msg); stop(paste0(name, ": ", msg)) }
}

# ---------------------------
# Logging helpers
# ---------------------------
cleanup_method_memory <- function(name = NULL) {
  try({
    gc(verbose = FALSE, reset = FALSE, full = TRUE)
  }, silent = TRUE)
  invisible()
}

run_method <- function(name, expr){
  t0 <- start_step(name)
  on.exit({ cleanup_method_memory(name) }, add = TRUE)
  out <- withCallingHandlers(
    tryCatch({
      val <- force(expr)
      ok_step(name, t0)
      if (name %in% record_methods) {
        dt <- as.numeric((proc.time() - t0)["elapsed"])
        if (length(.session_summary$methods)) {
          keep <- vapply(
            .session_summary$methods,
            function(item) {
              if (!is.list(item)) return(TRUE)
              n <- item[["name"]]
              if (is.null(n)) return(TRUE)
              !identical(as.character(n), as.character(name))
            },
            logical(1),
            USE.NAMES = FALSE
          )
          .session_summary$methods <<- .session_summary$methods[keep]
        }
        .session_summary$methods[[length(.session_summary$methods) + 1L]] <<- list(
          name = name,
          status = "success",
          elapsed_sec = round(dt, 3),
          finished = as.character(Sys.time())
        )
        save_summary()
      }
      val
    }, error = function(e){
      fail_step(name, conditionMessage(e))
    }),
    warning = function(w){ warn_step(name, conditionMessage(w)) }
  )
  invisible(out)
}

# ---------------------------
# Desired inputs per method (uses converters from preprocess.r)
# ---------------------------
expected_input <- list(
  QN        = "tss",
  BMC       = "log",
  limma     = "log",
  ConQuR    = "counts",
  PLSDA     = "clr",
  ComBat    = "log",
  FSQN      = "tss",
  MMUPHin   = "tss",
  RUV       = "counts",
  MetaDICT  = "tss",
  PN        = "tss",
  FAbatch   = "log",
  ComBatSeq = "counts",
  DEBIAS    = "counts"
)

get_input_for <- function(method, base_M, base_form) {
  target <- expected_input[[method]]
  if (is.null(target)) stop("Unknown method in expected_input: ", method)
  if (target == base_form) return(base_M)
  say(sprintf("🔄 Convert for %s: %s → %s", method, base_form, target))
  out <- switch(
    target,
    "tss"    = to_tss(base_M, base_form),
    "log"    = to_log(base_M, base_form),
    "clr"    = to_clr(base_M, base_form),
    "counts" = to_counts(base_M, base_form),
    stop("Unhandled target: ", target)
  )
  say(sprintf("   ↳ done (%s → %s) | n=%d×%d", base_form, target, nrow(out), ncol(out)))
  out
}

# ---------------------------
# Load data (matrix + metadata)
# ---------------------------
custom_matrix_path    <- file.path(output_folder, "raw.csv")
custom_metadata_path  <- file.path(output_folder, "metadata.csv")
default_matrix_path   <- file.path("assets", "example", "raw_1.csv")
default_metadata_path <- file.path("assets", "example", "metadata_1.csv")

if (file.exists(custom_matrix_path) && file.exists(custom_metadata_path)) {
  say("✅ Using uploaded user files")
  uploaded_mat <- read.csv(custom_matrix_path, header = FALSE, check.names = FALSE)
  metadata     <- read.csv(custom_metadata_path, check.names = FALSE)
} else {
  say("⚠️ No uploaded files found — using default assets")
  uploaded_mat <- read.csv(default_matrix_path, header = FALSE, check.names = FALSE)
  metadata     <- read.csv(default_metadata_path, check.names = FALSE)
}

check_table(uploaded_mat, "uploaded_mat", allow_negative = TRUE)
input_form <- detect_input_form(uploaded_mat)
say("ℹ️ Detected input form: ", input_form)

if (!("sample_id" %in% colnames(metadata))) {
  metadata$sample_id <- sprintf("S%03d", seq_len(nrow(metadata)))
}
metadata <- make_unique_sample_ids(metadata, id_col = "sample_id", sep = "_")
if (nrow(uploaded_mat) != nrow(metadata))
  fail_step("Alignment", "Row count mismatch between matrix and metadata.")
rownames(uploaded_mat) <- metadata$sample_id
rownames(metadata)     <- metadata$sample_id

base_M    <- as.matrix(uploaded_mat)
base_form <- input_form

try({
  .session_summary$n_samples <- nrow(base_M)
  .session_summary$n_features <- ncol(base_M)
  save_summary()
}, silent = TRUE)

metadata$batch_id <- factor(metadata$batch_id, levels = unique(metadata$batch_id))
batch_id <- metadata$batch_id

covar <- metadata[, !(colnames(metadata) %in% c("sample_id","batch_id","phenotype")), drop = FALSE]
covar <- as.data.frame(lapply(covar, function(col) {
  if (is.numeric(col))      col[is.na(col)] <- mean(col, na.rm = TRUE)
  else if (is.factor(col))  { if (anyNA(col)) col[is.na(col)] <- levels(col)[1] }
  else                      { if (anyNA(col)) { kept <- suppressWarnings(as.character(stats::na.omit(col))); if (length(kept)) col[is.na(col)] <- kept[1] } }
  col
}))

lvl <- levels(batch_id)
if (!is.na(REFERENCE_BATCH) && REFERENCE_BATCH %in% lvl) {
  reference_batch <- REFERENCE_BATCH
} else {
  reference_batch <- lvl[1]
}
ref_idx <- which(batch_id == reference_batch)

method_list <- character(0)
CURRENT_METHOD_CODE <- NA_character_

run_input_checks <- function() {
  run_method("Input checks", {
    check_table(base_M, "base_M (detected input)", allow_negative = TRUE)
    check_table(metadata, "metadata", allow_negative = TRUE)
    if (!identical(rownames(base_M), rownames(metadata))) {
      fail_step("Alignment", "rownames(base_M) != rownames(metadata).")
    }
    say("Method list: ", paste(method_list, collapse=", "))
    say("Samples: ", nrow(base_M), " | Features: ", ncol(base_M))
    say("Reference batch level: ", as.character(reference_batch),
        " | #ref samples: ", length(ref_idx))
    if (length(ref_idx) < 1) warn_step("Reference", "No samples in reference batch level.")
  })
}

prepare_method <- function(method_code) {
  method_list <<- as.character(method_code)
  CURRENT_METHOD_CODE <<- as.character(method_code)
  run_input_checks()
}

expected_outputs <- list(
  QN="normalized_qn.csv", BMC="normalized_bmc.csv", limma="normalized_limma.csv",
  ConQuR="normalized_conqur.csv", PLSDA="normalized_plsda.csv", ComBat="normalized_combat.csv",
  FSQN="normalized_fsqn.csv", MMUPHin="normalized_mmuphin.csv",
  RUV="normalized_ruv.csv", MetaDICT="normalized_metadict.csv",
  PN="normalized_pn.csv", FAbatch="normalized_fabatch.csv", ComBatSeq="normalized_combatseq.csv",
  DEBIAS = "normalized_debias.csv"
)

audit_outputs <- function(selected, out_dir) {
  say("— Output audit (TSS & CLR) —")
  for (m in selected) {
    fname <- expected_outputs[[m]]
    if (is.null(fname)) { say("  ", m, ": (no declared output)"); next }
    base <- sub("\\.csv$", "", fname, ignore.case = TRUE)
    p_tss <- file.path(out_dir, paste0(base, "_tss.csv"))
    p_clr <- file.path(out_dir, paste0(base, "_clr.csv"))

    if (file.exists(p_tss)) {
      sz <- file.info(p_tss)$size
      say("  ✅ ", m, " -> ", basename(p_tss), " (", format(sz, big.mark=","), " bytes)")
    } else {
      say("  ❌ ", m, " -> ", basename(p_tss), " (missing)")
    }

    if (file.exists(p_clr)) {
      sz <- file.info(p_clr)$size
      say("  ✅ ", m, " -> ", basename(p_clr), " (", format(sz, big.mark=","), " bytes)")
    } else {
      say("  ❌ ", m, " -> ", basename(p_clr), " (missing)")
    }
  }
}

finalize_method <- function() {
  if (!length(method_list)) return(invisible(NULL))
  say("🎉 Method completed: ", paste(method_list, collapse=", "))
  audit_outputs(method_list, output_folder)
  invisible(NULL)
}
