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

parse_param_value <- function(val) {
  if (identical(val, "")) return("")
  converted <- utils::type.convert(val, as.is = TRUE)
  if (is.character(converted)) return(val)
  converted
}

METHOD_PARAMS <- list()
if (length(args) > 1) {
  for (arg in args[-1]) {
    if (!startsWith(arg, "--")) next
    eq_pos <- regexpr("=", arg, fixed = TRUE)
    if (eq_pos < 0) next
    key <- substr(arg, 3, eq_pos - 1L)
    if (!nzchar(key)) next
    val <- substr(arg, eq_pos + 1L, nchar(arg))
    METHOD_PARAMS[[key]] <- parse_param_value(val)
  }
}

get_param <- function(name, default = NULL) {
  if (!nzchar(name)) return(default)
  if (name %in% names(METHOD_PARAMS)) {
    return(METHOD_PARAMS[[name]])
  }
  default
}

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

# ---------------------------
# Target/label helpers
# ---------------------------
TARGET_BINARY_COL <- "target_binary"

resolve_label_column <- function(metadata, output_dir = output_folder) {
  cfg_path <- file.path(output_dir, "session_config.json")
  label_col <- NULL
  try({
    if (.can_json && file.exists(cfg_path)) {
      cfg <- jsonlite::fromJSON(cfg_path)
      if (!is.null(cfg$label_column)) label_col <- cfg$label_column
    }
  }, silent = TRUE)
  if (!is.null(label_col) && label_col %in% colnames(metadata)) return(label_col)
  if ("phenotype" %in% colnames(metadata)) return("phenotype")
  NULL
}

convert_target_to_binary <- function(metadata, label_col) {
  if (!is.data.frame(metadata)) return(list(metadata = metadata, mapping = NULL, changed = FALSE))
  if (is.null(label_col) || !(label_col %in% colnames(metadata))) {
    return(list(metadata = metadata, mapping = NULL, changed = FALSE, reason = "label column missing"))
  }

  vals <- metadata[[label_col]]
  if (is.numeric(vals) || is.logical(vals)) {
    uniq <- sort(unique(vals))
    uniq <- uniq[is.finite(uniq)]
    uniq <- uniq[!is.na(uniq)]
    if (length(uniq) == 2) {
      mapping <- data.frame(label = uniq, binary = c(0L, 1L), stringsAsFactors = FALSE)
      bin <- as.integer(factor(vals, levels = uniq)) - 1L
      metadata[[TARGET_BINARY_COL]] <- bin
      return(list(metadata = metadata, mapping = mapping, changed = TRUE, reason = "numeric labels coerced to 0/1"))
    }
    return(list(metadata = metadata, mapping = NULL, changed = FALSE, reason = "numeric labels need exactly 2 unique values"))
  }

  vals_chr <- trimws(as.character(vals))
  uniq <- unique(vals_chr[!is.na(vals_chr) & nzchar(vals_chr)])
  if (length(uniq) != 2) {
    return(list(metadata = metadata, mapping = NULL, changed = FALSE, reason = "label column needs exactly 2 unique text levels"))
  }

  uniq <- sort(uniq)
  bin <- ifelse(vals_chr == uniq[1], 0L, 1L)
  metadata[[TARGET_BINARY_COL]] <- as.integer(bin)

  mapping <- data.frame(
    label = uniq,
    binary = c(0L, 1L),
    stringsAsFactors = FALSE
  )

  list(metadata = metadata, mapping = mapping, changed = TRUE, reason = "converted labels to target_binary")
}

record_methods <- c(
  "BMC","limma","ConQuR","PLSDA","ComBat","FSQN","MMUPHin",
  "fastRUV-III-NB","RUV-III-NB","MetaDICT","FAbatch","ComBat-Seq","DEBIAS"
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
METADATA_MAPPINGS <- NULL
try({
  if (.can_json) {
    cfg_path <- file.path(output_folder, "session_config.json")
    if (file.exists(cfg_path)) {
      cfg <- jsonlite::fromJSON(cfg_path, simplifyVector = FALSE)
      if (!is.null(cfg$control_label) && nzchar(as.character(cfg$control_label))) {
        CONTROL_LABEL <- as.character(cfg$control_label)
      }
      if (!is.null(cfg$reference_batch) && nzchar(as.character(cfg$reference_batch))) {
        REFERENCE_BATCH <- as.character(cfg$reference_batch)
      }
      if (!is.null(cfg$metadata_mappings)) {
        METADATA_MAPPINGS <- cfg$metadata_mappings
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
  say <- function(...){ cat(paste0(..., collapse=""), "\n") }
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

run_method <- function(name, expr, log_name = name){
  log_label <- if (is.null(log_name) || !nzchar(log_name)) name else log_name
  t0 <- start_step(log_label)
  on.exit({ cleanup_method_memory(name) }, add = TRUE)
  out <- withCallingHandlers(
    tryCatch({
      val <- force(expr)
      ok_step(log_label, t0)
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
      fail_step(log_label, conditionMessage(e))
    }),
    warning = function(w){ warn_step(log_label, conditionMessage(w)) }
  )
  invisible(out)
}

# ---------------------------
# Desired inputs per method (uses converters from preprocess.r)
# ---------------------------
expected_input <- list(
  BMC       = "log",
  limma     = "log",
  ConQuR    = "counts",
  PLSDA     = "clr",
  ComBat    = "log",
  FSQN      = "tss",
  MMUPHin   = "tss",
  RUV       = "counts",
  MetaDICT  = "tss",
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
custom_matrix_path      <- file.path(output_folder, "raw.csv")
custom_metadata_path    <- file.path(output_folder, "metadata.csv")
fallback_metadata_path  <- file.path(output_folder, "metadata_origin.csv")
default_matrix_path     <- file.path("assets", "example", "raw_1.csv")
default_metadata_path   <- file.path("assets", "example", "metadata_1.csv")

if (file.exists(custom_matrix_path) && file.exists(custom_metadata_path)) {
  say("✅ Using uploaded user files")
  uploaded_mat <- read.csv(custom_matrix_path, header = FALSE, check.names = FALSE)
  metadata <- read.csv(custom_metadata_path, check.names = FALSE)
} else if (file.exists(custom_matrix_path) && file.exists(fallback_metadata_path)) {
  say("✅ Using uploaded matrix with fallback metadata_origin.csv")
  uploaded_mat <- read.csv(custom_matrix_path, header = FALSE, check.names = FALSE)
  metadata <- read.csv(fallback_metadata_path, check.names = FALSE)
} else {
  say("⚠️ No uploaded files found — using default assets")
  uploaded_mat <- read.csv(default_matrix_path, header = FALSE, check.names = FALSE)
  metadata     <- read.csv(default_metadata_path, check.names = FALSE)
}

target_mapping <- NULL
map_config <- METADATA_MAPPINGS
if (!is.null(map_config$target_binary$mapping)) {
  target_mapping <- map_config$target_binary$mapping
}

label_col <- if (!is.null(map_config$label_column)) map_config$label_column else resolve_label_column(metadata, output_folder)
if (!(TARGET_BINARY_COL %in% colnames(metadata))) {
  conv_target <- convert_target_to_binary(metadata, label_col)
  if (isTRUE(conv_target$changed)) {
    metadata <- conv_target$metadata
    if (!is.null(conv_target$mapping)) target_mapping <- conv_target$mapping
    say("ℹ️ Converted target to binary for correction use")
  } else if (!is.null(conv_target$reason)) {
    say("ℹ️ Target left unchanged: ", conv_target$reason)
  }
}

if (!is.na(CONTROL_LABEL)) {
  if (!is.null(target_mapping) && "label" %in% colnames(target_mapping)) {
    mapped <- target_mapping$binary[match(CONTROL_LABEL, target_mapping$label)]
    if (length(mapped) && is.finite(mapped[1])) {
      CONTROL_LABEL <- mapped[1]
    }
  }
  if (is.character(CONTROL_LABEL)) {
    num_label <- suppressWarnings(as.numeric(CONTROL_LABEL))
    if (is.finite(num_label)) CONTROL_LABEL <- num_label
  }
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

metadata$batch <- factor(metadata$batch, levels = unique(metadata$batch))
batch <- metadata$batch

covar <- metadata[, !(colnames(metadata) %in% c("sample_id","batch", TARGET_BINARY_COL)), drop = FALSE]
covar <- as.data.frame(lapply(covar, function(col) {
  if (is.numeric(col))      col[is.na(col)] <- mean(col, na.rm = TRUE)
  else if (is.factor(col))  { if (anyNA(col)) col[is.na(col)] <- levels(col)[1] }
  else                      { if (anyNA(col)) { kept <- suppressWarnings(as.character(stats::na.omit(col))); if (length(kept)) col[is.na(col)] <- kept[1] } }
  col
}))

lvl <- levels(batch)
if (!is.na(REFERENCE_BATCH) && REFERENCE_BATCH %in% lvl) {
  reference_batch <- REFERENCE_BATCH
} else {
  reference_batch <- lvl[1]
}
ref_idx <- which(batch == reference_batch)

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
  BMC="normalized_bmc.csv", limma="normalized_limma.csv",
  ConQuR="normalized_conqur.csv", PLSDA="normalized_plsda.csv", ComBat="normalized_combat.csv",
  FSQN="normalized_fsqn.csv", MMUPHin="normalized_mmuphin.csv",
  RUV="normalized_ruv.csv", MetaDICT="normalized_metadict.csv",
  FAbatch="normalized_fabatch.csv", ComBatSeq="normalized_combatseq.csv",
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
