# -----------------------------------------------------------------------------
# Load Data
# -----------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
# Allow caller (e.g., per-method scripts) to predefine destinations to avoid mis-parsing
if (exists("PREPROC_OUTPUT_DIR", inherits = TRUE)) {
  output_folder <- get("PREPROC_OUTPUT_DIR", inherits = TRUE)
} else {
  output_folder <- if (length(args) >= 1) args[1] else "output/example"
}
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

if (exists("PREPROC_MATRIX_PATH", inherits = TRUE)) {
  matrix_path <- get("PREPROC_MATRIX_PATH", inherits = TRUE)
} else {
  matrix_path <- if (length(args) >= 2) args[2] else file.path(output_folder, "raw.csv")
}
if (!file.exists(matrix_path)) {
  alt <- file.path("assets", "example", "raw_1.csv")
  if (file.exists(alt)) {
    message("No uploaded file found at ", matrix_path, "; using ", alt)
    matrix_path <- alt
  } else {
    stop("Neither ", matrix_path, " nor ", alt, " exists.")
  }
}

# ---------------------------
# Console Logger & Guards
# ---------------------------
.ts <- function() format(Sys.time(), "%H:%M:%S")
say <- function(...){ cat(sprintf("[%s] ", .ts()), paste0(..., collapse=""), "\n") }
start_step <- function(name){ say("▶️ START: ", name); proc.time() }
ok_step    <- function(name, t0){ dt <- proc.time()-t0; say("✅ DONE:  ", name, sprintf(" (%.2fs)", dt["elapsed"])) }
warn_step  <- function(name, msg){ say("⚠️ WARN:  ", name, " — ", msg) }
fail_step  <- function(name, msg){ say("❌ FAIL:  ", name, " — ", msg); stop(paste0(name, ": ", msg)) }

post_summary <- function(M, name){
  M <- as.matrix(M)
  say(sprintf("%s summary: min=%.5f max=%.5f rowmean|avg=%.2e NA=%d",
              name, min(M, na.rm=TRUE), max(M, na.rm=TRUE),
              mean(abs(rowMeans(M))), sum(!is.finite(M))))
}

# Ensure metadata$sample_id values are unique; rename duplicates with suffixes
make_unique_sample_ids <- function(metadata, id_col = "sample_id", sep = "_") {
  if (!is.data.frame(metadata) || !(id_col %in% colnames(metadata))) return(metadata)
  ids <- as.character(metadata[[id_col]])
  if (!length(ids)) return(metadata)
  dup_mask <- duplicated(ids) | duplicated(ids, fromLast = TRUE)
  if (any(dup_mask)) {
    ids_new <- make.unique(ids, sep = sep)
    changed <- sum(ids_new != ids)
    # Log a concise summary of changes
    ex <- unique(ids[dup_mask])
    ex_str <- paste(utils::head(ex, 5), collapse=", ")
    if (length(ex) > 5) ex_str <- paste0(ex_str, ", ...")
    warn_step("Metadata IDs", sprintf("Detected %d duplicate sample_id(s): %s", length(ex), ex_str))
    warn_step("Metadata IDs", sprintf("Renamed %d entries to ensure uniqueness (suffix %sN).", changed, sep))
    metadata[[id_col]] <- ids_new
  }
  metadata
}

TARGET_BINARY_COL <- "target_binary"

# Convert a text or numeric label column into target_binary (0/1).
# Returns a list with the updated metadata, a mapping (label -> binary), and a
# boolean indicating whether conversion occurred. If conversion is not possible,
# `changed` is FALSE and `metadata` is returned unchanged.
resolve_label_column <- function(metadata, output_dir) {
  cfg_path <- file.path(output_dir, "session_config.json")
  label_col <- NULL
  try({
    if (file.exists(cfg_path)) {
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
  # If already numeric/logical with two levels, coerce to 0/1 deterministically
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

encode_to_numeric <- function(x) {
  if (is.numeric(x)) return(list(values = as.numeric(x), mapping = NULL))
  if (is.logical(x)) return(list(values = as.integer(x), mapping = data.frame(label = c(FALSE, TRUE), numeric = 0:1)))

  vals <- trimws(as.character(x))
  levels <- unique(vals[!is.na(vals)])
  if (!length(levels)) {
    return(list(values = rep(NA_real_, length(x)), mapping = NULL))
  }

  codes <- match(vals, levels) - 1L
  list(
    values  = as.numeric(codes),
    mapping = data.frame(label = levels, numeric = seq_along(levels) - 1L, stringsAsFactors = FALSE)
  )
}

prepare_metadata_outputs <- function(output_dir) {
  origin_path <- file.path(output_dir, "metadata_origin.csv")
  corr_path   <- file.path(output_dir, "metadata.csv")

  src_path <- if (file.exists(origin_path)) origin_path else corr_path
  if (!file.exists(src_path)) return(invisible(NULL))

  t0 <- start_step("Prepare metadata for correction")
  meta <- tryCatch(utils::read.csv(src_path, check.names = FALSE), error = function(e) NULL)
  if (is.null(meta)) {
    warn_step("metadata", "Failed to read metadata; skipping metadata prep.")
    return(invisible(NULL))
  }

  # Ensure sample_id
  if (!("sample_id" %in% colnames(meta))) {
    meta$sample_id <- sprintf("S%03d", seq_len(nrow(meta)))
  }
  meta <- make_unique_sample_ids(meta, id_col = "sample_id", sep = "_")

  label_col <- resolve_label_column(meta, output_dir)
  bin_res <- convert_target_to_binary(meta, label_col)
  if (!is.null(bin_res$metadata)) meta <- bin_res$metadata

  # Textual copy kept for plotting
  meta_text <- meta

  # Numeric encoding for correction
  keep_cols <- c("sample_id", "batch_id", TARGET_BINARY_COL)
  num_meta <- meta
  col_maps <- list()
  for (nm in colnames(num_meta)) {
    if (nm %in% keep_cols) {
      if (nm == TARGET_BINARY_COL) {
        num_meta[[nm]] <- suppressWarnings(as.numeric(num_meta[[nm]]))
      }
      next
    }
    enc <- encode_to_numeric(num_meta[[nm]])
    num_meta[[nm]] <- enc$values
    if (!is.null(enc$mapping)) col_maps[[nm]] <- enc$mapping
  }

  utils::write.csv(meta_text, origin_path, row.names = FALSE)
  utils::write.csv(num_meta, corr_path, row.names = FALSE)

  mapping_payload <- list(
    label_column = label_col,
    target_binary = if (!is.null(bin_res$mapping)) list(label_column = label_col, mapping = bin_res$mapping) else NULL,
    encodings = if (length(col_maps)) col_maps else NULL
  )
  map_path <- file.path(output_dir, "metadata_mappings.json")
  if (length(Filter(Negate(is.null), mapping_payload)) > 0) {
    jsonlite::write_json(mapping_payload, map_path, pretty = TRUE, auto_unbox = TRUE)
  }

  ok_step("Prepare metadata for correction", t0)
  invisible(NULL)
}

# --- If called as a script from Upload step, fix duplicates in-place ---
try({
  meta_path <- file.path(output_folder, "metadata.csv")
  if (file.exists(meta_path)) {
    t0 <- start_step("Check duplicate sample_id in metadata.csv")
    meta <- utils::read.csv(meta_path, check.names = FALSE)
    if ("sample_id" %in% colnames(meta)) {
      before <- as.character(meta$sample_id)
      meta2 <- make_unique_sample_ids(meta, id_col = "sample_id", sep = "_")
      if (!identical(before, as.character(meta2$sample_id))) {
        utils::write.csv(meta2, meta_path, row.names = FALSE)
        ok_step("Updated metadata.csv (unique sample_id)", t0)
      } else {
        ok_step("No duplicate sample_id found", t0)
      }
    } else {
      warn_step("metadata.csv", "No 'sample_id' column — skipping dedup.")
    }
  }
}, silent = TRUE)

check_table <- function(x, name = "table", allow_negative = TRUE) {
  if (!is.data.frame(x) && !is.matrix(x)) fail_step(name, "Not a data.frame/matrix.")
  DF <- as.data.frame(x, stringsAsFactors = FALSE)
  if (nrow(DF) < 2 || ncol(DF) < 2) fail_step(name, "Too few rows/cols.")
  num_cols <- vapply(DF, is.numeric, TRUE)
  if (any(num_cols)) {
    Mnum <- as.matrix(DF[num_cols])
    bad_fin <- which(!is.finite(Mnum), arr.ind = TRUE)
    if (nrow(bad_fin) > 0) fail_step(name, "NA/NaN/Inf present.")
    if (!allow_negative && any(Mnum < 0, na.rm = TRUE)) fail_step(name, "Negative values not allowed.")
  }
  if (any(!num_cols)) {
    Mchr <- DF[!num_cols]
    na_idx <- which(is.na(Mchr), arr.ind = TRUE)
    if (nrow(na_idx) > 0) fail_step(name, "Missing values in non-numeric columns.")
  }
  invisible(TRUE)
}

# ---------------------------
# Input form detection + converters (minimal set)
# ---------------------------

.normalize_tss <- function(mat){
  row_sums <- rowSums(mat)
  row_sums[row_sums == 0] <- 1
  out <- sweep(mat, 1, row_sums, "/")
  out[is.na(out)] <- 0
  out
}

is_counts_matrix <- function(M, tol = 1e-6, frac = 0.97, min_row_sum = 1) {
  M <- as.matrix(M)
  if (any(!is.finite(M))) return(FALSE)
  if (any(M < 0, na.rm = TRUE)) return(FALSE)
  intish <- mean(abs(M - round(M)) <= tol, na.rm = TRUE)
  if (is.na(intish) || intish < frac) return(FALSE)
  rs <- rowSums(M)
  if (!any(rs > min_row_sum, na.rm = TRUE)) return(FALSE)
  TRUE
}

is_tss_matrix <- function(M, rel_tol = 0.05) {
  M <- as.matrix(M)
  nr <- nrow(M); nc <- ncol(M)
  if (!nr || !nc) return(FALSE)
  if (any(!is.finite(M))) return(FALSE)
  if (min(M) < 0)         return(FALSE)
  storage.mode(M) <- "double"
  rs <- .rowSums(M, nr, nc)
  mu  <- mean(rs)
  tol <- max(1e-5, rel_tol * abs(mu))
  mean(abs(rs - mu) <= tol) >= 0.97
}

is_clr_matrix <- function(M, tol_mean = 1e-5, min_frac = 0.9) {
  M <- as.matrix(M)
  if (any(!is.finite(M))) return(FALSE)
  nr <- nrow(M); nc <- ncol(M)
  if (nr < 2 || nc < 2) return(FALSE)
  rm <- rowMeans(M)
  mean_ok <- mean(abs(rm) <= tol_mean)
  pos <- rowSums(M > 0)
  neg <- rowSums(M < 0)
  signmix_ok <- mean((pos > 0 & neg > 0) | (pos == 0 & neg == 0))
  (mean_ok >= min_frac) && (signmix_ok >= min_frac)
}

is_nonneg_log_matrix <- function(M, zero_pos_thresh = 0.8,
                                 backcheck_max = 50000L, int_tol = 1e-6) {
  M <- as.matrix(M)
  nr <- nrow(M); nc <- ncol(M)
  if (nr < 2 || nc < 2) return(FALSE)
  if (any(!is.finite(M))) return(FALSE)
  if (min(M) < 0) return(FALSE)
  zero_pos_rows <- (rowSums(M == 0) > 0) & (rowSums(M > 0) > 0)
  if (mean(zero_pos_rows) < zero_pos_thresh) return(FALSE)
  v <- as.vector(M)
  if (length(v) > backcheck_max) v <- sample(v, backcheck_max)
  back <- expm1(v); back[back < 0] <- 0
  mean(abs(back - round(back)) <= int_tol) >= 0.5
}

detect_input_form <- function(M) {
  if (is_tss_matrix(M))        return("tss")
  if (is_clr_matrix(M))        return("clr")
  if (is_counts_matrix(M))     return("counts")
  if (is_nonneg_log_matrix(M)) return("log")
  if (all(as.matrix(M) >= 0, na.rm = TRUE)) return("positive")
  "log"
}

to_counts <- function(M, from = NULL, scale = 1e5) {
  M <- as.matrix(M)
  if (is.null(from)) from <- detect_input_form(M)
  
  if (from == "counts") {
    M[!is.finite(M)] <- 0
    M[M < 0] <- 0
    C <- round(M)
    dimnames(C) <- dimnames(M)
    return(C)
  }
  
  P <- to_tss(M, from = from)
  C <- round(P * scale)
  dimnames(C) <- dimnames(M)
  return(C)
}

to_tss <- function(M, from = NULL) {
  M <- as.matrix(M)
  if (is.null(from)) from <- detect_input_form(M)
  if (from %in% c("counts", "positive", "tss")) {
    M[!is.finite(M)] <- 0
    M[M < 0] <- 0
    P <- .normalize_tss(M)
    dimnames(P) <- dimnames(M)
    return(P)
  }
  if (from == "log") {
    P <- if (all(M >= 0, na.rm = TRUE)) expm1(M) else exp(M)
    P[!is.finite(P)] <- 0
    P[P < 0] <- 0
    P <- .normalize_tss(P)
    dimnames(P) <- dimnames(M)
    return(P)
  }
  if (from == "clr") {
    M[!is.finite(M)] <- -Inf
    row_max <- apply(M, 1, function(x) {
      xm <- max(x, na.rm = TRUE)
      if (!is.finite(xm)) 0 else xm
    })
    M <- sweep(M, 1, row_max, "-")
    P <- exp(M)
    P[!is.finite(P)] <- 0
    P <- .normalize_tss(P)
    dimnames(P) <- dimnames(M)
    return(P)
  }
  stop("to_tss: unknown 'from'=", from)
}

to_log <- function(M, from = NULL, pseudo_min = 1e-6) {
  M <- as.matrix(M)
  if (is.null(from)) from <- detect_input_form(M)
  
  if (from == "log") return(M)
  
  if (from %in% c("tss","positive","counts", "clr")) {
    P <- to_tss(M, from = from)
    nz <- P[P > 0]
    if (!length(nz)) stop("to_log: no positive entries")
    eps <- max(min(nz) * 0.65, pseudo_min)
    P[P == 0] <- eps
    return(log(P))
  }
  
  stop("to_log: unknown 'from'=", from)
}

to_clr <- function(M, from = NULL, pseudo_min = 1e-6) {
  M <- as.matrix(M)
  if (is.null(from)) from <- detect_input_form(M)
  if (from == "clr") return(M)
  C <- to_tss(M, from = from)
  C[!is.finite(C)] <- 0
  C[C < 0] <- 0
  if (any(C == 0)) {
    C <- t(apply(C, 1, function(r) {
      if (all(r == 0)) return(rep(pseudo_min, length(r)))
      nz <- r[r > 0]
      eps <- max(pseudo_min, 0.65 * min(nz))
      r[r == 0] <- eps
      r
    }))
  }
  L <- log(C)
  out <- sweep(L, 1, rowMeans(L), "-")
  dimnames(out) <- dimnames(M)
  out
}

# ---------------------------
# Writer: emits BOTH TSS and CLR with suffixes into output_folder
# ---------------------------
write_tss_clr <- function(method, native, native_type, filename) {
  base <- sub("\\.csv$", "", filename, ignore.case = TRUE)
  # --- TSS ---
  tss <- to_tss(native, from = native_type)
  post_summary(tss, paste0("🔄 ", method, " (TSS)"))
  nm_tss <- basename(file.path(output_folder, paste0(base, "_tss.csv")))
  t0 <- start_step(paste0("Write ", nm_tss))
  write.csv(tss, file.path(output_folder, paste0(base, "_tss.csv")), row.names = FALSE)
  ok_step(paste0("Write ", nm_tss), t0)
  
  # --- CLR ---
  clr <- to_clr(native, from = native_type)
  post_summary(clr, paste0("🔄 ", method, " (CLR)"))
  nm_clr <- basename(file.path(output_folder, paste0(base, "_clr.csv")))
  t0 <- start_step(paste0("Write ", nm_clr))
  write.csv(clr, file.path(output_folder, paste0(base, "_clr.csv")), row.names = FALSE)
  ok_step(paste0("Write ", nm_clr), t0)
}

# ---------------------------
# Run
# ---------------------------
run_main <- TRUE
if (exists("PREPROCESS_SKIP_MAIN", inherits = TRUE)) {
  run_main <- !isTRUE(get("PREPROCESS_SKIP_MAIN", inherits = TRUE))
}

if (isTRUE(run_main)) {
  say("▶ Converting to TSS & CLR")
  say("Output folder: ", normalizePath(output_folder, winslash = "/"))
  say("Input matrix : ", normalizePath(matrix_path,  winslash = "/"))

  # Prepare metadata: keep a text copy for plotting and write numeric encodings
  # (including target_binary) into metadata.csv for correction.
  prepare_metadata_outputs(output_folder)

  # Prefer header-less numeric matrix; fall back to headered if needed
  read_matrix_guess <- function(p) {
    opt1 <- tryCatch(utils::read.csv(p, header = FALSE, check.names = FALSE), error = function(e) NULL)
    opt2 <- tryCatch(utils::read.csv(p, header = TRUE,  check.names = FALSE), error = function(e) NULL)
    score <- function(df) { if (is.null(df)) return(-Inf); mean(vapply(df, is.numeric, TRUE)) }
    if (!is.null(opt1) && score(opt1) >= score(opt2)) return(opt1)
    if (!is.null(opt2)) return(opt2)
    stop("Failed to read matrix: ", p)
  }

  uploaded_mat <- read_matrix_guess(matrix_path)
  check_table(uploaded_mat, "uploaded_mat", allow_negative = TRUE)
  input_form <- detect_input_form(uploaded_mat)
  say("ℹ️ Detected input form: ", input_form)

  base_M <- as.matrix(uploaded_mat)
  write_tss_clr("INPUT", base_M, input_form, basename(matrix_path))

  say("🎉 Done. Wrote _tss and _clr files into: ", normalizePath(output_folder, winslash = "/"))
}
