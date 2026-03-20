#' Copy files into batch folders using `fs`
#'
#' Copies files listed in a data frame into destination subfolders of fixed
#' size. This is useful when a large image collection must be reorganized into
#' evenly sized groups (for example, 250 files per folder) while preserving a
#' record of the operation.
#'
#' The input data frame must contain at least two columns:
#'
#' - `id`: unique or semi-unique identifier for each file
#' - `path`: source path to the file on disk
#'
#' Files are assigned to folders according to their row order in `df`.
#'
#' @param df A `data.frame` containing at least the columns `id` and `path`.
#' @param output_dir Character scalar. Root directory where batch folders
#'   will be created.
#' @param batch_size Integer scalar. Number of files per destination folder.
#'   Default is `250`.
#' @param folder_prefix Character scalar. Prefix used to name batch folders.
#'   Default is `"batch_"`.
#' @param rename_with_id Logical. If `TRUE` (default), output file names are
#'   prefixed with `id` to reduce the risk of name collisions.
#' @param overwrite Logical. If `TRUE`, existing files in the destination are
#'   overwritten. Default is `FALSE`.
#' @param dry_run Logical. If `TRUE`, no files are copied, but the returned
#'   report shows the intended destination structure. Default is `FALSE`.
#' @param verbose Logical. If `TRUE`, prints progress messages. Default is
#'   `TRUE`.
#'
#' @details
#' The function creates one folder per batch using zero-padded sequential
#' numbering, for example:
#'
#' - `batch_001`
#' - `batch_002`
#' - `batch_003`
#'
#' Destination file names are generated from the source file name. When
#' `rename_with_id = TRUE`, the output name becomes:
#'
#' `"{id}_{original_filename}"`
#'
#' If duplicates remain within a batch, they are disambiguated with
#' [base::make.unique()].
#'
#' The function returns a data frame describing the outcome for each row,
#' including whether the source existed, the assigned batch, the destination
#' path, and the copy status.
#'
#' @return
#' A `data.frame` with one row per valid input file and the following columns:
#'
#' \describe{
#'   \item{id}{Identifier from the input data frame.}
#'   \item{path}{Original source path.}
#'   \item{source_exists}{Logical; whether the source file existed.}
#'   \item{batch_id}{Integer batch number assigned to the file.}
#'   \item{batch_folder_name}{Name of the destination batch folder.}
#'   \item{dest_file_name}{Destination file name.}
#'   \item{dest_path}{Full destination path.}
#'   \item{copied}{Logical indicating whether the copy succeeded. `NA` in
#'   `dry_run` mode.}
#'   \item{status}{Character status label such as `"copied"`,
#'   `"source_not_found"`, `"destination_exists_not_overwritten"`,
#'   `"copy_failed"`, or `"dry_run"`.}
#' }
#'
#' @importFrom fs dir_create file_copy file_exists path path_file
#'
#' @examples
#' # Create a temporary source directory with example files
#' src_dir <- file.path(tempdir(), "example_source")
#' out_dir <- file.path(tempdir(), "example_output")
#'
#' dir.create(src_dir, recursive = TRUE, showWarnings = FALSE)
#' dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
#'
#' file1 <- file.path(src_dir, "img1.jpg")
#' file2 <- file.path(src_dir, "img2.jpg")
#' file3 <- file.path(src_dir, "img3.jpg")
#'
#' writeBin(charToRaw("a"), file1)
#' writeBin(charToRaw("b"), file2)
#' writeBin(charToRaw("c"), file3)
#'
#' df <- data.frame(
#'   id = c("A01", "A02", "A03"),
#'   path = c(file1, file2, file3),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Dry run: inspect the planned organization without copying
#' report_dry <- copy_in_batches(
#'   df = df,
#'   output_dir = out_dir,
#'   batch_size = 2,
#'   dry_run = TRUE,
#'   verbose = FALSE
#' )
#'
#' report_dry[, c("id", "batch_folder_name", "dest_file_name", "status")]
#'
#' # Actual copy
#' report <- copy_in_batches(
#'   df = df,
#'   output_dir = out_dir,
#'   batch_size = 2,
#'   dry_run = FALSE,
#'   verbose = FALSE
#' )
#'
#' report[, c("id", "dest_path", "status")]
#'
#' @export

copy_in_batches <- function(
  df,
  output_dir,
  batch_size = 250L,
  folder_prefix = "batch_",
  rename_with_id = TRUE,
  overwrite = FALSE,
  dry_run = FALSE,
  verbose = TRUE
) {
  # ----------------------------------------------------------
  # 1. Validate input
  # ----------------------------------------------------------
  if (!is.data.frame(df)) {
    stop("`df` must be a data.frame.", call. = FALSE)
  }

  required_cols <- c("id", "path")
  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns in `df`: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (
    !is.character(output_dir) || length(output_dir) != 1L || is.na(output_dir)
  ) {
    stop("`output_dir` must be a non-missing character scalar.", call. = FALSE)
  }

  if (
    !is.numeric(batch_size) ||
      length(batch_size) != 1L ||
      is.na(batch_size) ||
      batch_size <= 0
  ) {
    stop("`batch_size` must be a positive integer.", call. = FALSE)
  }

  if (
    !is.character(folder_prefix) ||
      length(folder_prefix) != 1L ||
      is.na(folder_prefix)
  ) {
    stop(
      "`folder_prefix` must be a non-missing character scalar.",
      call. = FALSE
    )
  }

  if (
    !is.logical(rename_with_id) ||
      length(rename_with_id) != 1L ||
      is.na(rename_with_id)
  ) {
    stop("`rename_with_id` must be a single TRUE/FALSE value.", call. = FALSE)
  }

  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("`overwrite` must be a single TRUE/FALSE value.", call. = FALSE)
  }

  if (!is.logical(dry_run) || length(dry_run) != 1L || is.na(dry_run)) {
    stop("`dry_run` must be a single TRUE/FALSE value.", call. = FALSE)
  }

  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be a single TRUE/FALSE value.", call. = FALSE)
  }

  batch_size <- as.integer(batch_size)

  # ----------------------------------------------------------
  # 2. Normalize working data
  # ----------------------------------------------------------
  x <- df[, required_cols, drop = FALSE]
  x$id <- as.character(x$id)
  x$path <- as.character(x$path)

  valid_rows <- !(is.na(x$id) | is.na(x$path) | x$id == "" | x$path == "")

  if (!all(valid_rows)) {
    if (verbose) {
      message(
        sum(!valid_rows),
        " rows were dropped because `id` or `path` was missing/empty."
      )
    }
    x <- x[valid_rows, , drop = FALSE]
  }

  if (nrow(x) == 0L) {
    stop(
      "No valid rows available after filtering missing/empty `id` or `path`.",
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # 3. Source existence
  # ----------------------------------------------------------
  x$source_exists <- fs::file_exists(x$path)

  if (verbose) {
    message("Valid rows: ", nrow(x))
    message("Existing source files: ", sum(x$source_exists))
    message("Missing source files: ", sum(!x$source_exists))
  }

  # ----------------------------------------------------------
  # 4. Assign batches
  # ----------------------------------------------------------
  n <- nrow(x)
  x$index <- seq_len(n)
  x$batch_id <- ceiling(x$index / batch_size)

  n_batches <- max(x$batch_id)
  batch_digits <- max(3L, nchar(as.character(n_batches)))

  x$batch_folder_name <- sprintf(
    paste0(folder_prefix, "%0", batch_digits, "d"),
    x$batch_id
  )

  x$batch_folder_path <- fs::path(output_dir, x$batch_folder_name)

  # ----------------------------------------------------------
  # 5. Create destination directories
  # ----------------------------------------------------------
  fs::dir_create(output_dir)

  if (!dry_run) {
    unique_dirs <- unique(x$batch_folder_path)
    for (d in unique_dirs) {
      fs::dir_create(d)
    }
  }

  # ----------------------------------------------------------
  # 6. Build destination file names
  # ----------------------------------------------------------
  x$dest_file_name <- NA_character_

  split_idx <- split(seq_len(nrow(x)), x$batch_id)

  for (idx in split_idx) {
    x$dest_file_name[idx] <- .build_destination_names_fs(
      ids = x$id[idx],
      paths = x$path[idx],
      rename_with_id = rename_with_id
    )
  }

  x$dest_path <- fs::path(x$batch_folder_path, x$dest_file_name)

  # ----------------------------------------------------------
  # 7. Copy files
  # ----------------------------------------------------------
  x$copied <- FALSE
  x$status <- NA_character_

  for (i in seq_len(nrow(x))) {
    src <- x$path[i]
    dst <- x$dest_path[i]

    if (!x$source_exists[i]) {
      x$status[i] <- "source_not_found"
      next
    }

    if (fs::file_exists(dst) && !overwrite) {
      x$status[i] <- "destination_exists_not_overwritten"
      next
    }

    if (dry_run) {
      x$copied[i] <- NA
      x$status[i] <- "dry_run"
      next
    }

    ok <- tryCatch(
      {
        fs::file_copy(src, dst, overwrite = overwrite)
        TRUE
      },
      error = function(e) FALSE
    )

    x$copied[i] <- ok
    x$status[i] <- if (ok) "copied" else "copy_failed"
  }

  # ----------------------------------------------------------
  # 8. Return report
  # ----------------------------------------------------------
  out <- x[, c(
    "id",
    "path",
    "source_exists",
    "batch_id",
    "batch_folder_name",
    "dest_file_name",
    "dest_path",
    "copied",
    "status"
  )]

  rownames(out) <- NULL
  out
}
