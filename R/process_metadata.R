#' Process Camera Trap Metadata
#'
#' Extracts EXIF metadata from images in a directory, processes the
#' `UserComment` column, and saves the resulting metadata to a CSV file.
#'
#' @param output_dir Character. Path to the main directory containing camera
#'   folders with renamed images.
#' @param output_file Character. Relative file path (from `output_dir`) for the
#'   output CSV where metadata will be saved. The parent directory for this file
#'   must already exist inside `output_dir`.
#' @param UserComment Logical. Default `FALSE`. When `TRUE`, attempts to parse
#'   and expand the EXIF `UserComment` field into additional metadata columns.
#'   Set to `FALSE` if parsing fails for your files.
#'
#' @details
#' **Expected minimum input columns (internal contract):** none. This function
#' reads EXIF tags directly from image files under `output_dir` and appends a
#' `Camera` column derived from each first-level folder name.
#'
#' **Produced columns:** varies by EXIF availability. Output always includes
#' available EXIF fields plus `Camera`; when `UserComment = TRUE` and parsing is
#' successful, it may also include `Image_dttm`, `File_date`, `File_hms`,
#' `File_hour`, `File_minute`, and parsed fields such as `temp`/`bLuma`.
#'
#' **I/O side-effects:** writes one CSV to `file.path(output_dir, output_file)`.
#' The function does not create missing parent directories for `output_file`.
#'
#' @return A tibble with processed metadata. Typical contract columns used by
#' downstream functions include `Directory` and `FileName` (required by
#' [process_training_images()]).
#' @seealso [rename_images()], [generate_plot()], [process_training_images()],
#'   [copy_in_batches()]
#' @importFrom exifr read_exif
#' @importFrom dplyr bind_rows mutate select left_join relocate
#' @importFrom tidyr separate_wider_delim pivot_wider unnest
#' @importFrom stringr str_split str_replace_all str_sub
#' @importFrom lubridate ymd_hms hour minute date
#' @importFrom readr write_csv
#' @export
#' @examples
#' \dontrun{
#' # Requires real image files with EXIF metadata.
#' renamed_dir <- tempfile("cam2model_renamed_")
#' dir.create(renamed_dir, recursive = TRUE)
#' dir.create(file.path(renamed_dir, "camera_A"), recursive = TRUE)
#'
#' # Copy real camera-trap images into `renamed_dir/camera_A` before running.
#' meta <- process_metadata(
#'   output_dir = renamed_dir,
#'   output_file = "metadata.csv",
#'   UserComment = FALSE
#' )
#'
#' file.exists(file.path(renamed_dir, "metadata.csv"))
#' head(meta)
#' }

process_metadata <- function(output_dir, output_file, UserComment = FALSE) {
  # Validations
  if (!dir.exists(output_dir)) {
    stop("The main directory does not exist: ", output_dir)
  }
  if (missing(output_file) || !is.character(output_file) || length(output_file) != 1 || output_file == "") {
    stop("Please provide a non-empty output file path.")
  }
  if (fs::is_absolute_path(output_file)) {
    stop("`output_file` must be a relative path inside `output_dir`.")
  }

  output_path <- file.path(output_dir, output_file)
  output_parent <- dirname(output_path)
  if (!dir.exists(output_parent)) {
    stop("The output directory does not exist inside `output_dir`: ", output_parent)
  }

  # Helper function: Parse UserComment
  parse_user_comment <- function(df) {
    if ("UserComment" %in% colnames(df) && any(!is.na(df$UserComment))) {
      user_comment_data <- df |>
        mutate(UserComment = strsplit(UserComment, ",")) |>
        tidyr::unnest(UserComment) |>
        tidyr::separate_wider_delim(
          UserComment,
          delim = ":",
          names = c("key", "value"),
          too_few = "align_start"
        ) |>
        tidyr::pivot_wider(
          names_from = key,
          values_from = value,
          values_fn = list(value = ~ paste(unique(.), collapse = ","))
        ) |>
        mutate(
          Image_dttm = ymd_hms(str_replace_all(
            str_sub(FileName, 1, 19),
            c("-" = ":", "_" = " ")
          )),
          File_hms = format(Image_dttm, "%H:%M:%S"),
          File_hour = hour(Image_dttm),
          File_minute = minute(Image_dttm),
          File_date = date(Image_dttm)
        ) |>
        relocate(
          c(Image_dttm, File_date, File_hms, File_hour, File_minute),
          .after = FileModifyDate
        ) |>
        select(
          FileName,
          HV1.1.9.4,
          ID,
          moon,
          temp,
          bLuma,
          sEV,
          cEv,
          batAdc,
          batPer,
          Image_dttm,
          File_date,
          File_hms,
          File_hour,
          File_minute
        )

      df <- df |>
        select(-UserComment) |>
        left_join(user_comment_data, by = "FileName") |>
        relocate(
          c(Image_dttm, File_date, File_hms, File_hour, File_minute),
          .after = FileModifyDate
        ) |>
        select(-c(FileModifyDate, FileAccessDate, FileInodeChangeDate))
    }
    return(df)
  }

  # Extract Metadata
  cameras <- list.dirs(output_dir, recursive = FALSE)

  extract_camera_metadata <- function(camera_dir) {
    camera_name <- basename(camera_dir)
    images <- list.files(
      camera_dir,
      full.names = TRUE,
      pattern = "\\.(jpg|png)$",
      ignore.case = TRUE
    )

    if (length(images) > 0) {
      metadata <- exifr::read_exif(images)
      metadata <- dplyr::mutate(metadata, Camera = camera_name)
      return(metadata)
    } else {
      return(data.frame()) # Return empty data frame if no images found
    }
  }

  all_metadata <- dplyr::bind_rows(lapply(cameras, extract_camera_metadata))

  # Process UserComment
  if (UserComment) {
    processed_metadata <- parse_user_comment(all_metadata)
  } else {
    processed_metadata <- all_metadata
  }

  # Save to CSV
  readr::write_csv(
    processed_metadata,
    file = output_path
  )

  # Return the processed metadata
  return(processed_metadata)
}
