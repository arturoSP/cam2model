#' Process Camera Trap Metadata
#'
#' Extracts EXIF metadata from images in a directory, processes the 'UserComment' column,
#' and saves the resulting metadata to a CSV file.
#'
#' @param output_dir Character. Path to the main directory containing camera folders with renamed images.
#' @param output_file Character. Path to the output CSV file where metadata will be saved.
#' @return A tibble with the processed metadata.
#' @importFrom exifr read_exif
#' @importFrom dplyr bind_rows mutate select left_join relocate
#' @importFrom tidyr separate_wider_delim pivot_wider unnest
#' @importFrom stringr str_split str_replace_all str_sub
#' @importFrom lubridate ymd_hms hour minute date
#' @importFrom readr write_csv
#' @export
#' @examples
#' # Define paths
#' output_dir <- "~/Descargas/test1"
#' output_file <- "12_ago_2024_metadata.csv"
#'
#' # Run the function
#' metadata <- process_metadata(output_dir, output_file, UserComment = FALSE)
#'
#' print(metadata)
#'

process_metadata <- function(output_dir, output_file) {
  # Validations
  if (!dir.exists(output_dir)) {
    stop("The main directory does not exist: ", output_dir)
  }
  if (missing(output_file)) {
    stop("Please provide an output file path.")
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
    file = paste(output_dir, output_file, sep = "/")
  )

  # Return the processed metadata
  return(processed_metadata)
}
