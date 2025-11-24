#' Process and Select Images for Training
#'
#' This function filters metadata to select daytime images for training, samples a subset of images,
#' copies the sampled images to a destination directory, and generates a CSV file for further annotation.
#'
#' @param metadata_file Character. Path to the CSV file containing metadata.
#' @param dest_dir Character. Path to the directory where the sampled images will be copied.
#' @param output_sample_file Character. Path to save the sampled images' metadata as a CSV file.
#' @param output_annotation_file Character. Path to save the annotation file.
#' @param sample_proportion Numeric. Proportion of images to sample (default is 0.5).
#' @param filter_by Unquoted column name giving the luminosity variable to be used for filtering
#' (e.g. `bLuma`)
#' @param min_lum Numeric. Lower luminosity threshold. Images with luminosity below this value are
#' excluded. If `NULL`, the minimum of `filter_by` in the data is used.
#' @param max_lum Numeric. Upper luminosity threshold. Images with luminosity above this value are
#' excluded. If `NULL`, the maximum of `filter_by` in the data is used.
#' @param min_per_dir Integer. Minimum number of images to sample per directory (default 1).
#' @param max_per_dir Integer. Maximum number of images to sample per directory (default Inf).
#'
#' @return A tibble with the sampled images' metadata.
#'
#' @importFrom dplyr filter group_by summarise select mutate n pull group_modify
#' @importFrom readr read_csv write_csv
#' @importFrom fs dir_create
#' @importFrom rlang as_name enquo
#' @export
#'
#' @examples
#' # Parameters
#' metadata_file <- "12_26_ago_2024_images_metadata.csv"
#' dest_dir <- "C:/train.images_ago_2024"
#' output_sample_file <- "ago_2024_images_sampled.csv"
#' output_annotation_file <- "images_ago_2024.csv"
#'
#' # Example call
#' sampled_images <- process_training_images(
#'   metadata_file = metadata_file,
#'   dest_dir = dest_dir,
#'   output_sample_file = output_sample_file,
#'   output_annotation_file = output_annotation_file,
#'   sample_proportion = 0.5,
#'   filter_by = bLuma
#' )
#'
#' print(sampled_images)

#'
process_training_images <- function(
  metadata_file,
  dest_dir,
  output_sample_file,
  output_annotation_file,
  sample_proportion = 0.5,
  filter_by,
  min_threshold = NULL,
  max_threshold = NULL,
  min_per_dir = 1,
  max_per_dir = Inf
) {
  # Load metadata
  metadata <- read_csv(metadata_file)

  # Use filtering column and check that it exists
  filter_by_quo <- rlang::enquo(filter_by)
  filter_by_name <- rlang::quo_name(filter_by_quo)

  if (!filter_by_name %in% names(metadata)) {
    stop(
      "Column '",
      filter_by_name,
      "' not found in metadata.\nAvailable columns are: ",
      paste(names(metadata), collapse = ", "),
      call. = FALSE
    )
  }

  # Reads available values in the filtering column
  threshold_values <- dplyr::pull(metadata, !!filter_by_quo)

  if (all(is.na(threshold_values))) {
    stop(
      "Column '",
      filter_by_name,
      "' only contains NA values.",
      call. = FALSE
    )
  }

  # Helper function for checking the threshold values
  col_is_numeric <- is.numeric(threshold_values)
  col_is_date <- inherits(threshold_values, "Date")
  col_is_posix <- inherits(threshold_values, "POSIXt")
  col_is_char <- is.character(threshold_values)

  check_threshold_type <- function(th, which_th) {
    if (is.null(th)) {
      return(invisible(TRUE))
    }
    if (col_is_numeric && !is.numeric(th)) {
      stop(
        which_th,
        " must be numeric because '",
        filter_by_name,
        "' is numeric.",
        call. = FALSE
      )
    }
    if (col_is_date && !inherits(th, "Date")) {
      stop(
        which_th,
        " must be of class 'Date' because '",
        filter_by_name,
        "' is a Date column.",
        call. = FALSE
      )
    }
    if (col_is_posix && !inherits(th, "POSIXt")) {
      stop(
        which_th,
        " must be POSIXt (POSIXct/POSIXlt) because '",
        filter_by_name,
        "' is POSIXt.",
        call. = FALSE
      )
    }
    if (col_is_char && !is.character(th)) {
      stop(
        which_th,
        " must be character because '",
        filter_by_name,
        "' is character.",
        call. = FALSE
      )
    }
    invisible(TRUE)
  }

  check_threshold_type(min_threshold, "min_threshold")
  check_threshold_type(max_threshold, "max_threshold")

  # Fill the values for threshold in case of NULL
  if (is.null(min_threshold)) {
    min_threshold <- min(threshold_values, na.rm = TRUE)
  }
  if (is.null(max_threshold)) {
    max_threshold <- max(threshold_values, na.rm = TRUE)
  }

  # Check that min is smaller than max
  if (any(max_threshold < min_threshold, na.rm = TRUE)) {
    stop(
      "`max_threshold` must be greater than or equal to `min_threshold`.",
      call. = FALSE
    )
  }

  # Filter images
  daytime_images <- metadata[
    metadata[[filter_by_name]] >= min_threshold &
      metadata[[filter_by_name]] <= max_threshold,
    c("Directory", "FileName", "File_date", "File_hour")
  ]

  if (nrow(daytime_images) == 0) {
    stop(
      "No images passed the filter on '",
      filter_by_name,
      "' between ",
      min_threshold,
      " and ",
      max_threshold,
      ".",
      call. = FALSE
    )
  }

  # Count total images by directory
  num_images <- daytime_images |>
    group_by(Directory) |>
    summarise(images = n())

  # Per-directory proportional sampling
  sampled_images <- daytime_images |>
    dplyr::group_by(Directory) |>
    dplyr::group_modify(\(df, key) {
      n_dir <- nrow(df)
      # raw proportional size
      k <- floor(n_dir * sample_proportion)
      # enforce bounds and not exceed n_dir
      k <- max(min_per_dir, k)
      k <- min(k, n_dir, max_per_dir)
      # slice_sample handles k == n_dir
      df |>
        dplyr::slice_sample(n = k)
    }) |>
    dplyr::ungroup() |>
    dplyr::select(Directory, FileName)

  # Create destination directory if it doesn't exist
  if (!dir.exists(dest_dir)) {
    fs::dir_create(dest_dir)
  }

  # Save sampled metadata
  if (length(grep("*.csv", output_sample_file)) == 0) {
    output_sample_file <- paste0(output_sample_file, ".csv")
  }

  readr::write_csv(
    sampled_images,
    file = paste(dest_dir, output_sample_file, sep = "/")
  )

  # Ensure necessary columns are present
  if (!all(c("Directory", "FileName") %in% colnames(sampled_images))) {
    stop(
      "The DataFrame 'sampled_images' must contain 'Directory' and 'FileName' columns."
    )
  }

  # Copy sampled images to destination directory
  for (i in seq_len(nrow(sampled_images))) {
    source_file <- file.path(
      sampled_images$Directory[i],
      sampled_images$FileName[i]
    )
    dest_file <- file.path(dest_dir, sampled_images$FileName[i])
    file.copy(source_file, dest_file, overwrite = TRUE)
  }

  # Create annotation file
  annotation_data <- sampled_images |>
    select(FileName) |>
    mutate(Annotation = NA, fish = NA, turtle = NA)

  if (length(grep("*.csv", output_annotation_file)) == 0) {
    output_annotation_file <- paste0(output_annotation_file, ".csv")
  }
  readr::write_csv(
    annotation_data,
    file = paste(dest_dir, output_annotation_file, sep = "/"),
    na = ""
  )

  return(sampled_images)
}
