#' Rename Images Based on EXIF Data
#'
#' This function processes images stored in subdirectories, extracts EXIF metadata,
#' and renames them based on their file modification date, camera folder name, and subfolder.
#'
#' @param main_dir Character. Path to the main directory containing camera folders.
#' @param output_dir Character. Path to the directory where renamed images will be stored.
#' @return A tibble summarizing the renamed files and their paths.
#' @importFrom exifr read_exif
#' @importFrom tibble tibble add_row
#' @export
#' @examples
#' # Define input and output paths
#' main_dir <- "~/Descargas/DSCF0099"
#' output_dir <- "~/Descargas/test1"
#'
#' # Run the function
#' renamed_files <- rename_images(main_dir, output_dir)
#'
#' print(renamed_files)
#'

rename_images <- function(main_dir, output_dir) {
  # Validations
  if (!dir.exists(main_dir)) {
    stop("The main directory does not exist: ", main_dir)
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Get a list of camera directories
  cameras <- list.dirs(main_dir, recursive = FALSE)
  if (length(cameras) == 0) {
    stop("No camera directories found in the main directory: ", main_dir)
  }

  # Initialize a log of renamed files
  renamed_files <- tibble::tibble(
    original_path = character(),
    new_path = character()
  )

  # Iterate over camera directories
  for (camera_dir in cameras) {
    # Extract camera folder name
    camera_name <- basename(camera_dir)

    # Create output folder for this camera
    camera_output_dir <- file.path(output_dir, camera_name)
    if (!dir.exists(camera_output_dir)) {
      dir.create(camera_output_dir)
    }

    # Get all subfolders in the camera directory
    subfolders <- list.dirs(camera_dir, recursive = FALSE)

    for (subfolder in subfolders) {
      # List all image files in the subfolder
      images <- list.files(
        subfolder,
        full.names = TRUE,
        pattern = "\\.(jpg|jpeg|png|tiff|bmp)$",
        ignore.case = TRUE
      )

      for (image in images) {
        # Read EXIF data
        exif_data <- tryCatch(
          exifr::read_exif(image),
          error = function(e) NULL
        )

        # Extract the file modification date or assign a default
        date_taken <- if (
          !is.null(exif_data) &&
            !is.null(exif_data$FileModifyDate[1])
        ) {
          # Clean date format for file names
          gsub(":", "-", gsub(" ", "_", exif_data$FileModifyDate[1]))
        } else {
          "unknown_date"
        }

        # Create new name for the file
        image_name <- basename(image)
        new_name <- paste(
          date_taken,
          camera_name,
          basename(subfolder),
          image_name,
          sep = "_"
        )
        new_path <- file.path(camera_output_dir, new_name)

        # Copy the file and log the operation
        if (file.copy(image, new_path)) {
          renamed_files <- tibble::add_row(
            renamed_files,
            original_path = image,
            new_path = new_path
          )
        }
      }
    }
  }

  return(renamed_files)
}
