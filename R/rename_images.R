#' Rename Images Based on EXIF Data
#'
#' This function processes images stored in subdirectories, extracts EXIF metadata,
#' and renames them based on their file modification date, camera folder name, and subfolder.
#'
#' @param main_dir Character. Path to the main directory containing camera folders.
#' @param output_dir Character. Path to the directory where renamed images will be stored.
#' @return A tibble summarizing the renamed files and their paths.
#' @importFrom exifr read_exif
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
      images <- list.files(subfolder,
                           full.names = TRUE,
                           pattern = "\\.(jpg|jpeg|png|tiff|bmp)$",
                           ignore.case = TRUE)

      for (image in images) {
        # Read EXIF data
        exif_data <- tryCatch(
          exifr::read_exif(image),
          error = function(e) NULL
        )

        # Extract the file modification date or assign a default
        date_taken <- if (!is.null(exif_data) &&
                          !is.null(exif_data$FileModifyDate[1])) {
          # Clean date format for file names
          gsub(":", "-", gsub(" ", "_", exif_data$FileModifyDate[1]))
        } else {
          "unknown_date"
        }

        # Create new name for the file
        image_name <- basename(image)
        new_name <- paste(date_taken,
                          camera_name, basename(subfolder),
                          image_name, sep = "_")
        new_path <- file.path(camera_output_dir, new_name)

        # Copy the file and log the operation
        if (file.copy(image, new_path)) {
          renamed_files <- tibble::add_row(renamed_files,
                                           original_path = image,
                                           new_path = new_path)
        }
      }
    }
  }

  return(renamed_files)
}

# #install.packages("exifr")
# library(exifr)
#
#
# # 12 a 26 de agosto ---------------------------------------------------------------------------------------------------------------------------------------
# # Definir el directorio principal donde están las carpetas de las cámaras
# main_dir <- "C:/IA_fish_12_26_ago_2024"  # Cambiar esto por la ruta correcta a las imágenes originales
#
# # Definir el directorio donde se almacenarán las imágenes renombradas
# output_dir <- "C:/rename_IA_fish_12_26_ago_2024"  # Cambia esto por la ruta a las imágenes renombradas
#
# # Obtener la lista de carpetas de cámaras (cada cámara en una carpeta)
# cameras <- list.dirs(main_dir, recursive = FALSE)
#
# # Función para copiar y renombrar archivos sin modificar los originales
# rename_images_with_date <- function(camera_dir) {
#   # Obtener el nombre de la cámara a partir del nombre de la carpeta
#   camera_name <- basename(camera_dir)
#
#   # Crear una carpeta en el directorio de salida para esta cámara
#   camera_output_dir <- file.path(output_dir, camera_name)
#   if (!dir.exists(camera_output_dir)) {
#     dir.create(camera_output_dir)
#   }
#
#   # Obtener todas las subcarpetas de cada cámara
#   subfolders <- list.dirs(camera_dir, recursive = FALSE)
#
#   for (subfolder in subfolders) {
#     # Listar todas las imágenes en la subcarpeta
#     images <- list.files(subfolder, full.names = TRUE)
#
#     for (image in images) {
#       # Leer los metadatos EXIF de la imagen
#       exif_data <- read_exif(image)
#
#       # Extraer la fecha de la imagen si está disponible
#       if (!is.null(exif_data$FileModifyDate[1])) {
#         date_taken <- exif_data$FileModifyDate[1]
#         # Limpiar el formato de la fecha para que sea adecuado para un nombre de archivo
#         date_taken <- gsub(":", "-", date_taken)
#         date_taken <- gsub(" ", "_", date_taken)
#       } else {
#         # Si no se encuentra la fecha, se utiliza una marca de tiempo genérica
#         date_taken <- "unknown_date"
#       }
#
#       # Obtener el nombre original del archivo
#       image_name <- basename(image)
#
#       # Crear el nuevo nombre incorporando la fecha, el nombre de la cámara y la subcarpeta
#       new_name <- paste(date_taken, camera_name, basename(subfolder), image_name, sep = "_")
#
#       # Crear la ruta completa del nuevo archivo en el directorio de salida
#       new_path <- file.path(camera_output_dir, new_name)
#
#       # Copiar el archivo original al nuevo directorio con el nuevo nombre
#       file.copy(image, new_path)
#     }
#   }
# }
#
# # Aplicar la función a cada carpeta de cámara
# for (camera in cameras) {
#   rename_images_with_date(camera)
# }
#
#
# # 28 agosto - 20 septiembre -------------------------------------------------------------------------------------------------------------------------------
#
# # Definir el directorio principal donde están las carpetas de las cámaras
# main_dir <- "C:/IA_fish_28go_20sep_2024"  # Cambiar esto por la ruta correcta a las imágenes originales
#
# # Definir el directorio donde se almacenarán las imágenes renombradas
# output_dir <- "C:/rename_IA_fish_28go_20sep_2024"  # Cambia esto por la ruta a las imágenes renombradas
#
# # Obtener la lista de carpetas de cámaras (cada cámara en una carpeta)
# cameras <- list.dirs(main_dir, recursive = FALSE)
#
# # Función para copiar y renombrar archivos sin modificar los originales
# rename_images_with_date <- function(camera_dir) {
#   # Obtener el nombre de la cámara a partir del nombre de la carpeta
#   camera_name <- basename(camera_dir)
#
#   # Crear una carpeta en el directorio de salida para esta cámara
#   camera_output_dir <- file.path(output_dir, camera_name)
#   if (!dir.exists(camera_output_dir)) {
#     dir.create(camera_output_dir)
#   }
#
#   # Obtener todas las subcarpetas de cada cámara
#   subfolders <- list.dirs(camera_dir, recursive = FALSE)
#
#   for (subfolder in subfolders) {
#     # Listar todas las imágenes en la subcarpeta
#     images <- list.files(subfolder, full.names = TRUE)
#
#     for (image in images) {
#       # Leer los metadatos EXIF de la imagen
#       exif_data <- read_exif(image)
#
#       # Extraer la fecha de la imagen si está disponible
#       if (!is.null(exif_data$FileModifyDate[1])) {
#         date_taken <- exif_data$FileModifyDate[1]
#         # Limpiar el formato de la fecha para que sea adecuado para un nombre de archivo
#         date_taken <- gsub(":", "-", date_taken)
#         date_taken <- gsub(" ", "_", date_taken)
#       } else {
#         # Si no se encuentra la fecha, se utiliza una marca de tiempo genérica
#         date_taken <- "unknown_date"
#       }
#
#       # Obtener el nombre original del archivo
#       image_name <- basename(image)
#
#       # Crear el nuevo nombre incorporando la fecha, el nombre de la cámara y la subcarpeta
#       new_name <- paste(date_taken, camera_name, basename(subfolder), image_name, sep = "_")
#
#       # Crear la ruta completa del nuevo archivo en el directorio de salida
#       new_path <- file.path(camera_output_dir, new_name)
#
#       # Copiar el archivo original al nuevo directorio con el nuevo nombre
#       file.copy(image, new_path)
#     }
#   }
# }
#
# # Aplicar la función a cada carpeta de cámara
# for (camera in cameras) {
#   rename_images_with_date(camera)
# }
#
