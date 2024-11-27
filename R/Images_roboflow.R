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
#' @param date_cutoff Date. Maximum date to include images (default is `Sys.Date()`).
#' @param start_hour Numeric. Start of daytime hours (default is 6).
#' @param end_hour Numeric. End of daytime hours (default is 18).
#' @return A tibble with the sampled images' metadata.
#' @importFrom dplyr filter group_by summarise select mutate reframe
#' @importFrom readr read_csv write_csv
#' @importFrom fs dir_create
#' @export
#' @examples
#' # Define parámetros
#' metadata_file <- "12_26_ago_2024_images_metadata.csv"
#' dest_dir <- "C:/train.images_ago_2024"
#' output_sample_file <- "ago_2024_images_sampled.csv"
#' output_annotation_file <- "images_ago_2024.csv"
#'
#' # Ejecutar la función
#' sampled_images <- process_training_images(
#'   metadata_file = metadata_file,
#'   dest_dir = dest_dir,
#'   output_sample_file = output_sample_file,
#'   output_annotation_file = output_annotation_file,
#'   sample_proportion = 0.5,
#'   date_cutoff = as.Date("2024-08-26"),
#'   start_hour = 6,
#'   end_hour = 18
#' )
#'
#' # Revisar resultados
#' print(sampled_images)

#'
process_training_images <- function(metadata_file,
                                    dest_dir,
                                    output_sample_file,
                                    output_annotation_file,
                                    sample_proportion = 0.5,
                                    date_cutoff = Sys.Date(), # este puede ser innecesario, debería hacerse a mano o hacerlo NULL y si existe filtrarlo
                                    start_hour = 6,
                                    end_hour = 18) {
  # Load metadata
  metadata <- read_csv(metadata_file)

  # Filter daytime images
  daytime_images <- metadata |>
    filter(File_date <= date_cutoff) |>
    filter(File_hour >= start_hour & File_hour <= end_hour) |>
    select(Directory, FileName, File_date, File_hour)

  # Count total images by directory
  num_images <- daytime_images |>
    group_by(Directory) |>
    summarise(images = n())

  # Calculate sample size
  samp_size <- round(sample_proportion * mean(num_images$images), 0)

  # Sample images
  sampled_images <- daytime_images |>
    group_by(Directory) |>
    reframe(FileName = sample(FileName, samp_size))

  # Save sampled metadata
  write_csv(sampled_images, file = output_sample_file)

  # Create destination directory if it doesn't exist
  if (!dir.exists(dest_dir)) {
    fs::dir_create(dest_dir)
  }

  # Ensure necessary columns are present
  if (!all(c("Directory", "FileName") %in% colnames(sampled_images))) {
    stop("The DataFrame 'sampled_images' must contain 'Directory' and 'FileName' columns.")
  }

  # Copy sampled images to destination directory
  for (i in seq_len(nrow(sampled_images))) {
    source_file <- file.path(sampled_images$Directory[i], sampled_images$FileName[i])
    dest_file <- file.path(dest_dir, sampled_images$FileName[i])
    file.copy(source_file, dest_file, overwrite = TRUE)
  }

  # Create annotation file
  annotation_data <- sampled_images |>
    select(FileName) |>
    mutate(Annotation = NA, fish = NA, turtle = NA)

  write_csv(annotation_data, file = output_annotation_file, na = "")

  return(sampled_images)
}


# library(dplyr)
# library(tidyr)
# library(lubridate)
# library(stringr)
# library(ggplot2)
# library(readr)
# library(scales)
# library(fs)
# library(purrr)
#
#
# # 12 a 26 de agosto ---------------------------------------------------------------------------------------------------------------------------------------
# # Importar metadatos
# metadata <- read_csv("12_26_ago_2024_images_metadata.csv")
#
# # Imágenes para entrenamiento. Solo las diurnas entre 6:00h y 18:00h
# todas <- metadata|>
#   filter(File_date <= "2024-08-26")|>
#   filter(File_hour >= 6 & File_hour <= 18)|>
#   select(1,3,4)
#
# # total de imágenes
# num.images <- metadata|>
#   filter(File_date <= "2024-08-26")|>
#   filter(File_hour >= 6 & File_hour <= 18)|>
#   group_by(Directory)|>
#   summarise(images = length(Directory))
#
# # selección del 50 % de imágenes
# samp.size <- round(0.5*(mean(num.images$images)),0)
#
# train.images <- todas|>
#   group_by(Directory)|>
#   reframe(FileName = sample(FileName, samp.size))
#
# table(table(train.images$FileName) == 2) # Se confirma que no se repitieron imágenes
#
# write_csv(train.images, file = "ago_2024_images_sampled.csv")
#
# # train.images <- read.csv("ago_2024_images_sampled.csv")
#
# # Definir el directorio destino
# dest_dir <- "C:/train.images_ago_2024"
#
# # Crear la carpeta destino si no existe
# if (!dir.exists(dest_dir)) {
#   dir.create(dest_dir)
# }
#
# # Asegúrate de que el DataFrame `train.images` tenga las columnas `Directory` y `FileName`
# if (!all(c("Directory", "FileName") %in% colnames(train.images))) {
#   stop("El DataFrame 'train.images' debe contener las columnas 'Directory' y 'FileName'")
# }
#
# # Seleccionar las imágenes a copiar
#
# # Recorrer cada fila del DataFrame para copiar las imágenes
# for (i in 1:nrow(train.images)) {
#   # Construir la ruta completa del archivo fuente
#   source_file <- file.path(train.images$Directory[i], train.images$FileName[i])
#
#   # Construir la ruta completa del archivo destino
#   dest_file <- file.path(dest_dir, train.images$FileName[i])
#
#   # Copiar el archivo al directorio destino
#   file.copy(source_file, dest_file, overwrite = TRUE)
# }
#
#
# # Base de datos para control de procesamiento
#
# train.images|>
#   select(FileName)|>
#   mutate(Annotation = NA, fish = NA, turtle = NA)|>
#   write_csv(file = "images_ago_2024.csv", na = "")
#
#
#
# # 28 agosto - 20 septiembre -------------------------------------------------------------------------------------------------------------------------------
# # Importar metadatos
# metadata <- read_csv("8ago_20sep_2024_images_metadata.csv")
#
#
# metadata|>
#   filter(File_date >= "2024-08-28")|>
#   filter(File_date <= "2024-09-20")|>
#   ggplot(aes(x = File_hour, y = bLuma))+
#   geom_point(aes(colour = Camera))+
#   facet_wrap(~Camera)
#
# # Imágenes para entrenamiento. Solo las diurnas entre 6:00h y 18:00h
# todas <- metadata|>
#   filter(File_date >= "2024-08-28")|>
#   filter(File_date <= "2024-09-20")|>
#   filter(File_hour >= 6 & File_hour <= 18)|>
#   select(1,3,4)
#
# # total de imágenes
# num.images <- metadata|>
#   filter(File_date >= "2024-08-27")|>
#   filter(File_date <= "2024-09-20")|>
#   filter(File_hour >= 6 & File_hour <= 18)|>
#   group_by(Directory)|>
#   summarise(images = length(Directory))
#
# # selección del 50 % de imágenes
# samp.size <- round(0.5*(mean(num.images$images)),0)
#
# train.images <- todas|>
#   group_by(Directory)|>
#   reframe(FileName = sample(FileName, samp.size))
#
# table(table(train.images$FileName) == 2) # Se confirma que no se repitieron imágenes
#
# write_csv(train.images, file = "8ago_20sep_2024_images_sampled.csv")
#
# # train.images <- read.csv("ago_2024_images_sampled.csv")
#
# # Definir el directorio destino
# dest_dir <- "C:/train.images_ago_sep_2024"
#
# # Crear la carpeta destino si no existe
# if (!dir.exists(dest_dir)) {
#   dir.create(dest_dir)
# }
#
# # Asegúrate de que el DataFrame `train.images` tenga las columnas `Directory` y `FileName`
# if (!all(c("Directory", "FileName") %in% colnames(train.images))) {
#   stop("El DataFrame 'train.images' debe contener las columnas 'Directory' y 'FileName'")
# }
#
# # Seleccionar las imágenes a copiar
#
# # Recorrer cada fila del DataFrame para copiar las imágenes
# for (i in 1:nrow(train.images)) {
#   # Construir la ruta completa del archivo fuente
#   source_file <- file.path(train.images$Directory[i], train.images$FileName[i])
#
#   # Construir la ruta completa del archivo destino
#   dest_file <- file.path(dest_dir, train.images$FileName[i])
#
#   # Copiar el archivo al directorio destino
#   file.copy(source_file, dest_file, overwrite = TRUE)
# }
#
#
# # Base de datos para control de procesamiento
# # Eliminar los primeras 77 registros ya que son imágenes de la cámara dentro del saco
# # Eliminar los últimos 178 registros ya que son imágenes de la cámara dentro del saco
# # Estas imágenes ya fueron removidas de train.images_ago_seop_2024
#
# list.files(dest_dir)
#
# tibble(
#   FileName = list.files(dest_dir),
#   Annotation = NA, fish = NA, turtle = NA)|>
#   write_csv(file = "images_ago_sep_2024.csv", na = "")
#
#
# # xx <- train.images|>
# #   left_join(metadata)|>
# #   select(FileName, File_date, File_hour, File_minute)|>
# #   arrange(FileName)|>
# #   #arrange(File_date, File_hour, File_minute)|>
# #   mutate(Annotation = NA, fish = NA, turtle = NA)
#
#
#
#
#
