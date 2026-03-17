#' Rename and Flatten Camera Trap Images from a Sampling Site
#'
#' This function recursively searches for image files inside a sampling-site
#' directory, extracts EXIF metadata when available, and copies the images into
#' a flattened output directory using informative file names.
#'
#' @param input_dir Character. Path to the root directory of a sampling site.
#' @param output_dir Character. Path to the directory where renamed images
#'   will be copied.
#' @param site_name Character or NULL. Name of the sampling site. If NULL,
#'   the basename of `input_dir` is used.
#' @param recursive Logical. Should image files be searched recursively?
#'   Default is TRUE.
#' @param extensions Character vector. Allowed image extensions.
#' @param keep_relative_path Logical. If TRUE, part of the relative path is
#'   included in the new file name to preserve provenance.
#' @param date_field Character. EXIF field to use for date extraction.
#'   Default is "FileModifyDate".
#'
#' @return A tibble with original and new file paths.
#' @importFrom exifr read_exif
#' @importFrom tibble tibble add_row
#' @export
rename_images <- function(
  input_dir,
  output_dir,
  site_name = NULL,
  recursive = TRUE,
  extensions = c("jpg", "jpeg", "png", "tif", "tiff"),
  keep_relative_path = TRUE,
  date_field = "FileModifyDate"
) {
  if (!dir.exists(input_dir)) {
    stop("`input_dir` does not exist: ", input_dir, call. = FALSE)
  }

  if (is.null(site_name)) {
    site_name <- basename(normalizePath(input_dir, winslash = "/"))
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # carpeta específica de salida para el sitio
  site_output_dir <- file.path(output_dir, site_name)
  if (!dir.exists(site_output_dir)) {
    dir.create(site_output_dir, recursive = TRUE)
  }

  # patrón de extensiones
  ext_pattern <- paste0("\\.(", paste(extensions, collapse = "|"), ")$")

  # listar imágenes en cualquier profundidad
  image_files <- list.files(
    path = input_dir,
    pattern = ext_pattern,
    recursive = recursive,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(image_files) == 0) {
    stop("No image files found in `input_dir`.", call. = FALSE)
  }

  results <- tibble::tibble(
    original_path = character(),
    relative_path = character(),
    camera_name = character(),
    datetime_used = character(),
    new_name = character(),
    new_path = character(),
    copied = logical()
  )

  for (image in image_files) {
    # Ruta relativa respecto al sitio
    rel_path <- sub(
      paste0(
        "^",
        gsub(
          "([.|()\\^{}+$*?]|\\[|\\]|\\\\)",
          "\\\\\\1",
          normalizePath(input_dir, winslash = "/")
        ),
        "/?"
      ),
      "",
      normalizePath(image, winslash = "/")
    )

    rel_parts <- strsplit(rel_path, "/", fixed = TRUE)[[1]]

    # Inferencia simple: la primera subcarpeta es la cámara
    camera_name <- if (length(rel_parts) > 1) rel_parts[1] else "unknown_camera"

    # Nombre original
    image_name <- basename(image)

    # Leer EXIF con seguridad
    exif_data <- tryCatch(
      exifr::read_exif(image),
      error = function(e) NULL
    )

    # Extraer fecha
    date_taken <- "unknown_date"

    if (
      !is.null(exif_data) &&
        date_field %in% names(exif_data) &&
        !is.na(exif_data[[date_field]][1])
    ) {
      date_taken <- exif_data[[date_field]][1]
      date_taken <- gsub(":", "-", date_taken)
      date_taken <- gsub(" ", "_", date_taken)
    }

    # Recuperar subruta sin nombre de archivo
    rel_dir <- dirname(rel_path)
    rel_dir <- if (identical(rel_dir, ".")) "" else rel_dir

    # Sanitizar ruta para que pueda entrar en el nombre del archivo
    rel_dir_clean <- gsub("[/\\\\]+", "_", rel_dir)
    rel_dir_clean <- gsub("[^[:alnum:]_-]", "-", rel_dir_clean)

    # Construcción del nuevo nombre
    if (keep_relative_path && nzchar(rel_dir_clean)) {
      new_name <- paste(
        site_name,
        date_taken,
        camera_name,
        rel_dir_clean,
        image_name,
        sep = "_"
      )
    } else {
      new_name <- paste(
        site_name,
        date_taken,
        camera_name,
        image_name,
        sep = "_"
      )
    }

    # Limpieza final de nombre
    new_name <- gsub("__+", "_", new_name)

    new_path <- file.path(site_output_dir, new_name)

    copied <- file.copy(image, new_path, overwrite = FALSE)

    # Si ya existe un archivo con el mismo nombre, generar sufijo incremental
    if (!copied && file.exists(new_path)) {
      file_base <- tools::file_path_sans_ext(new_name)
      file_ext <- tools::file_ext(new_name)

      counter <- 1
      repeat {
        candidate_name <- paste0(file_base, "_", counter, ".", file_ext)
        candidate_path <- file.path(site_output_dir, candidate_name)

        if (!file.exists(candidate_path)) {
          copied <- file.copy(image, candidate_path, overwrite = FALSE)
          new_name <- candidate_name
          new_path <- candidate_path
          break
        }
        counter <- counter + 1
      }
    }

    results <- tibble::add_row(
      results,
      original_path = image,
      relative_path = rel_path,
      camera_name = camera_name,
      datetime_used = date_taken,
      new_name = new_name,
      new_path = new_path,
      copied = copied
    )
  }

  results
}
