#' Process and Select Images for Training (multiple filters via rules list)
#'
#' This function filters metadata according to one or more rules (each rule
#' applies min/max thresholds to a given column), samples a per-directory
#' proportion of images, copies them to a destination directory, and generates
#' CSV files for bookkeeping and annotation.
#'
#' @param metadata_file Character. Path to the CSV file containing metadata.
#'   The file must include at least the columns `Directory` and `FileName`.
#' @param dest_dir Character. Path to the directory where the sampled images
#'   will be copied. The directory will be created if it does not exist.
#' @param output_sample_file Character. Base name or path (with or without
#'   `.csv`) where the sampled images list will be saved (inside `dest_dir`).
#' @param output_annotation_file Character. Base name or path (with or without
#'   `.csv`) where the annotation template will be saved (inside `dest_dir`).
#' @param sample_proportion Numeric in (0, 1]. Proportion of images to sample
#'   within each directory (default 0.5).
#' @param filters A list of filtering rules. Each element must be a list with
#'   components:
#'   \itemize{
#'     \item \code{var}: character, name of the column to filter by.
#'     \item \code{min}: lower threshold (same class as the column, or NULL).
#'     \item \code{max}: upper threshold (same class as the column, or NULL).
#'   }
#'   If \code{filters = NULL}, no filtering is applied.
#' @param min_per_dir Integer. Minimum number of images to sample per directory
#'   (default 1).
#' @param max_per_dir Integer. Maximum number of images to sample per directory
#'   (default Inf).
#'
#' @return A tibble with the sampled images' metadata (at least `Directory` and
#'   `FileName`).
#'
#' @importFrom dplyr group_by summarise select mutate n group_modify slice_sample ungroup any_of
#' @importFrom readr read_csv write_csv
#' @importFrom fs dir_create
#' @export
#'
#' @examples
#' # Example of using two filters: brightness (bLuma) and hour (File_hour)
#' # filters <- list(
#' #   list(var = "bLuma",     min = 200, max = 255),
#' #   list(var = "File_hour", min = 6,   max = 18)
#' # )
#' # sampled_images <- process_training_images(
#' #   metadata_file = "12_26_ago_2024_images_metadata.csv",
#' #   dest_dir = "C:/train.images_ago_2024",
#' #   output_sample_file = "ago_2024_images_sampled",
#' #   output_annotation_file = "images_ago_2024",
#' #   sample_proportion = 0.5,
#' #   filters = filters
#' # )
#'

process_training_images <- function(
  metadata_file,
  dest_dir,
  output_sample_file,
  output_annotation_file,
  sample_proportion = 0.5,
  filters = NULL,
  min_per_dir = 1,
  max_per_dir = Inf
) {
  # 1. Load metadata ----------------------------------------------------------
  metadata <- readr::read_csv(metadata_file)

  # Checks básicos
  if (!all(c("Directory", "FileName") %in% names(metadata))) {
    stop(
      "The metadata file must contain at least 'Directory' and 'FileName' columns.",
      call. = FALSE
    )
  }

  if (
    !(is.numeric(sample_proportion) &&
      length(sample_proportion) == 1 &&
      is.finite(sample_proportion) &&
      sample_proportion > 0 &&
      sample_proportion <= 1)
  ) {
    stop(
      "`sample_proportion` must be a numeric value in (0, 1].",
      call. = FALSE
    )
  }

  # 2. Aplicar reglas de filtrado (si las hay) -------------------------------
  filtered <- metadata

  if (!is.null(filters)) {
    if (!is.list(filters)) {
      stop(
        "`filters` must be a list of rules, each rule being a list with components 'var', 'min', 'max'.",
        call. = FALSE
      )
    }

    # helper interno para validar e imputar thresholds por tipo
    check_and_apply_rule <- function(df, rule) {
      if (
        !is.list(rule) ||
          is.null(rule$var)
      ) {
        stop(
          "Each filter rule must be a list with at least component 'var'.",
          call. = FALSE
        )
      }

      var_name <- rule$var
      min_threshold <- if (!is.null(rule$min)) rule$min else NULL
      max_threshold <- if (!is.null(rule$max)) rule$max else NULL

      if (!is.character(var_name) || length(var_name) != 1) {
        stop(
          "In each rule, 'var' must be a single character string (column name).",
          call. = FALSE
        )
      }

      if (!var_name %in% names(df)) {
        stop(
          "Column '",
          var_name,
          "' specified in filters$var not found in metadata.",
          call. = FALSE
        )
      }

      values <- df[[var_name]]

      if (all(is.na(values))) {
        stop("Column '", var_name, "' only contains NA values.", call. = FALSE)
      }

      col_is_numeric <- is.numeric(values)
      col_is_date <- inherits(values, "Date")
      col_is_posix <- inherits(values, "POSIXt")
      col_is_char <- is.character(values)

      # chequeo de tipo para un threshold dado
      check_threshold_type <- function(th, which_th) {
        if (is.null(th)) {
          return(invisible(TRUE))
        }

        if (col_is_numeric && !is.numeric(th)) {
          stop(
            which_th,
            " must be numeric because '",
            var_name,
            "' is numeric.",
            call. = FALSE
          )
        }
        if (col_is_date && !inherits(th, "Date")) {
          stop(
            which_th,
            " must be of class 'Date' because '",
            var_name,
            "' is a Date column.",
            call. = FALSE
          )
        }
        if (col_is_posix && !inherits(th, "POSIXt")) {
          stop(
            which_th,
            " must be POSIXt (POSIXct/POSIXlt) because '",
            var_name,
            "' is POSIXt.",
            call. = FALSE
          )
        }
        if (col_is_char && !is.character(th)) {
          stop(
            which_th,
            " must be character because '",
            var_name,
            "' is character.",
            call. = FALSE
          )
        }

        invisible(TRUE)
      }

      check_threshold_type(min_threshold, "min_threshold")
      check_threshold_type(max_threshold, "max_threshold")

      # imputar thresholds si son NULL
      if (is.null(min_threshold)) {
        min_threshold <- min(values, na.rm = TRUE)
      }
      if (is.null(max_threshold)) {
        max_threshold <- max(values, na.rm = TRUE)
      }

      # asegurarse de que min <= max
      if (any(max_threshold < min_threshold, na.rm = TRUE)) {
        stop(
          "In filter for '",
          var_name,
          "', `max` must be greater than or equal to `min`.",
          call. = FALSE
        )
      }

      # aplicar filtrado
      df_sub <- df[
        df[[var_name]] >= min_threshold &
          df[[var_name]] <= max_threshold,
        ,
        drop = FALSE
      ]

      if (nrow(df_sub) == 0) {
        stop(
          "No images remain after applying filter on '",
          var_name,
          "' between ",
          min_threshold,
          " and ",
          max_threshold,
          ".",
          call. = FALSE
        )
      }

      df_sub
    }

    # aplicar todas las reglas en cascada (AND)
    for (rule in filters) {
      filtered <- check_and_apply_rule(filtered, rule)
    }
  }

  # 3. Subconjunto de columnas relevantes ------------------------------------
  # (Directory, FileName y, si existen, File_date y File_hour)
  daytime_images <- filtered |>
    dplyr::select(
      Directory,
      FileName,
      dplyr::any_of(c("File_date", "File_hour"))
    )

  if (nrow(daytime_images) == 0) {
    stop("No images available after filtering.", call. = FALSE)
  }

  # 4. Conteo de imágenes por carpeta ----------------------------------------
  num_images <- daytime_images |>
    dplyr::group_by(Directory) |>
    dplyr::summarise(images = dplyr::n(), .groups = "drop")

  # 5. Muestreo proporcional por carpeta -------------------------------------
  sampled_images <- daytime_images |>
    dplyr::group_by(Directory) |>
    dplyr::group_modify(\(df, key) {
      n_dir <- nrow(df)
      k <- floor(n_dir * sample_proportion)
      k <- max(min_per_dir, k)
      k <- min(k, n_dir, max_per_dir)
      df |>
        dplyr::slice_sample(n = k)
    }) |>
    dplyr::ungroup() |>
    dplyr::select(Directory, FileName)

  # 6. Crear directorio destino si no existe ----------------------------------
  if (!dir.exists(dest_dir)) {
    fs::dir_create(dest_dir)
  }

  # 7. Guardar lista muestreada -----------------------------------------------
  if (!grepl("\\.csv$", output_sample_file, ignore.case = TRUE)) {
    output_sample_file <- paste0(output_sample_file, ".csv")
  }

  readr::write_csv(
    sampled_images,
    file = file.path(dest_dir, output_sample_file)
  )

  # 8. Verificar columnas necesarias -----------------------------------------
  if (!all(c("Directory", "FileName") %in% colnames(sampled_images))) {
    stop(
      "The DataFrame 'sampled_images' must contain 'Directory' and 'FileName' columns.",
      call. = FALSE
    )
  }

  # 9. Copiar imágenes al directorio destino ---------------------------------
  for (i in seq_len(nrow(sampled_images))) {
    source_file <- file.path(
      sampled_images$Directory[i],
      sampled_images$FileName[i]
    )
    dest_file <- file.path(dest_dir, sampled_images$FileName[i])
    file.copy(source_file, dest_file, overwrite = TRUE)
  }

  # 10. Crear base de anotación ----------------------------------------------
  annotation_data <- sampled_images |>
    dplyr::select(FileName) |>
    dplyr::mutate(Annotation = NA, fish = NA, turtle = NA)

  if (!grepl("\\.csv$", output_annotation_file, ignore.case = TRUE)) {
    output_annotation_file <- paste0(output_annotation_file, ".csv")
  }

  readr::write_csv(
    annotation_data,
    file = file.path(dest_dir, output_annotation_file),
    na = ""
  )

  sampled_images
}
