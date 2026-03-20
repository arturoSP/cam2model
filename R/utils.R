#' Build a renaming plan for camera trap images
#'
#' Recursively scans a sampling-site directory, extracts EXIF metadata when
#' available, and builds a renaming plan for flattening and copying images into
#' a single output folder per site.
#'
#' @param input_dir Character. Path to the root directory of a sampling site.
#' @param output_dir Character. Path to the root output directory.
#' @param site_name Character or NULL. Name of the sampling site. If `NULL`,
#'   the basename of `input_dir` is used.
#' @param recursive Logical. Should files be searched recursively? Default `TRUE`.
#' @param extensions Character vector. Allowed image extensions.
#' @param keep_relative_path Logical. If `TRUE`, part of the relative path is
#'   included in the new file name.
#' @param date_fields Character vector. EXIF date fields to try in order of
#'   priority. Default is `c("DateTimeOriginal", "FileModifyDate")`.
#' @param parallel Logical. Should EXIF extraction and plan building run in
#'   parallel? Default `FALSE`.
#' @param workers Integer or NULL. Number of workers for parallel execution.
#'
#' @return A tibble with the renaming plan.
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom progressr with_progress progressor
#' @keywords internal
#' @noRd

build_rename_plan <- function(
  input_dir,
  output_dir,
  site_name = NULL,
  recursive = TRUE,
  extensions = c("jpg", "jpeg", "png", "tif", "tiff"),
  keep_relative_path = TRUE,
  date_fields = c("DateTimeOriginal", "FileModifyDate"),
  parallel = FALSE,
  workers = NULL
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

  process_one_image <- function(
    image,
    input_dir,
    site_output_dir,
    site_name,
    keep_relative_path,
    date_fields
  ) {
    input_dir_norm <- normalizePath(input_dir, winslash = "/", mustWork = TRUE)
    image_norm <- normalizePath(image, winslash = "/", mustWork = TRUE)

    rel_path <- sub(
      paste0(
        "^",
        gsub("([.|()\\^{}+$*?]|\\[|\\]|\\\\)", "\\\\\\1", input_dir_norm),
        "/?"
      ),
      "",
      image_norm
    )

    rel_parts <- strsplit(rel_path, "/", fixed = TRUE)[[1]]

    # inferencia simple: la primera subcarpeta es la cámara
    camera_name <- if (length(rel_parts) > 1) rel_parts[1] else "unknown_camera"

    # nombre original
    image_name <- basename(image)

    # leer EXIF con seguridad
    exif_data <- tryCatch(
      exifr::read_exif(image),
      error = function(e) NULL
    )

    # extraer fecha
    date_taken <- "unknown_date"
    date_field_used <- NA_character_

    if (!is.null(exif_data)) {
      for (fld in date_fields) {
        if (fld %in% names(exif_data) && !is.na(exif_data[[fld]][1])) {
          date_taken <- exif_data[[fld]][1]
          date_field_used <- fld
          break
        }
      }
    }

    if (!identical(date_taken, "unknown_date")) {
      date_taken <- gsub(":", "-", date_taken)
      date_taken <- gsub(" ", "_", date_taken)
    }

    # recuperar subruta sin nombre de archivo
    rel_dir <- dirname(rel_path)
    rel_dir <- if (identical(rel_dir, ".")) "" else rel_dir

    rel_dir_clean <- gsub("[/\\\\]+", "_", rel_dir)
    rel_dir_clean <- gsub("[^[:alnum:]_-]", "-", rel_dir_clean)

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

    # limpieza final del nombre
    new_name <- gsub("__+", "_", new_name)
    camera_output_dir <- file.path(site_output_dir, camera_name)
    new_path <- file.path(camera_output_dir, new_name)

    tibble::tibble(
      original_path = image,
      relative_path = rel_path,
      camera_name = camera_name,
      date_field_used = date_field_used,
      datetime_used = date_taken,
      new_name = new_name,
      new_path = new_path
    )
  }

  if (!parallel) {
    progressr::with_progress({
      p <- progressr::progressor(steps = length(image_files))

      plan_list <- lapply(
        image_files,
        function(img) {
          res <- process_one_image(
            img,
            input_dir = input_dir,
            site_output_dir = site_output_dir,
            site_name = site_name,
            keep_relative_path = keep_relative_path,
            date_fields = date_fields
          )
          p()
          res
        }
      )
    })
  } else {
    if (!requireNamespace("future", quietly = TRUE)) {
      stop(
        "Package 'future' is required when `parallel = TRUE`.",
        call. = FALSE
      )
    }
    if (!requireNamespace("future.apply", quietly = TRUE)) {
      stop(
        "Package 'future.apply' is required when `parallel = TRUE`.",
        call. = FALSE
      )
    }

    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)

    if (is.null(workers)) {
      workers <- max(1, future::availableCores() - 1)
    }

    future::plan(future::multisession, workers = workers)

    progressr::with_progress({
      p <- progressr::progressor(steps = length(image_files))

      plan_list <- future.apply::future_lapply(
        image_files,
        function(img) {
          res <- process_one_image(
            img,
            input_dir = input_dir,
            site_output_dir = site_output_dir,
            site_name = site_name,
            keep_relative_path = keep_relative_path,
            date_fields = date_fields
          )
          p()
          res
        },
        future.seed = TRUE
      )
    })
  }

  plan_tbl <- dplyr::bind_rows(plan_list)

  # asegurar nombres únicos antes de copiar
  if (anyDuplicated(plan_tbl$new_name) > 0) {
    idx <- ave(seq_len(nrow(plan_tbl)), plan_tbl$new_name, FUN = seq_along)
    dup <- duplicated(plan_tbl$new_name) |
      duplicated(plan_tbl$new_name, fromLast = TRUE)

    file_base <- tools::file_path_sans_ext(plan_tbl$new_name)
    file_ext <- tools::file_ext(plan_tbl$new_name)

    plan_tbl$new_name[dup] <- paste0(
      file_base[dup],
      "_",
      idx[dup],
      ".",
      file_ext[dup]
    )
    plan_tbl$new_path[dup] <- file.path(
      dirname(plan_tbl$new_path[dup]),
      plan_tbl$new_name[dup]
    )
  }

  plan_tbl
}


#' Execute a renaming plan for camera trap images
#'
#' Copies files according to a renaming plan generated by `build_rename_plan()`.
#'
#' @param plan_tbl A tibble containing at least `original_path`, `new_name`,
#'   and `new_path`.
#' @param overwrite Logical. Should existing files be overwritten? Default `FALSE`.
#'
#' @return The input tibble with an added logical column `copied`.
#' @importFrom progressr with_progress progressor
#' @keywords internal
#' @noRd

execute_rename_plan <- function(plan_tbl, overwrite = FALSE) {
  required_cols <- c("original_path", "new_name", "new_path")
  if (!all(required_cols %in% names(plan_tbl))) {
    stop(
      "`plan_tbl` must contain columns: ",
      paste(required_cols, collapse = ", "),
      call. = FALSE
    )
  }

  copied <- logical(nrow(plan_tbl))

  progressr::with_progress({
    p <- progressr::progressor(steps = nrow(plan_tbl))

    for (i in seq_len(nrow(plan_tbl))) {
      dest_dir_i <- dirname(plan_tbl$new_path[i])

      if (!dir.exists(dest_dir_i)) {
        dir.create(dest_dir_i, recursive = TRUE)
      }

      copied[i] <- file.copy(
        from = plan_tbl$original_path[i],
        to = plan_tbl$new_path[i],
        overwrite = overwrite
      )

      p()
    }
  })

  plan_tbl$copied <- copied
  plan_tbl
}

#' Build unique destination file names for a batch
#'
#' Internal helper used by copy_in_batches() to create
#' destination names, optionally prefixing them with an identifier.
#'
#' @param ids Character vector of identifiers.
#' @param paths Character vector of source file paths.
#' @param rename_with_id Logical; if `TRUE`, prefixes each output file
#'   name with the corresponding `id`.
#'
#' @return A character vector of unique file names.
#'
#' @keywords internal
#' @noRd
.build_destination_names_fs <- function(ids, paths, rename_with_id = TRUE) {
  original_names <- fs::path_file(paths)

  if (rename_with_id) {
    has_ext <- grepl("\\.", original_names)

    base_name <- ifelse(
      has_ext,
      sub("\\.[^.]+$", "", original_names),
      original_names
    )

    ext <- ifelse(
      has_ext,
      sub("^.*(\\.[^.]+)$", "\\1", original_names),
      ""
    )

    ids_clean <- gsub("[^[:alnum:]_-]", "_", as.character(ids))

    new_names <- paste0(ids_clean, "_", base_name, ext)
  } else {
    new_names <- original_names
  }

  make.unique(new_names, sep = "_dup_")
}
