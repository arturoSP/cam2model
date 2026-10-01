#' Process and Select Images for Training
#'
#' Filters metadata according to one or more rules, samples a per-directory
#' proportion of images, optionally copies sampled images to a destination
#' directory, and generates CSV files for bookkeeping and annotation.
#'
#' @param metadata_file Character. Path to the CSV file containing metadata.
#' @param dest_dir Character. Path to the directory where outputs are written.
#' @param output_sample_file Character. Name of the CSV file storing the sampled
#'   image list.
#' @param output_annotation_file Character. Name of the CSV file storing the
#'   annotation template.
#' @param sample_proportion Numeric in `(0, 1]`. Proportion of images to sample
#'   within each directory.
#' @param filters A list of filtering rules. Each rule must be a list with:
#'   \itemize{
#'     \item \code{var}: character, name of the metadata column to filter by.
#'     \item \code{min}: lower threshold (or NULL).
#'     \item \code{max}: upper threshold (or NULL).
#'   }
#'   If `NULL`, no filtering is applied.
#' @param min_per_dir Integer. Minimum number of images to sample per directory.
#' @param max_per_dir Integer. Maximum number of images to sample per directory.
#' @param copy_images Logical. If `TRUE`, sampled images are copied to
#'   `dest_dir`.
#' @param parallel Logical. If `TRUE`, file copying is done in parallel.
#' @param workers Integer or NULL. Number of workers for parallel copying.
#' @param show_progress Logical. If `TRUE`, show a progress bar using
#'   `progressr`.
#'
#' @details
#' **Expected minimum input columns (contract):** `Directory` and `FileName`
#' must be present in `metadata_file`.
#'
#' **Produced columns:** returns sampled rows with at least `Directory`,
#' `FileName`, optional `File_date`/`File_hour` (if present in input), and
#' `file_path`; adds `dest_file` and `copied` when `copy_images = TRUE`.
#'
#' **I/O side-effects:**
#' - Creates `dest_dir` if it does not exist.
#' - Writes sampled CSV to `file.path(dest_dir, output_sample_file)`.
#' - Writes annotation template CSV to
#'   `file.path(dest_dir, output_annotation_file)`.
#' - Optionally copies sampled files into `dest_dir` when
#'   `copy_images = TRUE`.
#'
#' @return A tibble with sampled images and file paths used for training.
#' @seealso [rename_images()], [process_metadata()], [generate_plot()],
#'   [copy_in_batches()]
#' @importFrom dplyr group_by summarise select mutate n group_modify slice_sample ungroup any_of
#' @importFrom readr read_csv write_csv
#' @importFrom fs dir_create
#' @importFrom progressr with_progress progressor
#' @export
#' @examples
#' src_dir <- tempfile("cam2model_train_src_")
#' dest_dir <- tempfile("cam2model_train_out_")
#' dir.create(src_dir, recursive = TRUE)
#'
#' # Create minimal files that can be referenced from metadata
#' f1 <- file.path(src_dir, "img1.jpg")
#' f2 <- file.path(src_dir, "img2.jpg")
#' writeBin(charToRaw("a"), f1)
#' writeBin(charToRaw("b"), f2)
#'
#' metadata_tbl <- data.frame(
#'   Directory = c(src_dir, src_dir),
#'   FileName = c("img1.jpg", "img2.jpg"),
#'   File_hour = c(10, 14),
#'   stringsAsFactors = FALSE
#' )
#' metadata_csv <- tempfile("cam2model_metadata_", fileext = ".csv")
#' readr::write_csv(metadata_tbl, metadata_csv)
#'
#' sampled <- process_training_images(
#'   metadata_file = metadata_csv,
#'   dest_dir = dest_dir,
#'   output_sample_file = "sampled_images.csv",
#'   output_annotation_file = "annotation_data.csv",
#'   sample_proportion = 1,
#'   copy_images = FALSE,
#'   show_progress = FALSE,
#'   parallel = FALSE
#' )
#'
#' sampled
#' file.exists(file.path(dest_dir, "sampled_images.csv"))
#' file.exists(file.path(dest_dir, "annotation_data.csv"))

process_training_images <- function(
  metadata_file,
  dest_dir,
  output_sample_file,
  output_annotation_file,
  sample_proportion = 0.5,
  filters = NULL,
  min_per_dir = 1,
  max_per_dir = Inf,
  copy_images = FALSE,
  parallel = FALSE,
  workers = NULL,
  show_progress = TRUE
) {
  # 1. Load metadata ----------------------------------------------------------
  metadata <- readr::read_csv(metadata_file)

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

  # 2. Apply filtering rules --------------------------------------------------
  filtered <- metadata

  if (!is.null(filters)) {
    if (!is.list(filters)) {
      stop(
        "`filters` must be a list of rules, each rule being a list with components 'var', 'min', and 'max'.",
        call. = FALSE
      )
    }

    check_and_apply_rule <- function(df, rule) {
      if (!is.list(rule) || is.null(rule$var)) {
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
          "In each rule, 'var' must be a single character string.",
          call. = FALSE
        )
      }

      if (!var_name %in% names(df)) {
        stop(
          "Column '",
          var_name,
          "' specified in filters was not found in metadata.",
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
            " must be POSIXt because '",
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

      check_threshold_type(min_threshold, "min")
      check_threshold_type(max_threshold, "max")

      if (is.null(min_threshold)) {
        min_threshold <- min(values, na.rm = TRUE)
      }
      if (is.null(max_threshold)) {
        max_threshold <- max(values, na.rm = TRUE)
      }

      if (any(max_threshold < min_threshold, na.rm = TRUE)) {
        stop(
          "In filter for '",
          var_name,
          "', `max` must be greater than or equal to `min`.",
          call. = FALSE
        )
      }

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

    for (rule in filters) {
      filtered <- check_and_apply_rule(filtered, rule)
    }
  }

  # 3. Keep relevant columns --------------------------------------------------
  training_images <- filtered |>
    dplyr::select(
      Directory,
      FileName,
      dplyr::any_of(c("File_date", "File_hour"))
    )

  if (nrow(training_images) == 0) {
    stop("No images available after filtering.", call. = FALSE)
  }

  # 4. Sample proportionally within each directory ----------------------------
  sampled_images <- training_images |>
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
    dplyr::mutate(
      file_path = file.path(Directory, FileName)
    )

  # 5. Create destination directory -------------------------------------------
  if (!dir.exists(dest_dir)) {
    fs::dir_create(dest_dir)
  }

  # 6. Save sampled metadata with original paths ------------------------------
  if (!grepl("\\.csv$", output_sample_file, ignore.case = TRUE)) {
    output_sample_file <- paste0(output_sample_file, ".csv")
  }

  readr::write_csv(
    sampled_images,
    file = file.path(dest_dir, output_sample_file)
  )

  # 7. Optional image copying -------------------------------------------------
  if (copy_images) {
    sampled_images <- sampled_images |>
      dplyr::mutate(
        dest_file = file.path(dest_dir, FileName)
      )

    copy_one_file <- function(source_file, dest_file) {
      file.copy(source_file, dest_file, overwrite = TRUE)
    }

    if (!parallel) {
      if (show_progress) {
        progressr::with_progress({
          p <- progressr::progressor(steps = nrow(sampled_images))

          copied <- vapply(
            seq_len(nrow(sampled_images)),
            function(i) {
              res <- copy_one_file(
                source_file = sampled_images$file_path[i],
                dest_file = sampled_images$dest_file[i]
              )
              p(message = basename(sampled_images$FileName[i]))
              res
            },
            logical(1)
          )
        })
      } else {
        copied <- vapply(
          seq_len(nrow(sampled_images)),
          function(i) {
            copy_one_file(
              source_file = sampled_images$file_path[i],
              dest_file = sampled_images$dest_file[i]
            )
          },
          logical(1)
        )
      }
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

      if (show_progress) {
        progressr::with_progress({
          p <- progressr::progressor(steps = nrow(sampled_images))

          copied <- unlist(
            future.apply::future_lapply(
              seq_len(nrow(sampled_images)),
              function(i) {
                res <- file.copy(
                  from = sampled_images$file_path[i],
                  to = sampled_images$dest_file[i],
                  overwrite = TRUE
                )
                p(message = basename(sampled_images$FileName[i]))
                res
              },
              future.seed = TRUE
            )
          )
        })
      } else {
        copied <- unlist(
          future.apply::future_lapply(
            seq_len(nrow(sampled_images)),
            function(i) {
              file.copy(
                from = sampled_images$file_path[i],
                to = sampled_images$dest_file[i],
                overwrite = TRUE
              )
            },
            future.seed = TRUE
          )
        )
      }
    }

    sampled_images$copied <- copied
  }

  # 8. Create annotation file -------------------------------------------------
  annotation_data <- sampled_images |>
    dplyr::select(FileName, file_path) |>
    dplyr::mutate(
      Annotation = NA,
      fish = NA,
      turtle = NA
    )

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
