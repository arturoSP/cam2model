#' Rename and flatten camera trap images
#'
#' This wrapper function builds a renaming plan and immediately executes it,
#' allowing the user to flatten and rename all images from a sampling-site
#' directory in a single call.
#'
#' @param input_dir Character. Path to the root directory of a sampling site.
#' @param output_dir Character. Path to the root output directory.
#' @param site_name Character or NULL. Name of the sampling site. If `NULL`,
#'   the basename of `input_dir` is used.
#' @param recursive Logical. Should files be searched recursively? Default `TRUE`.
#' @param extensions Character vector. Allowed image extensions.
#' @param keep_relative_path Logical. If `TRUE`, part of the relative path is
#'   included in the new file name.
#' @param date_fields Character vector. EXIF date fields to try in order.
#' @param parallel Logical. Should the renaming plan be built in parallel?
#'   Default `FALSE`.
#' @param workers Integer or NULL. Number of workers when `parallel = TRUE`.
#' @param overwrite Logical. Should existing files be overwritten? Default `FALSE`.
#' @param return_plan Logical. If `TRUE`, returns a list with both `plan` and
#'   `result`. If `FALSE`, returns only the executed plan with copy status.
#'
#' @details
#' **Expected minimum input columns (internal contract):** none. The function
#' reads image files directly from `input_dir` and builds metadata internally.
#'
#' **Produced columns:**
#' - If `return_plan = FALSE`, returns the execution table with columns from the
#'   renaming plan plus `copied`.
#' - If `return_plan = TRUE`, returns a list with:
#'   - `plan`: planning table that includes `original_path`, `new_name`, and
#'     `new_path`.
#'   - `result`: the same rows plus `copied`.
#'
#' **I/O side-effects:** creates `<output_dir>/<site_name>` when needed and
#' copies files to that folder using standardized names.
#'
#' @return A tibble with copy results, or a list containing both the plan and
#'   the execution result.
#' @seealso [process_metadata()], [generate_plot()], [process_training_images()],
#'   [copy_in_batches()]
#' @examples
#' \dontrun{
#' # Requires real image files on disk.
#' raw_dir <- tempfile("cam2model_raw_")
#' out_dir <- tempfile("cam2model_out_")
#' dir.create(raw_dir, recursive = TRUE)
#' dir.create(out_dir, recursive = TRUE)
#'
#' # Copy real camera-trap images into `raw_dir` before running.
#' result <- rename_images(
#'   input_dir = raw_dir,
#'   output_dir = out_dir,
#'   site_name = "site_demo",
#'   overwrite = FALSE
#' )
#' head(result)
#' }
#' @export

rename_images <- function(
  input_dir,
  output_dir,
  site_name = NULL,
  recursive = TRUE,
  extensions = c("jpg", "jpeg", "png", "tif", "tiff"),
  keep_relative_path = TRUE,
  date_fields = c("DateTimeOriginal", "FileModifyDate"),
  parallel = FALSE,
  workers = NULL,
  overwrite = FALSE,
  return_plan = FALSE
) {
  plan_tbl <- build_rename_plan(
    input_dir = input_dir,
    output_dir = output_dir,
    site_name = site_name,
    recursive = recursive,
    extensions = extensions,
    keep_relative_path = keep_relative_path,
    date_fields = date_fields,
    parallel = parallel,
    workers = workers
  )

  result_tbl <- execute_rename_plan(
    plan_tbl = plan_tbl,
    overwrite = overwrite
  )

  if (return_plan) {
    return(list(
      plan = plan_tbl,
      result = result_tbl
    ))
  }

  result_tbl
}
