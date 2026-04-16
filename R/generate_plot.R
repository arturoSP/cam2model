#' Generate Custom Plots from Metadata
#'
#' Generates `ggplot2` visualizations for processed camera-trap metadata,
#' with flexible x/y mappings, optional color/faceting, and optional filtering.
#'
#' @param processed_metadata A data frame containing processed metadata.
#' @param x Bare column name to use for the x-axis.
#' @param y Bare column name to use for the y-axis.
#' @param color Bare column name (optional) to map to point/line color.
#' @param facets Bare column name (optional) used in `facet_wrap()`.
#' @param filter_expr Expression (optional) used to filter the metadata before
#'   plotting.
#' @param angle Numeric. Angle for x-axis labels. Default is `0`.
#'
#' @details
#' **Expected minimum input columns (contract):** the columns referenced in
#' `x` and `y`, and optionally those used in `color`, `facets`, and
#' `filter_expr`.
#'
#' **Produced columns/artifacts:** none (no data is modified on disk). The
#' function returns a `ggplot` object combining `geom_line()` and `geom_point()`.
#'
#' **I/O side-effects:** none.
#'
#' @return A `ggplot` object.
#' @seealso [rename_images()], [process_metadata()], [process_training_images()],
#'   [copy_in_batches()]
#' @importFrom ggplot2 ggplot aes geom_line geom_point facet_wrap scale_x_datetime theme element_text vars
#' @importFrom dplyr filter
#' @importFrom rlang enquo quo_is_null
#' @export
#' @examples
#' demo_metadata <- data.frame(
#'   Image_dttm = as.POSIXct("2024-08-01 00:00:00", tz = "UTC") + 0:5 * 3600,
#'   bLuma = c(100, 120, 140, 130, 150, 160),
#'   Camera = rep(c("C1", "C2"), each = 3),
#'   stringsAsFactors = FALSE
#' )
#'
#' p <- generate_plot(
#'   processed_metadata = demo_metadata,
#'   x = Image_dttm,
#'   y = bLuma,
#'   color = Camera,
#'   facets = Camera
#' )
#' p

generate_plot <- function(processed_metadata, x, y, color = NULL, facets = NULL,
                          filter_expr = NULL, angle = 0) {
  # Validate inputs
  if (missing(processed_metadata) || !is.data.frame(processed_metadata)) {
    stop("The 'processed_metadata' argument must be a data frame.")
  }
  if (missing(x) || missing(y)) {
    stop("Both 'x' and 'y' arguments must be specified.")
  }

  # Convert column names and filter expression to quosures
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  color <- rlang::enquo(color)
  facets <- rlang::enquo(facets)
  filter_expr <- rlang::enquo(filter_expr)

  # Apply filtering if filter expression is provided
  if (!rlang::quo_is_null(filter_expr)) {
    processed_metadata <- dplyr::filter(processed_metadata, !!filter_expr)
  }

  # Start building the plot
  p <- ggplot(processed_metadata, aes(x = !!x, y = !!y))

  # Add color aesthetic if specified
  if (!rlang::quo_is_null(color)) {
    p <- p + aes(colour = !!color)
  }

  # Add default geom_line
  p <- p + geom_line()

  # Add geom_point for emphasis
  p <- p + geom_point()

  # Add facets if specified
  if (!rlang::quo_is_null(facets)) {
    p <- p + facet_wrap(vars(!!facets))
  }

  # Customize x-axis text angle
  p <- p + theme(axis.text.x = element_text(angle = angle, hjust = 1))

  return(p)
}
