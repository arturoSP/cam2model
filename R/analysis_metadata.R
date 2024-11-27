#' Generate Custom Plots from Metadata
#'
#' This function generates ggplot visualizations for the provided metadata,
#' allowing dynamic specification of axes, color, and additional filtering.
#'
#' @param metadata A data frame containing the metadata.
#' @param x Character. The column name to use for the x-axis.
#' @param y Character. The column name to use for the y-axis.
#' @param color Character. (Optional) The column name to use for coloring the points/lines.
#' @param facets Character. (Optional) The column name for facet wrapping.
#' @param filter_expr Expression. (Optional) An expression to filter the metadata.
#' @param breaks Character or NULL. (Optional) Breaks for the x-axis (e.g., "1 day", "1 hour").
#' @param date_labels Character. (Optional) Date format for x-axis labels.
#' @param angle Numeric. (Optional) Angle for x-axis text. Default is 0.
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot aes geom_line geom_point facet_wrap scale_x_datetime theme element_text
#' @importFrom dplyr filter
#' @importFrom rlang enquo eval_tidy
#' @export
#' @examples
#' # Línea de tiempo temperatura por cámara
#' generate_plot(metadata = metadata, x = Image_dttm, y = temp, color = Camera, facets = Camera, filter_expr = File_date <= "2024-08-27")
#' # Línea de tiempo de luminancia con color
#' generate_plot(metadata = metadata, x = Image_dttm, y = bLuma, color = Camera, facets = Camera, filter_expr = File_date <= "2024-08-27")
#' # Puntos de luminancia filtrados por valor mínimo
#' generate_plot(metadata = metadata, x = Image_dttm, y = bLuma, color = Camera, facets = Camera, filter_expr = File_date <= "2024-08-27" & bLuma >= 250)
#'



generate_plot <- function(metadata, x, y, color = NULL, facets = NULL,
                          filter_expr = NULL, breaks = NULL, date_labels = NULL, angle = 0) {
  # Validate inputs
  if (missing(metadata) || !is.data.frame(metadata)) {
    stop("The 'metadata' argument must be a data frame.")
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
    metadata <- dplyr::filter(metadata, !!filter_expr)
  }

  # Start building the plot
  p <- ggplot(metadata, aes(x = !!x, y = !!y))

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

  # Customize x-axis breaks and labels if provided
  if (!is.null(breaks) || !is.null(date_labels)) {
    p <- p + scale_x_datetime(
      breaks = breaks,
      date_labels = date_labels
    )
  }

  # Customize x-axis text angle
  p <- p + theme(axis.text.x = element_text(angle = angle, hjust = 1))

  return(p)
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
# # Importar metadatos
# metadata <- read_csv("12_26_ago_2024_images_metadata.csv")
#
# metadata|>
#   filter(File_date <= "2024-08-27")|>
#   ggplot(aes(x = Image_dttm, y = temp))+
#   geom_line(aes(colour = Camera))+
#   facet_wrap(~Camera)
#
# metadata|>
#   filter(File_date <= "2024-08-27")|>
#   ggplot(aes(x = Image_dttm, y = bLuma, colour))+
#   geom_line(aes(colour = Camera))+
#   facet_wrap(~Camera)
#
# metadata|>
#   filter(File_date <= "2024-08-27")|>
#   ggplot(aes(x = File_hour, y = bLuma))+
#   geom_point(aes(colour = Camera))+
#   facet_wrap(~Camera)
#
# metadata|>
#   filter(File_date <= "2024-08-27")|>
#   filter(bLuma >= 250)|>
#   ggplot(aes(x = Image_dttm, y = bLuma))+
#   geom_point(aes(colour = Camera))+
#   facet_wrap(~Camera)
#
# datebreaks <- seq(min(metadata$File_date), max(metadata$File_date), by = "1 day")
#
# metadata|>
#   filter(File_date <= "2024-08-26")|>
#   #filter(File_hour >= 6 & File_hour <= 18)|>
#   ggplot(aes(x = Image_dttm, y = temp))+
#   geom_line(aes(colour = Camera))+
#   geom_point()+
#   facet_wrap(~Camera)+
#   scale_x_datetime(breaks = "1 day", date_labels = "%d %b")+
#   theme(axis.text.x = element_text(angle = 30, hjust = 1))
#
# #scale_x_datetime(date_breaks = "1 hour")
#
#
