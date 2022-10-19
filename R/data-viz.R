#' plot_tag_data
#'
#' @param .tag_dat A data frame containing one date (dttm, daily data) and tag (numeric) columns.
#' @param .ncol The number of columns per page.
#' @param .nrow The number of rows per page.
#' @param .loess Logical - Whether or not to include a LOESS smoother.
#' @param .plotly Returns either a static (ggplot2) visualization or an interactive (plotly) visualization.
#'
#' @return A static \code{ggplot2} plot or an interactive \code{plotly} plot.
#' @export
#'
plot_tag_data <- function(
    .tag_dat,
    .ncol   = 2,
    .nrow   = 1,
    .loess  = TRUE,
    .plotly = FALSE
) {

  g <- .tag_dat |>
    tidyr::pivot_longer(-date) |>
    timetk::plot_time_series(
      .date_var    = date,
      .value       = value,
      .color_var   = name,
      .smooth      = .loess,
      .interactive = .plotly
    ) +
    trelliscopejs::facet_trelliscope(~ name, ncol = .ncol, nrow = .nrow,
                                     scales = "free")

  return(g)

}

