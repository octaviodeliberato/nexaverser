#' plot_tag_data
#'
#' @author Octavio Deliberato Neto.
#'
#' @details
#' This uses the `trelliscopejs::facet_trelliscope()` to finalize the plots.
#'
#' @description This is a data viz function to automagically plot the time
#' series.
#'
#' @param .tag_dat A data frame containing one date (dttm) and some numeric
#' columns.
#' @param .ncol The number of columns per page.
#' @param .nrow The number of rows per page.
#' @param .loess Logical, whether or not to include a LOESS smoother.
#' @param .plotly Returns either a static (ggplot2) visualization or an
#' interactive (plotly) visualization.
#'
#' @return Static or interactive panel plots.
#' @export
#'
plot_tag_data <- function(
    .tag_dat,
    .ncol   = 2L,
    .nrow   = 1L,
    .loess  = TRUE,
    .plotly = FALSE
) {

  # Check if .tag_dat inherits from data.frame ot tibble
  if (!inherits(.tag_dat, c("data.frame", "tbl", "tbl_df"))) {

    rlang::abort(
      message = "The supplied data must be a data frame or a tibble.",
      use_cli_format = TRUE
    )

  }

  # Check if .tag_dat has a date column
  if (!"date" %in% names(.tag_dat)) {

    rlang::abort(
      message = "The supplied data must have a date column.",
      use_cli_format = TRUE
    )

  }

  # Check if .ncol is a positive integer
  if (!is.integer(.ncol) || .ncol <= 0) {

    rlang::abort(
      message = "The supplied number of columns must be a positive integer.",
      use_cli_format = TRUE
    )

  }

  # Check if .nrow is a positive integer
  if (!is.integer(.nrow) || .nrow <= 0) {

    rlang::abort(
      message = "The supplied number of rows must be a positive integer.",
      use_cli_format = TRUE
    )

  }

  # Check if .loess is a logical
  if (!is.logical(.loess)) {

    rlang::abort(
      message = "The supplied loess argument must be a logical.",
      use_cli_format = TRUE
    )

  }

  # Check if .plotly is a logical
  if (!is.logical(.plotly)) {

    rlang::abort(
      message = "The supplied plotly argument must be a logical.",
      use_cli_format = TRUE
    )

  }

  g <- tryCatch(
    {
      .tag_dat |>
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
    },
    error = function(e) {
      .tag_dat |>
        tidyr::pivot_longer(-date) |>
        timetk::plot_time_series(
          .date_var    = date,
          .value       = value,
          .color_var   = name,
          .smooth      = FALSE,
          .interactive = FALSE
        ) +
        trelliscopejs::facet_trelliscope(~ name, ncol = .ncol, nrow = .nrow,
                                         scales = "free")
    }
  )

  return(g)

}


generate_trend_analysis_data <- function(
    tag_dat,
    value_col,
    ks = 7,
    kl = 21
) {

  rolling_ks_mean <- timetk::slidify(
    .f = ~ mean(.x, na.rm = TRUE),
    .period  = ks,
    .align   = "right",
    .partial = TRUE
  )

  rolling_kl_mean <- timetk::slidify(
    .f = ~ mean(.x, na.rm = TRUE),
    .period  = kl,
    .align   = "right",
    .partial = TRUE
  )

  tag_dat <- tag_dat |>
    dplyr::mutate(
      mavg_short = rolling_ks_mean({{ value_col }}),
      mavg_long  = rolling_kl_mean({{ value_col }})
    )

  tag_dat <- tag_dat |>
    dplyr::mutate(diff_perc = (mavg_short - mavg_long) / mavg_long * 100) |>
    dplyr::mutate(
      diff_perc = ifelse(
        is.nan(diff_perc) | is.na(diff_perc) | is.infinite(diff_perc),
        0, diff_perc
      )
    )

  return(tag_dat)

}


plot_mavg_data <- function(data) {

  # Check if data inherits from data.frame ot tibble
  if (!inherits(data, c("data.frame", "tbl", "tbl_df"))) {

    rlang::abort(
      message = "The supplied data must be a data frame or a tibble.",
      use_cli_format = TRUE
    )

  }

  names(data) <- c("date", "tag", "mavg_short", "mavg_long", "diff_perc")

  cols <- c("tag" = "gray50", "mavg_short" = "red", "mavg_long" = "blue")

  g <- data |>
    dplyr::select(-diff_perc) |>
    tidyr::gather(key = "legend", value = "value", tag:mavg_long,
                  factor_key = TRUE) |>
    ggplot2::ggplot(ggplot2::aes(date, value, color = legend, group = legend)) +
    ggplot2::geom_line(ggplot2::aes(linetype = legend)) +
    ggplot2::scale_y_continuous(labels = scales::number_format()) +
    ggplot2::labs(y = "Value", x = "") +
    tidyquant::theme_tq() +
    ggplot2::scale_color_manual(values = cols)

  return(plotly::ggplotly(g))

}


#' assess_tag
#'
#' @author Octavio Deliberato Neto.
#'
#' @param .tag_dat A time series with `date` and `value` cols.
#' @param .avg_by A string, like "day" or "2 hours", to average the time series.
#' @param .pad Logical, whether or not to pad the time series.
#' @param .imp Logical, whether or not to impute missing values using linear
#' interpolation.
#' @param .clean Logical, whether or not to identify and replace outliers and
#' missing values
#' @param .per A seasonal period to use during the transformation. If period = 1,
#' linear interpolation is performed. If period > 1, a robust STL decomposition
#' is first performed and a linear interpolation is applied to the seasonally
#' adjusted data.
#' @param .std Logical, whether or not to standardize to mean 0 and standard
#' deviation 1.
#' @param .chg_pts Logical, whether or not to perform change-point analysis.
#' @param .smooth Logical, whether or not to include a trendline smoother.
#' @param .anom Logical, whether or not to carry out anomaly detection.
#' @param .alpha Controls the width of the "normal" range regarding anomaly
#' detection. Lower values are more conservative while higher values are less
#' prone to incorrectly classifying "normal" observations.
#'
#' @return A list.
#' @export
#'
assess_tag <- function(
    .tag_dat,
    .avg_by  = "day",
    .pad     = FALSE,
    .imp     = FALSE,
    .clean   = FALSE,
    .per     = 1L,
    .std     = FALSE,
    .chg_pts = TRUE,
    .smooth  = FALSE,
    .anom    = TRUE,
    .alpha   = 0.1
) {

  # Check if data inherits from data.frame ot tibble
  if (!inherits(.tag_dat, c("data.frame", "tbl", "tbl_df"))) {

    rlang::abort(
      message = "The supplied data must be a data frame or a tibble.",
      use_cli_format = TRUE
    )

  }

  # Check if .tag_dat has exactly two columns
  if (ncol(.tag_dat) != 2) {

    rlang::abort(
      message = "The supplied data must have exactly two columns.",
      use_cli_format = TRUE
    )

  }

  # Check if .avg_by is a string
  if (!is.character(.avg_by)) {

    rlang::abort(
      message = "The supplied average by argument must be a string.",
      use_cli_format = TRUE
    )

  }

  # Check if .per is a positive integer
  if (!is.integer(.per) || .per < 1) {

    rlang::abort(
      message = "The supplied period must be a positive integer.",
      use_cli_format = TRUE
    )

  }

  # Check if .alpha is a number between 0 and 1
  if (!is.numeric(.alpha) || .alpha < 0 || .alpha > 1) {

    rlang::abort(
      message = "The supplied alpha must be a number between 0 and 1.",
      use_cli_format = TRUE
    )

  }

  # Check if .pad, .imp, .clean, .std, .chg_pts, .smooth, .anom are logical
  if (!all(sapply(c(.pad, .imp, .clean, .std, .chg_pts, .smooth, .anom),
                  is.logical))) {

    rlang::abort(
      message = "The supplied arguments must be logical.",
      use_cli_format = TRUE
    )

  }

  names(.tag_dat) <- c("date", "value")

  freq <- .tag_dat$date |> timetk::tk_get_frequency(period = "day")

  if (freq < 1) {

    rlang::abort(
      message = "A daily or a high frequency time series must be supplied.",
      use_cli_format = TRUE
    )

  } else if (freq > 1) { # more than 1 obs per day

    .tag_dat <- .tag_dat |>
      timetk::summarise_by_time(
        .date_var = date,
        .by = .avg_by,
        value = mean(value, na.rm = TRUE)
      )

  }

  .tag_dat$date <- as.Date(.tag_dat$date)

  if (.pad) {

    .tag_dat <- .tag_dat |>
      timetk::pad_by_time(.date_var = date, .by = .avg_by, .pad_value = 0)

  }

  if (.imp) {

    .tag_dat[, 2] <- timetk::ts_impute_vec(dplyr::pull(.tag_dat, 2),
                                           period = .per)

  }

  if (.clean) {

    .tag_dat[, 2] <- timetk::ts_clean_vec(dplyr::pull(.tag_dat, 2),
                                          period = .per)

  }

  if (.std) {

    .tag_dat[, 2] <- timetk::standardize_vec(.tag_dat |> dplyr::pull(2))

  }

  ts_plt <- timetk::plot_time_series(
    .data     = .tag_dat,
    .date_var = date,
    .value    = value,
    .smooth   = .smooth
  )

  ts_plt_week <- timetk::plot_time_series(
    .data      = .tag_dat,
    .date_var  = date,
    .value     = value,
    .color_var = lubridate::wday(date, label = TRUE),
    .smooth    = FALSE
  )

  calendar_heatmap <- healthyR.ts::ts_calendar_heatmap_plot(
    .data        = .tag_dat,
    .date_col    = date,
    .value_col   = value,
    .interactive = TRUE
  )

  if (.anom) {

    trend_dat <- generate_trend_analysis_data(
      tag_dat   = .tag_dat,
      value_col = value
    )

    trend_plt <- plot_mavg_data(trend_dat)

    anom_plt <- timetk::plot_anomaly_diagnostics(
      .data     = trend_dat,
      .date_var = date,
      .value    = value,
      .alpha    = .alpha
    )

    anom_tbl <- timetk::tk_anomaly_diagnostics(
      .data     = trend_dat,
      .date_var = date,
      .value    = value,
      .alpha    = .alpha
    )

  } else {

    anom_tbl  <- NULL
    anom_plt  <- NULL
    trend_dat <- NULL
    trend_plt <- NULL

  }

  proc_behaviour_30 <- qcc::qcc(
    data   = .tag_dat$value |> utils::tail(30),
    type   = "xbar.one",
    labels = .tag_dat$date |> utils::tail(30),
    plot   = FALSE
  )

  run_chart <- qicharts2::qic(.tag_dat$date, .tag_dat$value, chart = 'i')

  change_point_analyzer_safe <- purrr::possibly(
    ChangePointTaylor::change_point_analyzer,
    NULL
  )

  if (.chg_pts) {

    change_points <- change_point_analyzer_safe(
      .tag_dat$value,
      label = .tag_dat$date,
      n_bootstraps = 100
    )

    if (!is.null(change_points)) {

      Direction <- change_points$To - change_points$From

      change_points$Direction <- ifelse(
        Direction > 0,
        "Up",
        "Down"
      )

      change_points_balance <- change_points$Direction |>
        table()

      change_points_last_change <- change_points |>
        utils::tail(1)

      xintercept <- anytime::anydate(change_points$label)

      change_points_plt <-
        ggplot2::ggplot(.tag_dat, ggplot2::aes(x = anytime::anydate(date),
                                               y = value, group = 1)) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::theme_bw() +
        ggplot2::scale_x_date(date_breaks = "1 month",
                              date_labels = "%b '%y") +
        ggplot2::theme(
          axis.text.x  = ggplot2::element_text(angle = 45, vjust = 1,
                                               hjust =1),
          axis.title.x = ggplot2::element_blank()
        ) +
        ggplot2::geom_vline(
          xintercept = xintercept,
          color      = "steelblue",
          linetype   = "dashed",
          linewidth  = 1.3
        )

    } else {

      change_points_plt         <- NULL
      change_points_balance     <- NULL
      change_points_last_change <- NULL

    }

  } else {

    change_points             <- NULL
    change_points_plt         <- NULL
    change_points_balance     <- NULL
    change_points_last_change <- NULL

  }

  assmnt <- list(
    ts_plt                    = ts_plt,
    ts_plt_week               = ts_plt_week,
    calendar_heatmap          = calendar_heatmap,
    proc_behaviour_30         = proc_behaviour_30,
    run_chart                 = plotly::ggplotly(run_chart),
    trend_data                = trend_dat,
    trend_plt                 = trend_plt,
    anom_plt                  = anom_plt,
    anom_tbl                  = anom_tbl,
    change_points             = change_points,
    change_points_balance     = change_points_balance,
    change_points_last_change = change_points_last_change,
    change_points_plt         = change_points_plt
  ) |>
    purrr::compact()

  return(assmnt)

}


#' forecast_tag
#'
#' @param .tag_dat A time series with `date` (days) and `value` cols.
#' @param .ndays The forecasting horizon, in days.
#' @param .interactive Returns either a static (ggplot2) visualization or an
#' interactive (plotly) visualization.
#'
#' @return Either a `ggplot2` or a `plotly` plot.
#' @export
#'
forecast_tag <- function(.tag_dat, .ndays = 15L, .interactive = FALSE) {

  # Check if .tag_dat inherits either from data.frame or tibble
  if (!inherits(.tag_dat, c("data.frame", "tbl", "tbl_df"))) {

    rlang::abort(
      message = "A data.frame or tibble must be supplied.",
      use_cli_format = TRUE
    )

  }

  # Check if .tag_dat has a date column
  if (!"date" %in% names(.tag_dat)) {

    rlang::abort(
      message = "A data.frame or tibble must have a date column.",
      use_cli_format = TRUE
    )

  }

  # Check if .ndays is a positive integer
  if (!is.integer(.ndays) || .ndays <= 0) {

    rlang::abort(
      message = "The number of days must be a positive integer.",
      use_cli_format = TRUE
    )

  }

  # Check if .interactive is a logical
  if (!is.logical(.interactive)) {

    rlang::abort(
      message = "The interactive argument must be a logical.",
      use_cli_format = TRUE
    )

  }

  ds <- .tag_dat |>
    tibble::as_tibble() |>
    tibble::column_to_rownames(var = "date")

  dates <- .tag_dat[[1]]

  freq <- dates |> timetk::tk_get_frequency(period = "day")

  if (freq < 1) {

    rlang::abort(
      message = "A daily time series must be supplied.",
      use_cli_format = TRUE
    )

  }

  names(ds) <- "value"
  ds$value <- timetk::ts_impute_vec(ds$value)

  ex1 <- spooky::spooky(
    ds,
    n_samp    = ceiling(0.07 * length(ds$value)),
    seq_len   = .ndays,
    n_windows = ceiling(0.03 * length(ds$value)),
    dates     = as.Date(rownames(ds)),
    lno = NULL
  )

  fore      <- ex1$best_model$preds$value$mean
  fore_low  <- ex1$best_model$preds$value$`10%`
  fore_high <- ex1$best_model$preds$value$`90%`
  values    <- ds$value |> as.numeric()
  dates     <- as.Date(rownames(ds))

  full_data_tbl <- tibble::tibble(
    date  = dates,
    value = values
  ) |>
    timetk::future_frame(
      .date_var   = date,
      .length_out = .ndays,
      .bind_data  = TRUE
    ) |>
    dplyr::mutate(actual = NA, fore = NA, fore_low = NA, fore_high = NA)

  filt <- full_data_tbl$value |> is.na() |> which()

  full_data_tbl[-filt, "actual"]   <- ds |> dplyr::pull(value)
  full_data_tbl[filt, "fore"]      <- fore
  full_data_tbl[filt, "fore_low"]  <- fore_low
  full_data_tbl[filt, "fore_high"] <- fore_high

  if (.interactive) {

    fore_plt <- full_data_tbl |>
      dplyr::select(-value) |>
      tidyr::pivot_longer(-date) |>
      timetk::plot_time_series(
        .date_var  = date,
        .value     = value,
        .color_var = name,
        .smooth    = FALSE
      )

  } else {

    fore_plt <- ex1[["best_model"]][["plots"]][["value"]]

  }

  return(
    list(
      fore_data = dplyr::select(full_data_tbl, -value),
      fore_plot = fore_plt
    )
  )

}


#' impute_missing_values
#'
#' @param .tag_dat A time series with `date` (days) and `value` cols.
#'
#' @return A data frame with imputed values.
#' @export
#'
impute_missing_values <- function(.tag_dat) {

  # Check if .tag_dat inherits either from data.frame or tibble
  if (!inherits(.tag_dat, c("data.frame", "tbl", "tbl_df"))) {

    rlang::abort(
      message = "A data.frame or tibble must be supplied.",
      use_cli_format = TRUE
    )

  }

  # Check if .tag_dat has a date column
  if (!"date" %in% names(.tag_dat)) {

    rlang::abort(
      message = "A data.frame or tibble must have a date column.",
      use_cli_format = TRUE
    )

  }

  dates <- .tag_dat$date

  .tag_dat <- .tag_dat |> dplyr::select(-date)

  var_names <- names(.tag_dat)

  ts_sig <- timetk::tk_get_timeseries_signature(dates) |>
    dplyr::select(-index)

  .tag_dat <- dplyr::bind_cols(ts_sig, .tag_dat)

  imp <- mice::mice(data = .tag_dat, m = 5, method = "cart", seed = 123)

  var_names |>
    lapply(\(x) imp[["imp"]][[x]] |> apply(1, mean)) -> .

  names(.) <- var_names

  for (i in seq_along(var_names)) {

    .tag_dat[as.integer(names(.[[i]])), var_names[i]] <- .[[i]]

  }

  imputed_data <- dplyr::bind_cols(
    date = dates,
    .tag_dat[, var_names]
  )

  return(imputed_data)

}
