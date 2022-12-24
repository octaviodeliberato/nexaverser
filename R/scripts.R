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
#' @return A static `ggplot2` plot or an interactive `plotly` plot.
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
    .pad     = FALSE,
    .imp     = FALSE,
    .clean   = FALSE,
    .per     = 1,
    .std     = FALSE,
    .chg_pts = TRUE,
    .smooth  = FALSE,
    .anom    = TRUE,
    .alpha   = 0.1
) {

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
        .by = "day",
        value = mean(value, na.rm = TRUE)
      )

  }

  .tag_dat$date <- as.Date(.tag_dat$date)

  if (.pad) {

    .tag_dat <- .tag_dat |>
      timetk::pad_by_time(.date_var = date, .by = 'day', .pad_value = 0)

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
          size       = 1.3
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
forecast_tag <- function(.tag_dat, .ndays = 15, .interactive = FALSE) {

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

  dates <- .tag_dat$date

  .tag_dat <- .tag_dat |> dplyr::select(-date)

  var_names <- names(.tag_dat)

  ts_sig <- timetk::tk_get_timeseries_signature(dates)

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


#' select_features_with_boruta
#'
#' @param .tag_dat A time series with `date` (days) and `value` cols.
#' @param .target The name (string) of the target variable.
#' @param .balance Logical, whether or not to balance the dataset.
#' @param .with_tentative Logical, whether or not to include the 'tentative
#' features' in the selection.
#' @param .return_data Logical, whether or not to return the dataset in the
#' return object.
#' @param .task Either "regression" or "classification".
#'
#' @return A list.
#' @export
#'
select_features_with_boruta <- function(
    .tag_dat,
    .target,
    .balance        = FALSE,
    .with_tentative = TRUE,
    .return_data    = FALSE,
    .task           = "regression"
) {

  # missing some safety checks

  set.seed(1)

  x <- setdiff(names(.tag_dat), c("date", .target))

  df <- .tag_dat[, c(x, .target)]

  if (.balance) {

    if (.task == "regression") {

      df <- UBL::RandOverRegress(
        form   = stats::as.formula(stringr::str_glue("{.target} ~ .")),
        dat    = as.data.frame(df),
        C.perc = "balance"
      )

      f <- stats::as.formula(stringr::str_glue("{.target} ~ ."))

      feature_selection <- Boruta::Boruta(f, data = df)

      selected_attrs <- Boruta::getSelectedAttributes(
        feature_selection,
        withTentative = .with_tentative
      )

    } else { # classification

      df <- UBL::RandOverClassif(
        form   = stats::as.formula(stringr::str_glue("{.target} ~ .")),
        dat    = as.data.frame(df),
        C.perc = "balance"
      )

      f <- stats::as.formula(stringr::str_glue("{.target} ~ ."))

      feature_selection <- Boruta::Boruta(f, data = df)

      selected_attrs <- Boruta::getSelectedAttributes(
        feature_selection,
        withTentative = .with_tentative
      )

    }

  } else { # don't balance the data

    f <- stats::as.formula(stringr::str_glue("{.target} ~ ."))

    feature_selection <- Boruta::Boruta(f, data = df)

    selected_attrs <- Boruta::getSelectedAttributes(
      feature_selection,
      withTentative = .with_tentative
    )

  }

  features <- list(selected_features = selected_attrs)

  if (.return_data) {

    features[["data"]] <- df

  }

  return(features)

}


#' select_features_with_trex
#'
#' @param .tag_dat A time series with `date` (days) and `value` cols.
#' @param .target The name (string) of the target variable.
#' @param .balance Logical, whether or not to balance the dataset.
#' @param .return_data Logical, whether or not to return the dataset in the
#' return object.
#' @param .task Either "regression" or "classification".
#' @param .corr_max Maximum allowed correlation between any two predictors
#' from different clusters. Defaults to 0.5.
#' @param .parallel Logical. If TRUE random experiments are executed in
#' parallel. Defaults to FALSE.
#' @param .max_cores Maximum number of cores to be used for parallel processing
#' (default: min of 20 random experiments and number of physical cores).
#'
#' @return A list.
#' @export
#'
select_features_with_trex <- function(
    .tag_dat,
    .target,
    .balance     = FALSE,
    .return_data = FALSE,
    .task        = "regression",
    .corr_max    = 0.5,
    .parallel    = FALSE,
    .max_cores   = min(20, max(1, parallel::detectCores(logical = FALSE) - 1))
) {

  # missing some safety checks

  set.seed(1)

  # Numerical zero
  eps <- .Machine$double.eps

  x <- setdiff(names(.tag_dat), c("date", .target))

  df <- .tag_dat[, c(x, .target)]

  if (.balance) {

    if (.task == "regression") {

      df <- UBL::RandOverRegress(
        form   = stats::as.formula(stringr::str_glue("{.target} ~ .")),
        dat    = as.data.frame(df),
        C.perc = "balance"
      )

      X <- df |>
        dplyr::select(-dplyr::all_of(.target)) |>
        as.matrix()

      target <- as.matrix(dplyr::pull(df, .target))

      res <- TRexSelector::trex(
        X                  = X,
        y                  = target,
        corr_max           = .corr_max,
        parallel_process   = .parallel,
        parallel_max_cores = .max_cores,
        tFDR               = 0.05,
        verbose            = FALSE
      )

      selected_attrs <- names(df[, which(res$selected_var > eps)])

    } else { # classification

      df <- UBL::RandOverClassif(
        form   = stats::as.formula(stringr::str_glue("{.target} ~ .")),
        dat    = as.data.frame(df),
        C.perc = "balance"
      )

      X <- df |>
        dplyr::select(-dplyr::all_of(.target)) |>
        as.matrix()

      target <- as.matrix(dplyr::pull(df, .target))

      res <- TRexSelector::trex(X = X, y = target, tFDR = 0.05, verbose = FALSE)

      selected_attrs <- names(df[, which(res$selected_var > eps)])

    }

  } else { # don't balance the data

    X <- df |>
      dplyr::select(-dplyr::all_of(.target)) |>
      as.matrix()

    target <- as.matrix(dplyr::pull(df, .target))

    res <- TRexSelector::trex(X = X, y = target, tFDR = 0.05, verbose = FALSE)

    selected_attrs <- names(df[, which(res$selected_var > eps)])

  }

  features <- list(selected_features = selected_attrs)

  if (.return_data) {

    features[["data"]] <- df

  }

  return(features)

}


#' select_features_with_pps
#'
#' @param .tag_dat A time series with `date` (days) and `value` cols.
#' @param .target The name (string) of the target variable.
#' @param .balance Logical, whether or not to balance the dataset.
#' @param .return_data Logical, whether or not to return the dataset in the
#' return object.
#' @param .task Either "regression" or "classification".
#' @param .cutoff Predictors with PPS scores lower than `.cutoff` will be
#' discarded.
#' @param .parallel Logical, whether to perform score calls in parallel.
#' @param .max_cores Maximum number of cores to use, defaults to maximum
#' minus 1.
#'
#' @return A list.
#' @export
#'
select_features_with_pps <- function(
    .tag_dat,
    .target,
    .balance     = FALSE,
    .return_data = FALSE,
    .task        = "regression",
    .cutoff      = 0.15,
    .parallel    = FALSE,
    .max_cores   = -1
) {

  # missing some safety checks

  set.seed(1)

  x <- setdiff(names(.tag_dat), c("date", .target))

  df <- .tag_dat[, c(x, .target)]

  if (.balance) {

    if (.task == "regression") {

      df <- UBL::RandOverRegress(
        form   = stats::as.formula(stringr::str_glue("{.target} ~ .")),
        dat    = as.data.frame(df),
        C.perc = "balance"
      )

      selected_attrs <- ppsr::score_predictors(
        df = df,
        y  = .target
      ) |>
        tibble::as_tibble() |>
        dplyr::arrange(dplyr::desc(pps)) |>
        dplyr::filter(pps >= .cutoff) |>
        dplyr::pull("x") |>
        setdiff(x = _, y = .target)

    } else { # classification

      df <- UBL::RandOverClassif(
        form   = stats::as.formula(stringr::str_glue("{.target} ~ .")),
        dat    = as.data.frame(df),
        C.perc = "balance"
      )

      selected_attrs <- ppsr::score_predictors(
        df          = df,
        y           = .target,
        do_parallel = .parallel,
        n_cores     = .max_cores
      ) |>
        tibble::as_tibble() |>
        dplyr::arrange(dplyr::desc(pps)) |>
        dplyr::filter(pps >= .cutoff) |>
        dplyr::pull("x") |>
        setdiff(x = _, y = .target)

    }

  } else { # don't balance the data

    selected_attrs <- ppsr::score_predictors(
      df          = df,
      y           = .target,
      do_parallel = .parallel,
      n_cores     = .max_cores
    ) |>
      tibble::as_tibble() |>
      dplyr::arrange(dplyr::desc(pps)) |>
      dplyr::filter(pps >= .cutoff) |>
      dplyr::pull("x") |>
      setdiff(x = _, y = .target)

  }

  features <- list(selected_features = selected_attrs)

  if (.return_data) {

    features[["data"]] <- df

  }

  return(features)

}


#' calc_p_value_granger_fit
#'
#' @param tag_x An univariate series of observations.
#' @param tag_y An univariate series of observations.
#' @param max_lag An integer specifying the order of delays to include in the
#' auxiliary regression. Defaults to 7.
#'
#' @return A list.
#' @export
#'
calc_p_value_granger_fit <- function(tag_x, tag_y, max_lag = 7) {

  fit_granger <- vector(mode = "list", length = max_lag)

  for (i in 1:max_lag) { # max lag periods
    fit_granger[[i]] <- lmtest::grangertest(
      tag_x,
      tag_y,
      order = i,
      na.action = stats::na.omit
    ) |>
      purrr::pluck(4, 2)
  }

  names(fit_granger) <- sapply(
    1:max_lag,
    function(i) { stringr::str_glue("lag_of_{i}_days") }
  )

  return(unlist(fit_granger))

}


#' run_causation_analysis
#'
#' @param .tag_dat A data frame with numeric data.
#' @param .target The name of the target (response) column.
#' @param .max_lag An integer specifying the order of delays to include in the
#' auxiliary regression. Defaults to 7.
#' @param .assess The number of samples used for each test.
#'
#' @return A character vector.
#' @export
#'
run_causation_analysis <- function(
    .tag_dat,
    .target,
    .max_lag = 7,
    .assess  = NULL
) {

  x <- setdiff(names(.tag_dat), c("date", .target))

  if (is.null(.assess)) {
    obs <- nrow(.tag_dat)
  } else {
    obs <- .assess |> as.integer()
  }

  calc_p_value_granger_fit_safe <- purrr::possibly(calc_p_value_granger_fit,
                                                   NULL)

  causation_analysis <- lapply(
    .tag_dat[, x] |> utils::tail(obs),
    \(x) calc_p_value_granger_fit_safe(
      tag_x   = x,
      tag_y   = .tag_dat[[.target]] |> utils::tail(obs),
      max_lag = .max_lag
    )
  )

  causation_analysis <- purrr::compact(causation_analysis)

  good <- sapply(causation_analysis,
                 \(x) { any(x < 0.05) })

  probable_causes <- names(causation_analysis)[good]

  return(probable_causes)

}


#' train_cubist_model
#'
#' @param .data A time series with `date` (days) and `value` cols.
#' @param .target The name (string) of the target variable.
#' @param .prop The proportion of data to be retained for modeling/analysis.
#' @param .strat Logical, whether or not to conduct stratified sampling by
#' '.target'
#' @param .tune Default is TRUE, this will create a tuning grid and tuned
#' workflow.
#' @param .grid_size Default is 10.
#' @param .num_cores Default is 1.
#' @param .best_metric Default is "rmse".
#' @param .surrogate_model Logical, whether or not to conduct surrogate
#' modeling, i.e., use all data to train the model.
#'
#' @return A list.
#' @export
#'
train_cubist_model <- function(
    .data,
    .target,
    .prop            = 0.8,
    .strat           = FALSE,
    .tune            = TRUE,
    .grid_size       = 10,
    .num_cores       = 1,
    .best_metric     = "rmse",
    .surrogate_model = FALSE
) {

  # missing safety checks

  if (!requireNamespace("rules", quietly = TRUE)) {
    stop(
      "Package \"rules\" must be installed to use this function.",
      call. = FALSE
    )
  } else {
    requireNamespace("rules", quietly = TRUE)
  }

  f <- stats::as.formula(stringr::str_glue("{.target} ~ ."))
  .data$date <- NULL

  # Splits
  set.seed(1)

  if (!.surrogate_model) {

    if (.strat) {
      splits <- rsample::initial_split(data = .data, prop = .prop,
                                       strata = .target)
    } else {
      splits <- rsample::initial_split(data = .data, prop = .prop)
    }

    train <- rsample::training(splits)
    test  <- rsample::testing(splits)

  } else { # surrogate model

    train <- .data
    test  <- .data

  }

  rec_obj <- healthyR.ai::hai_cubist_data_prepper(train, f)

  auto_cube <- healthyR.ai::hai_auto_cubist(
    .data        = train,
    .rec_obj     = rec_obj,
    .tune        = .tune,
    .grid_size   = .grid_size,
    .num_cores   = .num_cores,
    .best_metric = .best_metric
  )

  best_model <- auto_cube$model_info$fitted_wflw

  # Check performance
  test_pred <- stats::predict(best_model, new_data = test)

  df_test <- tibble::tibble(
    actual = test[[.target]],
    pred   = test_pred$.pred
  )

  mod_rmse     <- yardstick::rmse_vec(df_test$actual, df_test$pred)
  mod_mae      <- yardstick::mae_vec(df_test$actual, df_test$pred)
  mod_rsq_trad <- yardstick::rsq_trad_vec(df_test$actual, df_test$pred)
  mod_acc      <- 100 - yardstick::smape_vec(df_test$actual, df_test$pred)

  tb <- tibble::tibble(
    rmse = mod_rmse |> round(2),
    mae  = mod_mae |> round(2),
    r2   = mod_rsq_trad |> round(2),
    acc  = mod_acc |> round(1)
  )

  inset_tbl <- tibble::tibble(
    x = df_test$actual[2],
    y = df_test$pred |> max(),
    tb = list(tb)
  )

  g <- ggplot2::ggplot(
    data = df_test,
    mapping = ggplot2::aes(x = actual, y = pred)
  ) +
    ggplot2::geom_abline(col = "green", lty = 2, lwd = 1) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::coord_fixed(ratio = 1) +
    ggpp::geom_table(
      data = inset_tbl,
      ggplot2::aes(x = x, y = y, label = tb)
    )

  return(
    list(
      model     = best_model,
      test_plot = g
    )
  )

}


#' train_xgboost_model
#'
#' @param .data A time series with `date` (days) and `value` cols.
#' @param .target The name (string) of the target variable.
#' @param .prop The proportion of data to be retained for modeling/analysis.
#' @param .strat Logical, whether or not to conduct stratified sampling by
#' '.target'
#' @param .tune Default is TRUE, this will create a tuning grid and tuned
#' workflow.
#' @param .grid_size Default is 10.
#' @param .num_cores Default is 1.
#' @param .model_type Default is `regression`, can also be `classification.`
#' @param .surrogate_model Logical, whether or not to conduct surrogate
#' modeling, i.e., use all data to train the model.
#'
#' @return A list.
#' @export
#'
train_xgboost_model <- function(
    .data,
    .target,
    .prop            = 0.8,
    .strat           = FALSE,
    .tune            = TRUE,
    .grid_size       = 10,
    .num_cores       = 1,
    .model_type      = "regression",
    .surrogate_model = FALSE
) {

  # missing safety checks

  f <- stats::as.formula(stringr::str_glue("{.target} ~ ."))
  .data$date <- NULL

  # Splits
  set.seed(1)

  if (!.surrogate_model) {

    if (.strat) {
      splits <- rsample::initial_split(data = .data, prop = .prop,
                                       strata = .target)
    } else {
      splits <- rsample::initial_split(data = .data, prop = .prop)
    }

    train <- rsample::training(splits)
    test  <- rsample::testing(splits)

  } else { # surrogate model

    train <- .data
    test  <- .data

  }

  rec_obj <- healthyR.ai::hai_xgboost_data_prepper(train, f)

  best_metric <- switch (.model_type,
    "regression"     = "rmse",
    "classification" = "accuracy"
  )

  auto_xgboost <- healthyR.ai::hai_auto_xgboost(
    .data        = train,
    .rec_obj     = rec_obj,
    .tune        = .tune,
    .grid_size   = .grid_size,
    .num_cores   = .num_cores,
    .model_type  = .model_type,
    .best_metric = best_metric
  )

  best_model <- auto_xgboost$model_info$fitted_wflw

  # Check performance
  test_pred <- stats::predict(best_model, new_data = test)

  df_test <- tibble::tibble(
    actual = test[[.target]],
    pred   = test_pred$.pred
  )

  mod_rmse     <- yardstick::rmse_vec(df_test$actual, df_test$pred)
  mod_mae      <- yardstick::mae_vec(df_test$actual, df_test$pred)
  mod_rsq_trad <- yardstick::rsq_trad_vec(df_test$actual, df_test$pred)
  mod_acc      <- 100 - yardstick::smape_vec(df_test$actual, df_test$pred)

  tb <- tibble::tibble(
    rmse = mod_rmse |> round(2),
    mae  = mod_mae |> round(2),
    r2   = mod_rsq_trad |> round(2),
    acc  = mod_acc |> round(1)
  )

  inset_tbl <- tibble::tibble(
    x = df_test$actual[2],
    y = df_test$pred |> max(),
    tb = list(tb)
  )

  g <- ggplot2::ggplot(
    data = df_test,
    mapping = ggplot2::aes(x = actual, y = pred)
  ) +
    ggplot2::geom_abline(col = "green", lty = 2, lwd = 1) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::coord_fixed(ratio = 1) +
    ggpp::geom_table(
      data = inset_tbl,
      ggplot2::aes(x = x, y = y, label = tb)
    )

  return(
    list(
      model     = best_model,
      test_plot = g
    )
  )

}


#' train_mars_model
#'
#' @param .data A time series with `date` (days) and `value` cols.
#' @param .target The name (string) of the target variable.
#' @param .prop The proportion of data to be retained for modeling/analysis.
#' @param .strat Logical, whether or not to conduct stratified sampling by
#' '.target'
#' @param .tune Default is TRUE, this will create a tuning grid and tuned
#' workflow.
#' @param .grid_size Default is 10.
#' @param .num_cores Default is 1.
#' @param .model_type Default is `regression`, can also be `classification.`
#' @param .surrogate_model Logical, whether or not to conduct surrogate
#' modeling, i.e., use all data to train the model.
#'
#' @return A list.
#' @export
#'
train_mars_model <- function(
    .data,
    .target,
    .prop            = 0.8,
    .strat           = FALSE,
    .tune            = TRUE,
    .grid_size       = 10,
    .num_cores       = 1,
    .model_type      = "regression",
    .surrogate_model = FALSE
) {

  # missing safety checks

  f <- stats::as.formula(stringr::str_glue("{.target} ~ ."))
  .data$date <- NULL

  # Splits
  set.seed(1)

  if (!.surrogate_model) {

    if (.strat) {
      splits <- rsample::initial_split(data = .data, prop = .prop,
                                       strata = .target)
    } else {
      splits <- rsample::initial_split(data = .data, prop = .prop)
    }

    train <- rsample::training(splits)
    test  <- rsample::testing(splits)

  } else { # surrogate model

    train <- .data
    test  <- .data

  }

  rec_obj <- healthyR.ai::hai_earth_data_prepper(train, f)

  best_metric <- switch (.model_type,
    "regression"     = "rmse",
    "classification" = "accuracy"
  )

  auto_earth <- healthyR.ai::hai_auto_earth(
    .data        = train,
    .rec_obj     = rec_obj,
    .tune        = .tune,
    .grid_size   = .grid_size,
    .num_cores   = .num_cores,
    .model_type  = .model_type,
    .best_metric = best_metric
  )

  best_model <- auto_earth$model_info$fitted_wflw

  # Check performance
  test_pred <- stats::predict(best_model, new_data = test)

  df_test <- tibble::tibble(
    actual = test[[.target]],
    pred   = test_pred$.pred
  )

  mod_rmse     <- yardstick::rmse_vec(df_test$actual, df_test$pred)
  mod_mae      <- yardstick::mae_vec(df_test$actual, df_test$pred)
  mod_rsq_trad <- yardstick::rsq_trad_vec(df_test$actual, df_test$pred)
  mod_acc      <- 100 - yardstick::smape_vec(df_test$actual, df_test$pred)

  tb <- tibble::tibble(
    rmse = mod_rmse |> round(2),
    mae  = mod_mae |> round(2),
    r2   = mod_rsq_trad |> round(2),
    acc  = mod_acc |> round(1)
  )

  inset_tbl <- tibble::tibble(
    x = df_test$actual[2],
    y = df_test$pred |> max(),
    tb = list(tb)
  )

  g <- ggplot2::ggplot(
    data = df_test,
    mapping = ggplot2::aes(x = actual, y = pred)
  ) +
    ggplot2::geom_abline(col = "green", lty = 2, lwd = 1) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::coord_fixed(ratio = 1) +
    ggpp::geom_table(
      data = inset_tbl,
      ggplot2::aes(x = x, y = y, label = tb)
    )

  return(
    list(
      model     = best_model,
      test_plot = g
    )
  )

}


#' train_ranger_model
#'
#' @param .data A time series with `date` (days) and `value` cols.
#' @param .target The name (string) of the target variable.
#' @param .prop The proportion of data to be retained for modeling/analysis.
#' @param .strat Logical, whether or not to conduct stratified sampling by
#' '.target'
#' @param .tune Default is TRUE, this will create a tuning grid and tuned
#' workflow.
#' @param .grid_size Default is 10.
#' @param .num_cores Default is 1.
#' @param .model_type Default is `regression`, can also be `classification.`
#' @param .surrogate_model Logical, whether or not to conduct surrogate
#' modeling, i.e., use all data to train the model.
#'
#' @return A list.
#' @export
#'
train_ranger_model <- function(
    .data,
    .target,
    .prop            = 0.8,
    .strat           = FALSE,
    .tune            = TRUE,
    .grid_size       = 10,
    .num_cores       = 1,
    .model_type      = "regression",
    .surrogate_model = FALSE
) {

  # missing safety checks

  f <- stats::as.formula(stringr::str_glue("{.target} ~ ."))
  .data$date <- NULL

  # Splits
  set.seed(1)

  if (!.surrogate_model) {

    if (.strat) {
      splits <- rsample::initial_split(data = .data, prop = .prop,
                                       strata = .target)
    } else {
      splits <- rsample::initial_split(data = .data, prop = .prop)
    }

    train <- rsample::training(splits)
    test  <- rsample::testing(splits)

  } else { # surrogate model

    train <- .data
    test  <- .data

  }

  rec_obj <- healthyR.ai::hai_ranger_data_prepper(train, f)

  best_metric <- switch (.model_type,
                         "regression"     = "rmse",
                         "classification" = "accuracy"
  )

  auto_ranger <- hai_auto_ranger(
    .data        = train,
    .rec_obj     = rec_obj,
    .tune        = .tune,
    .grid_size   = .grid_size,
    .num_cores   = .num_cores,
    .model_type  = .model_type,
    .best_metric = best_metric
  )

  best_model <- auto_ranger$model_info$fitted_wflw

  # Check performance
  test_pred <- stats::predict(best_model, new_data = test)

  df_test <- tibble::tibble(
    actual = test[[.target]],
    pred   = test_pred$.pred
  )

  mod_rmse     <- yardstick::rmse_vec(df_test$actual, df_test$pred)
  mod_mae      <- yardstick::mae_vec(df_test$actual, df_test$pred)
  mod_rsq_trad <- yardstick::rsq_trad_vec(df_test$actual, df_test$pred)
  mod_acc      <- 100 - yardstick::smape_vec(df_test$actual, df_test$pred)

  tb <- tibble::tibble(
    rmse = mod_rmse |> round(2),
    mae  = mod_mae |> round(2),
    r2   = mod_rsq_trad |> round(2),
    acc  = mod_acc |> round(1)
  )

  inset_tbl <- tibble::tibble(
    x = df_test$actual[2],
    y = df_test$pred |> max(),
    tb = list(tb)
  )

  g <- ggplot2::ggplot(
    data = df_test,
    mapping = ggplot2::aes(x = actual, y = pred)
  ) +
    ggplot2::geom_abline(col = "green", lty = 2, lwd = 1) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::coord_fixed(ratio = 1) +
    ggpp::geom_table(
      data = inset_tbl,
      ggplot2::aes(x = x, y = y, label = tb)
    )

  return(
    list(
      model     = best_model,
      test_plot = g
    )
  )

}


#' train_knn_model
#'
#' @param .data A time series with `date` (days) and `value` cols.
#' @param .target The name (string) of the target variable.
#' @param .prop The proportion of data to be retained for modeling/analysis.
#' @param .strat Logical, whether or not to conduct stratified sampling by
#' '.target'
#' @param .tune Default is TRUE, this will create a tuning grid and tuned
#' workflow.
#' @param .grid_size Default is 10.
#' @param .num_cores Default is 1.
#' @param .model_type Default is `regression`, can also be `classification.`
#' @param .surrogate_model Logical, whether or not to conduct surrogate
#' modeling, i.e., use all data to train the model.
#'
#' @return A list.
#' @export
#'
train_knn_model <- function(
    .data,
    .target,
    .prop            = 0.8,
    .strat           = FALSE,
    .tune            = TRUE,
    .grid_size       = 10,
    .num_cores       = 1,
    .model_type      = "regression",
    .surrogate_model = FALSE
) {

  # missing safety checks

  f <- stats::as.formula(stringr::str_glue("{.target} ~ ."))
  .data$date <- NULL

  # Splits
  set.seed(1)

  if (!.surrogate_model) {

    if (.strat) {
      splits <- rsample::initial_split(data = .data, prop = .prop,
                                       strata = .target)
    } else {
      splits <- rsample::initial_split(data = .data, prop = .prop)
    }

    train <- rsample::training(splits)
    test  <- rsample::testing(splits)

  } else { # surrogate model

    train <- .data
    test  <- .data

  }

  rec_obj <- healthyR.ai::hai_knn_data_prepper(train, f)

  best_metric <- switch (.model_type,
                         "regression"     = "rmse",
                         "classification" = "accuracy"
  )

  auto_knn <- healthyR.ai::hai_auto_knn(
    .data        = train,
    .rec_obj     = rec_obj,
    .tune        = .tune,
    .grid_size   = .grid_size,
    .num_cores   = .num_cores,
    .model_type  = .model_type,
    .best_metric = best_metric
  )

  best_model <- auto_knn$model_info$fitted_wflw

  # Check performance
  test_pred <- stats::predict(best_model, new_data = test)

  df_test <- tibble::tibble(
    actual = test[[.target]],
    pred   = test_pred$.pred
  )

  mod_rmse     <- yardstick::rmse_vec(df_test$actual, df_test$pred)
  mod_mae      <- yardstick::mae_vec(df_test$actual, df_test$pred)
  mod_rsq_trad <- yardstick::rsq_trad_vec(df_test$actual, df_test$pred)
  mod_acc      <- 100 - yardstick::smape_vec(df_test$actual, df_test$pred)

  tb <- tibble::tibble(
    rmse = mod_rmse |> round(2),
    mae  = mod_mae |> round(2),
    r2   = mod_rsq_trad |> round(2),
    acc  = mod_acc |> round(1)
  )

  inset_tbl <- tibble::tibble(
    x = df_test$actual[2],
    y = df_test$pred |> max(),
    tb = list(tb)
  )

  g <- ggplot2::ggplot(
    data = df_test,
    mapping = ggplot2::aes(x = actual, y = pred)
  ) +
    ggplot2::geom_abline(col = "green", lty = 2, lwd = 1) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::coord_fixed(ratio = 1) +
    ggpp::geom_table(
      data = inset_tbl,
      ggplot2::aes(x = x, y = y, label = tb)
    )

  return(
    list(
      model     = best_model,
      test_plot = g
    )
  )

}


#' train_glmnet_model
#'
#' @param .data A time series with `date` (days) and `value` cols.
#' @param .target The name (string) of the target variable.
#' @param .prop The proportion of data to be retained for modeling/analysis.
#' @param .strat Logical, whether or not to conduct stratified sampling by
#' '.target'
#' @param .tune Default is TRUE, this will create a tuning grid and tuned
#' workflow.
#' @param .grid_size Default is 10.
#' @param .num_cores Default is 1.
#' @param .model_type Default is `regression`, can also be `classification.`
#' @param .surrogate_model Logical, whether or not to conduct surrogate
#' modeling, i.e., use all data to train the model.
#'
#' @return A list.
#' @export
#'
train_glmnet_model <- function(
    .data,
    .target,
    .prop            = 0.8,
    .strat           = FALSE,
    .tune            = TRUE,
    .grid_size       = 10,
    .num_cores       = 1,
    .model_type      = "regression",
    .surrogate_model = FALSE
) {

  # missing safety checks

  f <- stats::as.formula(stringr::str_glue("{.target} ~ ."))
  .data$date <- NULL

  # Splits
  set.seed(1)

  if (!.surrogate_model) {

    if (.strat) {
      splits <- rsample::initial_split(data = .data, prop = .prop,
                                       strata = .target)
    } else {
      splits <- rsample::initial_split(data = .data, prop = .prop)
    }

    train <- rsample::training(splits)
    test  <- rsample::testing(splits)

  } else { # surrogate model

    train <- .data
    test  <- .data

  }

  rec_obj <- healthyR.ai::hai_glmnet_data_prepper(train, f)

  best_metric <- switch (.model_type,
                         "regression"     = "rmse",
                         "classification" = "accuracy"
  )

  auto_glmnet <- healthyR.ai::hai_auto_glmnet(
    .data        = train,
    .rec_obj     = rec_obj,
    .tune        = .tune,
    .grid_size   = .grid_size,
    .num_cores   = .num_cores,
    .model_type  = .model_type,
    .best_metric = best_metric
  )

  best_model <- auto_glmnet$model_info$fitted_wflw

  # Check performance
  test_pred <- stats::predict(best_model, new_data = test)

  df_test <- tibble::tibble(
    actual = test[[.target]],
    pred   = test_pred$.pred
  )

  mod_rmse     <- yardstick::rmse_vec(df_test$actual, df_test$pred)
  mod_mae      <- yardstick::mae_vec(df_test$actual, df_test$pred)
  mod_rsq_trad <- yardstick::rsq_trad_vec(df_test$actual, df_test$pred)
  mod_acc      <- 100 - yardstick::smape_vec(df_test$actual, df_test$pred)

  tb <- tibble::tibble(
    rmse = mod_rmse |> round(2),
    mae  = mod_mae |> round(2),
    r2   = mod_rsq_trad |> round(2),
    acc  = mod_acc |> round(1)
  )

  inset_tbl <- tibble::tibble(
    x = df_test$actual[2],
    y = df_test$pred |> max(),
    tb = list(tb)
  )

  g <- ggplot2::ggplot(
    data = df_test,
    mapping = ggplot2::aes(x = actual, y = pred)
  ) +
    ggplot2::geom_abline(col = "green", lty = 2, lwd = 1) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::coord_fixed(ratio = 1) +
    ggpp::geom_table(
      data = inset_tbl,
      ggplot2::aes(x = x, y = y, label = tb)
    )

  return(
    list(
      model     = best_model,
      test_plot = g
    )
  )

}


#' train_c50_model
#'
#' @param .data A time series with `date` (days) and `value` cols.
#' @param .target The name (string) of the target variable.
#' @param .prop The proportion of data to be retained for modeling/analysis.
#' @param .strat Logical, whether or not to conduct stratified sampling by
#' '.target'
#' @param .tune Default is TRUE, this will create a tuning grid and tuned
#' workflow.
#' @param .grid_size Default is 10.
#' @param .num_cores Default is 1.
#' @param .model_type Default is `regression`, can also be `classification.`
#' @param .surrogate_model Logical, whether or not to conduct surrogate
#' modeling, i.e., use all data to train the model.
#'
#' @return A list.
#' @export
#'
train_c50_model <- function(
    .data,
    .target,
    .prop            = 0.8,
    .strat           = FALSE,
    .tune            = TRUE,
    .grid_size       = 10,
    .num_cores       = 1,
    .model_type      = "classification",
    .surrogate_model = FALSE
) {

  # missing safety checks

  f <- stats::as.formula(stringr::str_glue("{.target} ~ ."))
  .data$date <- NULL

  # Splits
  set.seed(1)

  if (!.surrogate_model) {

    if (.strat) {
      splits <- rsample::initial_split(data = .data, prop = .prop,
                                       strata = .target)
    } else {
      splits <- rsample::initial_split(data = .data, prop = .prop)
    }

    train <- rsample::training(splits)
    test  <- rsample::testing(splits)

  } else { # surrogate model

    train <- .data
    test  <- .data

  }

  rec_obj <- healthyR.ai::hai_c50_data_prepper(train, f)

  best_metric <- switch (.model_type,
                         "regression"     = "rmse",
                         "classification" = "accuracy"
  )

  auto_c50 <- healthyR.ai::hai_auto_c50(
    .data        = train,
    .rec_obj     = rec_obj,
    .tune        = .tune,
    .grid_size   = .grid_size,
    .num_cores   = .num_cores,
    .model_type  = .model_type,
    .best_metric = best_metric
  )

  best_model <- auto_c50$model_info$fitted_wflw

  # Check performance
  test_pred <- stats::predict(best_model, new_data = test)

  df_test <- tibble::tibble(
    actual = test[[.target]],
    pred   = test_pred$.pred
  )

  mod_rmse     <- yardstick::rmse_vec(df_test$actual, df_test$pred)
  mod_mae      <- yardstick::mae_vec(df_test$actual, df_test$pred)
  mod_rsq_trad <- yardstick::rsq_trad_vec(df_test$actual, df_test$pred)
  mod_acc      <- 100 - yardstick::smape_vec(df_test$actual, df_test$pred)

  tb <- tibble::tibble(
    rmse = mod_rmse |> round(2),
    mae  = mod_mae |> round(2),
    r2   = mod_rsq_trad |> round(2),
    acc  = mod_acc |> round(1)
  )

  inset_tbl <- tibble::tibble(
    x = df_test$actual[2],
    y = df_test$pred |> max(),
    tb = list(tb)
  )

  g <- ggplot2::ggplot(
    data = df_test,
    mapping = ggplot2::aes(x = actual, y = pred)
  ) +
    ggplot2::geom_abline(col = "green", lty = 2, lwd = 1) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::coord_fixed(ratio = 1) +
    ggpp::geom_table(
      data = inset_tbl,
      ggplot2::aes(x = x, y = y, label = tb)
    )

  return(
    list(
      model     = best_model,
      test_plot = g
    )
  )

}


#' coeteris_paribus
#'
#' @param .model A fitted model from `tidymodels`.
#' @param .newdata A data frame.
#' @param .target The name of the dependent variable.
#'
#' @return A `ggplot2` plot.
#' @export
#'
coeteris_paribus <- function(
    .model,
    .newdata,
    .target
  ) {

  # create custom predict function
  pred <- function(model, newdata) {
    results <- stats::predict(model, newdata) |> dplyr::pull(.pred)
    return(results)
  }

  # create an explainer for the model
  x <- setdiff(names(.newdata), .target)
  y <- .target

  explainer <- DALEX::explain(
    model            = .model,
    data             = .newdata[, x],
    y                = .newdata[[y]],
    predict_function = pred,
    label            = "Best Fit"
  )

  # ceteris paribus analysis
  cp <- ingredients::ceteris_paribus(explainer, utils::tail(.newdata, 1))
  vars <- setdiff(names(.newdata), .target)
  cp_plt <- lapply(vars, \(x) plot(cp, variables = x))
  names(cp_plt) <- vars

  return(cp_plt)

}


#' plant_performance_map
#'
#' @param .model A `tidymodels` fitted model.
#' @param .data A data frame with the training set.
#' @param .xvar The first input.
#' @param .yvar The second input.
#' @param .zvar The dependent variable.
#' @param .res 3D plot resolution.
#' @param .use_rgl Boolean. If TRUE, a 3D plot is produced by the `{rgl}`
#' package.
#'
#' @return A `plotly` plot.
#' @export
#'
plant_performance_map <- function(
    .model,
    .data,
    .xvar,
    .yvar,
    .zvar,
    .res = 16, # 3d plot resolution
    .use_rgl = FALSE # just return the plotly plot
  ) {

  # create custom predict function
  pred <- function(model, newdata) {
    results <- stats::predict(model, newdata) |> dplyr::pull(.pred)
    return(results)
  }

  # Fixed variables from this point on:
  row_nr <- nrow(.data)

  # Given a model, predict zvar from xvar and yvar
  # Defaults to range of x and y variables, and a 16x16 grid
  predictgrid <- function(dat, model, xvar, yvar, zvar, row_nr = 1, res = 16) {
    # Find the range of the predictor variable. This works for lm and glm
    # and some others, but may require customization for others.
    xrange <- range(dat[[xvar]])
    yrange <- range(dat[[yvar]])

    newdata <- dat[row_nr, 1:(ncol(dat) - 1)]
    other_cols <- setdiff(names(newdata), c(xvar, yvar))
    new_data <- expand.grid(
      x = seq(xrange[1], xrange[2], length.out = res),
      y = seq(yrange[1], yrange[2], length.out = res)
    )
    names(new_data) <- c(xvar, yvar)
    new_data[, other_cols] <- newdata[1, other_cols]

    new_data[[zvar]] <- pred(model, new_data)

    new_data
  }

  # use it
  data_grid <- predictgrid(.data, .model, .xvar, .yvar, .zvar, row_nr, .res)

  # Convert long-style data frame with x, y, and z vars into a list
  # with x and y as row/column values, and z as a matrix.
  df2mat <- function(p, xvar = NULL, yvar = NULL, zvar = NULL) {
    if (is.null(xvar)) xvar <- names(p)[1]
    if (is.null(yvar)) yvar <- names(p)[2]
    if (is.null(zvar)) zvar <- names(p)[3]

    x <- unique(p[[xvar]])
    y <- unique(p[[yvar]])
    z <- matrix(p[[zvar]], nrow = length(y), ncol = length(x))

    m <- list(x, y, z)
    names(m) <- c(xvar, yvar, zvar)
    m
  }

  # use it
  data_list <- df2mat(data_grid, .xvar, .yvar, .zvar)

  # * 3D Plots
  Z <- data_list[[.zvar]]

  ppm <- plotly::plot_ly(
    x = data_list[[.xvar]],
    y = data_list[[.yvar]],
    z = Z
  ) |>
    plotly::add_surface(
      contours = list(
        z = list(
          show = TRUE,
          usecolormap = TRUE,
          highlightcolor = "#ff0000",
          project = list(z = TRUE)
        )
      )
    ) |>
    plotly::layout(
      title = "Performance Map",
      scene = list(
        xaxis = list(title = .xvar),
        yaxis = list(title = .yvar),
        zaxis = list(title = .zvar))
    )

  if (.use_rgl) {

    # Make the plot with the data points
    rgl::plot3d(data_list[[.xvar]], data_list[[.yvar]], data_list[[.zvar]],
                xlab = "", ylab = "", zlab = "", axes = FALSE, type = "n")

    # Add the mesh of predicted values
    color <- rev(grDevices::rainbow(.res, start = 0/6, end = 4/6))
    zcol  <- cut(data_list[[.zvar]], .res)
    rgl::persp3d(data_list[[.xvar]], data_list[[.yvar]], data_list[[.zvar]],
                 aspect = c(1, 1, 1), col = color[zcol],
                 xlab = "", ylab = "", zlab = "",
                 polygon_offset = 1)

    # Draw the box
    rgl::rgl.bbox(color    = "grey50",          # grey60 surface and black text
                  emission = "grey50",          # emission color is grey50
                  xlen = 0, ylen = 0, zlen = 0) # Don't add tick marks

    # Set default color of future objects to black
    rgl::rgl.material(color = "black")

    # Add axes to specific sides: "x--", "x-+", "x+-" and "x++
    rgl::axes3d(edges = c("x--", "y+-", "z--"),
                ntick = 6,   # Attempt 6 tick marks on each side
                cex   = .75) # Smaller font

    # Add axis labels. 'line' specifies how far to set the label from the axis.
    rgl::mtext3d(.xvar, edge = "x--", line = 4)
    rgl::mtext3d(.yvar, edge = "y+-", line = 4)
    rgl::mtext3d(.zvar, edge = "z--", line = 4)

  }

  return(ppm)

}


# custom mean
my_mean <- function(x, na.rm=TRUE) {
  mean(x, na.rm = na.rm)
}


#' cluster_ts
#'
#' @param .tag_dat A time series with `date` and `value` cols.
#' @param .clusters The number of clusters.
#'
#' @return A list.
#' @export
#'
cluster_ts <- function(
    .tag_dat,
    .clusters
) {

  full_data_tbl <- .tag_dat |>
    tidyr::pivot_longer(-date)

  # clustering process
  tsfeature_tbl <- full_data_tbl |>
    dplyr::group_by(name) |>
    timetk::tk_tsfeatures(
      .date_var = date,
      .value    = value,
      .period   = "day",
      .features = c("frequency", "stl_features", "entropy", "acf_features",
                    "my_mean"),
      .scale    = TRUE,
      .prefix   = "ts_"
    ) |>
    dplyr::ungroup()

  set.seed(123)

  cluster_tbl <- tibble::tibble(
    cluster = tsfeature_tbl |>
      dplyr::select(-name) |>
      dplyr::mutate_all(tidyr::replace_na, 0) |>
      as.matrix() |>
      stats::kmeans(centers = .clusters, nstart = 100) |>
      purrr::pluck("cluster")
  ) |>
    dplyr::bind_cols(
      tsfeature_tbl
    )

  cluster_lookup_tbl <- cluster_tbl |>
    dplyr::select(name, cluster) |>
    dplyr::arrange(name)

  full_data_tbl$cluster <- rep(1, nrow(full_data_tbl))

  full_data_tbl$cluster <- full_data_tbl |>
    dplyr::select(name) |>
    as.vector() |>
    sapply(
      \(x) {
        tidyquant::VLOOKUP(
          .lookup_values = x,
          .data          = cluster_lookup_tbl,
          .lookup_column = name,
          .return_column = cluster
        )
      }
    ) |>
    as.numeric()

  return(
    full_data_tbl |>
      purrr::set_names(c("date", "tag", "value", "cluster")) |>
      split(~cluster)
  )

}


# calibrate_and_plot
#
calibrate_and_plot <- function(..., .splits, .actual_data,
                               .show_ci = FALSE, type = "testing") {

  if (type == "testing") {
    new_data <- rsample::testing(.splits)
  } else {
    new_data <- rsample::training(.splits) |> tidyr::drop_na()
  }

  calibration_tbl <- modeltime::modeltime_table(...) |>
    modeltime::modeltime_calibrate(new_data)

  acc_tbl <- calibration_tbl |> modeltime::modeltime_accuracy()

  fore_dat <- calibration_tbl |>
    modeltime::modeltime_forecast(
      new_data    = new_data,
      actual_data = .actual_data
    )

  fore_plt <- fore_dat |>
    modeltime::plot_modeltime_forecast(.conf_interval_show = .show_ci)

  return(
    list(
      calibration = calibration_tbl,
      metrics     = acc_tbl,
      forecast    = list(data = fore_dat, plot = fore_plt)
    )
  )

}


#' forecast_prophet
#'
#' @param .tag_dat A time series with `date` and `value` cols.
#' @param .assess The number of samples used for each assessment resample.
#' @param .horiz The forecast horizon.
#'
#' @return A `parsnip` (PROPHET) model object.
#' @export
#'
forecast_prophet <- function(
  .tag_dat,
  .assess = 1,
  .horiz  = 1
) {

  splits <- timetk::time_series_split(
    data       = .tag_dat,
    date_var   = date,
    assess     = .assess,
    cumulative = TRUE
  )

  model_fit_prophet <- modeltime::prophet_reg() |>
    parsnip::set_engine("prophet") |>
    parsnip::fit(value ~ date, data = rsample::training(splits))

  output <- calibrate_and_plot(model_fit_prophet, .splits = splits,
                               .actual_data = .tag_dat)

  model_refit_prophet <- output$calibration |>
    modeltime::modeltime_refit(data = .tag_dat)

  fore_dat <- model_refit_prophet |>
    modeltime::modeltime_forecast(
      h = .horiz,
      actual_data = .tag_dat
    )

  fore_plt <- fore_dat |>
    modeltime::plot_modeltime_forecast(
      .conf_interval_show = TRUE
    )

  attr(output, "model_fit")   <- model_fit_prophet
  attr(output, "model_refit") <- model_refit_prophet
  attr(output, "forecast")    <- fore_dat
  attr(output, "plot")        <- fore_plt

  return(output)

}


#' forecast_nnetar
#'
#' @param .tag_dat A time series with `date` and `value` cols.
#' @param .assess The number of samples used for each assessment resample.
#' @param .horiz The forecast horizon.
#' @param .seasonal_period A seasonal frequency. Uses "auto" by default. A
#' character phrase of "auto" or time-based phrase of "2 weeks" can be used if
#' a date or date-time variable is provided.
#'
#' @return A `parsnip` (NNAR) model object.
#' @export
#'
forecast_nnetar <- function(
    .tag_dat,
    .assess = 1,
    .horiz  = 1,
    .seasonal_period = "auto"
) {

  splits <- timetk::time_series_split(
    data       = .tag_dat,
    date_var   = date,
    assess     = .assess,
    cumulative = TRUE
  )

  set.seed(123)
  model_fit_nnetar <- modeltime::nnetar_reg(
    seasonal_period = .seasonal_period,
    hidden_units    = 10,
    penalty         = 10,
    num_networks    = 10,
    epochs          = 100
  ) |>
    parsnip::set_engine("nnetar") |>
    parsnip::fit(value ~ date, rsample::training(splits))

  output <- calibrate_and_plot(model_fit_nnetar, .splits = splits,
                               .actual_data = .tag_dat)

  model_refit_nnetar <- output$calibration |>
    modeltime::modeltime_refit(data = .tag_dat)

  fore_dat <- model_refit_nnetar |>
    modeltime::modeltime_forecast(
      h = .horiz,
      actual_data = .tag_dat
    )

  fore_plt <- fore_dat |>
    modeltime::plot_modeltime_forecast(
      .conf_interval_show = TRUE
    )

  attr(output, "model_fit")   <- model_fit_nnetar
  attr(output, "model_refit") <- model_refit_nnetar
  attr(output, "forecast")    <- fore_dat
  attr(output, "plot")        <- fore_plt

  return(output)

}


#' forecast_tbats
#'
#' @param .tag_dat A time series with `date` and `value` cols.
#' @param .assess The number of samples used for each assessment resample.
#' @param .horiz The forecast horizon.
#' @param .seasonal_period_1 (required) The primary seasonal frequency. Uses
#' "auto" by default. A character phrase of "auto" or time-based phrase of "2
#' weeks" can be used if a date or date-time variable is provided.
#' @param .seasonal_period_2 (optional) A second seasonal frequency. Is NULL
#' by default. A character phrase of "auto" or time-based phrase of "2 weeks"
#' can be used if a date or date-time variable is provided.
#' @param .seasonal_period_3 (optional) A third seasonal frequency. Is NULL by
#' default. A character phrase of "auto" or time-based phrase of "2 weeks" can
#' be used if a date or date-time variable is provided.
#'
#' @return A `parsnip` (BATS) model object.
#' @export
#'
forecast_tbats <- function(
    .tag_dat,
    .assess = 1,
    .horiz  = 1,
    .seasonal_period_1 = "auto",
    .seasonal_period_2 = NULL,
    .seasonal_period_3 = NULL
) {

  splits <- timetk::time_series_split(
    data       = .tag_dat,
    date_var   = date,
    assess     = .assess,
    cumulative = TRUE
  )

  model_fit_tbats <- modeltime::seasonal_reg(
    seasonal_period_1 = .seasonal_period_1,
    seasonal_period_2 = .seasonal_period_2,
    seasonal_period_3 = .seasonal_period_3
  ) |>
    parsnip::set_engine("tbats") |>
    parsnip::fit(value ~ date, rsample::training(splits))

  output <- calibrate_and_plot(model_fit_tbats, .splits = splits,
                               .actual_data = .tag_dat)

  model_refit_tbats <- output$calibration |>
    modeltime::modeltime_refit(data = .tag_dat)

  fore_dat <- model_refit_tbats |>
    modeltime::modeltime_forecast(
      h = .horiz,
      actual_data = .tag_dat
    )

  fore_plt <- fore_dat |>
    modeltime::plot_modeltime_forecast(
      .conf_interval_show = TRUE
    )

  attr(output, "model_fit")   <- model_fit_tbats
  attr(output, "model_refit") <- model_refit_tbats
  attr(output, "forecast")    <- fore_dat
  attr(output, "plot")        <- fore_plt

  return(output)

}


#' forecast_stlm
#'
#' @param .tag_dat A time series with `date` and `value` cols.
#' @param .assess The number of samples used for each assessment resample.
#' @param .horiz The forecast horizon.
#' @param .seasonal_period_1 (required) The primary seasonal frequency. Uses
#' "auto" by default. A character phrase of "auto" or time-based phrase of "2
#' weeks" can be used if a date or date-time variable is provided.
#' @param .seasonal_period_2 (optional) A second seasonal frequency. Is NULL
#' by default. A character phrase of "auto" or time-based phrase of "2 weeks"
#' can be used if a date or date-time variable is provided.
#' @param .seasonal_period_3 (optional) A third seasonal frequency. Is NULL by
#' default. A character phrase of "auto" or time-based phrase of "2 weeks" can
#' be used if a date or date-time variable is provided.
#' @param .algo A character string for the software that should be used to fit
#' the model, "ets" or "arima".
#'
#' @return A `parsnip` (STLM) model object.
#' @export
#'
forecast_stlm <- function(
    .tag_dat,
    .assess            = 1,
    .horiz             = 1,
    .seasonal_period_1 = "auto",
    .seasonal_period_2 = NULL,
    .seasonal_period_3 = NULL,
    .algo              = "ets"
) {

  splits <- timetk::time_series_split(
    data       = .tag_dat,
    date_var   = date,
    assess     = .assess,
    cumulative = TRUE
  )

  suffix <- switch (.algo,
    "ets" = "ets",
    "arima"
  )

  engine <- paste0("stlm_", suffix)

  model_fit_stlm <- modeltime::seasonal_reg(
    seasonal_period_1 = .seasonal_period_1,
    seasonal_period_2 = .seasonal_period_2,
    seasonal_period_3 = .seasonal_period_3
  ) |>
    parsnip::set_engine(engine) |>
    parsnip::fit(value ~ date, rsample::training(splits))

  output <- calibrate_and_plot(model_fit_stlm, .splits = splits,
                               .actual_data = .tag_dat)

  model_refit_stlm <- output$calibration |>
    modeltime::modeltime_refit(data = .tag_dat)

  fore_dat <- model_refit_stlm |>
    modeltime::modeltime_forecast(
      h = .horiz,
      actual_data = .tag_dat
    )

  fore_plt <- fore_dat |>
    modeltime::plot_modeltime_forecast(
      .conf_interval_show = TRUE
    )

  attr(output, "model_fit")   <- model_fit_stlm
  attr(output, "model_refit") <- model_refit_stlm
  attr(output, "forecast")    <- fore_dat
  attr(output, "plot")        <- fore_plt

  return(output)

}
