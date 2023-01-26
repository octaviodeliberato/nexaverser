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
