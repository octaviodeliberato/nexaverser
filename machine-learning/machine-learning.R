#' train_cubist_model
#'
#' @param .data A data frame with `date` and numeric cols.
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
    .grid_size       = 10L,
    .num_cores       = 1L,
    .best_metric     = "rmse",
    .surrogate_model = FALSE
) {

  # Check if .data inherits from either "data.frame", "tbl" or 'tbl_df"
  if (!inherits(.data, c("data.frame", "tbl", "tbl_df"))) {
    stop(
      "Argument \".data\" must be a data.frame, tbl or tbl_df.",
      call. = FALSE
    )
  }

  # Check if .target is a string
  if (!is.character(.target)) {
    stop(
      "Argument \".target\" must be a string.",
      call. = FALSE
    )
  }

  # Check if .target is in .data
  if (!.target %in% colnames(.data)) {
    stop(
      "Argument \".target\" must be a column in \".data\".",
      call. = FALSE
    )
  }

  # Check if .prop is numeric
  if (!is.numeric(.prop)) {
    stop(
      "Argument \".prop\" must be numeric.",
      call. = FALSE
    )
  }

  # Check if .prop is between 0 and 1
  if (.prop < 0 || .prop > 1) {
    stop(
      "Argument \".prop\" must be between 0 and 1.",
      call. = FALSE
    )
  }

  # Check if .strat is logical
  if (!is.logical(.strat)) {
    stop(
      "Argument \".strat\" must be logical.",
      call. = FALSE
    )
  }

  # Check if .tune is logical
  if (!is.logical(.tune)) {
    stop(
      "Argument \".tune\" must be logical.",
      call. = FALSE
    )
  }

  # Check if .grid_size is integer
  if (!is.integer(.grid_size)) {
    stop(
      "Argument \".grid_size\" must be integer.",
      call. = FALSE
    )
  }

  # Check if .num_cores is integer
  if (!is.integer(.num_cores)) {
    stop(
      "Argument \".num_cores\" must be integer.",
      call. = FALSE
    )
  }

  # Check if .best_metric is a string
  if (!is.character(.best_metric)) {
    stop(
      "Argument \".best_metric\" must be a string.",
      call. = FALSE
    )
  }

  # Check if .best_metric is in c("rmse", "mae", "r2", "acc")
  if (!.best_metric %in% c("rmse", "mae", "r2", "acc")) {
    stop(
      "Argument \".best_metric\" must be one of \"rmse\", \"mae\", \"r2\", \"acc\".", # nolint: line_length_linter.
      call. = FALSE
    )
  }

  # Check if .surrogate_model is logical
  if (!is.logical(.surrogate_model)) {
    stop(
      "Argument \".surrogate_model\" must be logical.",
      call. = FALSE
    )
  }

  # Check if package "rules" is installed
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
    ggplot2::geom_smooth(col = "green", lty = 2, lwd = 1, method = "lm") +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::coord_fixed(ratio = 1) +
    ggpp::geom_table(
      data = inset_tbl,
      ggplot2::aes(x = x, y = y, label = tb)
    )

  auto_cube$test_plot <- g

  auto_cube$model <- best_model

  return(auto_cube)

}


#' train_xgboost_model
#'
#' @param .data A data frame with `date` and numeric cols.
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
    .grid_size       = 10L,
    .num_cores       = 1L,
    .model_type      = "regression",
    .surrogate_model = FALSE
) {

  # Check if .data inherits from either "data.frame", "tbl" or 'tbl_df"
  if (!inherits(.data, c("data.frame", "tbl", "tbl_df"))) {
    stop(
      "Argument \".data\" must be a data.frame, tbl or tbl_df.",
      call. = FALSE
    )
  }

  # Check if .target is a string
  if (!is.character(.target)) {
    stop(
      "Argument \".target\" must be a string.",
      call. = FALSE
    )
  }

  # Check if .target is in .data
  if (!.target %in% colnames(.data)) {
    stop(
      "Argument \".target\" must be a column in \".data\".",
      call. = FALSE
    )
  }

  # Check if .prop is numeric
  if (!is.numeric(.prop)) {
    stop(
      "Argument \".prop\" must be numeric.",
      call. = FALSE
    )
  }

  # Check if .prop is between 0 and 1
  if (.prop < 0 || .prop > 1) {
    stop(
      "Argument \".prop\" must be between 0 and 1.",
      call. = FALSE
    )
  }

  # Check if .strat is logical
  if (!is.logical(.strat)) {
    stop(
      "Argument \".strat\" must be logical.",
      call. = FALSE
    )
  }

  # Check if .tune is logical
  if (!is.logical(.tune)) {
    stop(
      "Argument \".tune\" must be logical.",
      call. = FALSE
    )
  }

  # Check if .grid_size is integer
  if (!is.integer(.grid_size)) {
    stop(
      "Argument \".grid_size\" must be integer.",
      call. = FALSE
    )
  }

  # Check if .num_cores is integer
  if (!is.integer(.num_cores)) {
    stop(
      "Argument \".num_cores\" must be integer.",
      call. = FALSE
    )
  }

  # Check if .model_type is either "regression" or "classification"
  if (!.model_type %in% c("regression", "classification")) {
    stop(
      "Argument \".model_type\" must be either \"regression\" or \"classification\".", # nolint: line_length_linter.
      call. = FALSE
    )
  }

  # Check if .surrogate_model is logical
  if (!is.logical(.surrogate_model)) {
    stop(
      "Argument \".surrogate_model\" must be logical.",
      call. = FALSE
    )
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

  rec_obj <- healthyR.ai::hai_xgboost_data_prepper(train, f)

  best_metric <- switch(
    .model_type,
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

  if (.model_type == "regression") {

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
      ggplot2::geom_smooth(col = "green", lty = 2, lwd = 1, method = "lm") +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::coord_fixed(ratio = 1) +
      ggpp::geom_table(
        data = inset_tbl,
        ggplot2::aes(x = x, y = y, label = tb)
      )

    auto_xgboost$test_plot <- g

  }

  auto_xgboost$model <- best_model

  return(auto_xgboost)

}


#' train_mars_model
#'
#' @param .data A data frame with `date` and numeric cols.
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
    .grid_size       = 10L,
    .num_cores       = 1L,
    .model_type      = "regression",
    .surrogate_model = FALSE
) {

  # Check if .data inherits from either "data.frame", "tbl" or 'tbl_df"
  if (!inherits(.data, c("data.frame", "tbl", "tbl_df"))) {
    stop(
      "Argument \".data\" must be a data.frame, tbl or tbl_df.",
      call. = FALSE
    )
  }

  # Check if .target is a string
  if (!is.character(.target)) {
    stop(
      "Argument \".target\" must be a string.",
      call. = FALSE
    )
  }

  # Check if .target is in .data
  if (!.target %in% colnames(.data)) {
    stop(
      "Argument \".target\" must be a column in \".data\".",
      call. = FALSE
    )
  }

  # Check if .prop is numeric
  if (!is.numeric(.prop)) {
    stop(
      "Argument \".prop\" must be numeric.",
      call. = FALSE
    )
  }

  # Check if .prop is between 0 and 1
  if (.prop < 0 || .prop > 1) {
    stop(
      "Argument \".prop\" must be between 0 and 1.",
      call. = FALSE
    )
  }

  # Check if .strat is logical
  if (!is.logical(.strat)) {
    stop(
      "Argument \".strat\" must be logical.",
      call. = FALSE
    )
  }

  # Check if .tune is logical
  if (!is.logical(.tune)) {
    stop(
      "Argument \".tune\" must be logical.",
      call. = FALSE
    )
  }

  # Check if .grid_size is integer
  if (!is.integer(.grid_size)) {
    stop(
      "Argument \".grid_size\" must be integer.",
      call. = FALSE
    )
  }

  # Check if .num_cores is integer
  if (!is.integer(.num_cores)) {
    stop(
      "Argument \".num_cores\" must be integer.",
      call. = FALSE
    )
  }

  # Check if .model_type is either "regression" or "classification"
  if (!.model_type %in% c("regression", "classification")) {
    stop(
      "Argument \".model_type\" must be either \"regression\" or \"classification\".", # nolint: line_length_linter.
      call. = FALSE
    )
  }

  # Check if .surrogate_model is logical
  if (!is.logical(.surrogate_model)) {
    stop(
      "Argument \".surrogate_model\" must be logical.",
      call. = FALSE
    )
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

  rec_obj <- healthyR.ai::hai_earth_data_prepper(train, f)

  best_metric <- switch(
    .model_type,
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

  if (.model_type == "regression") {

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
      ggplot2::geom_smooth(col = "green", lty = 2, lwd = 1, method = "lm") +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::coord_fixed(ratio = 1) +
      ggpp::geom_table(
        data = inset_tbl,
        ggplot2::aes(x = x, y = y, label = tb)
      )

    auto_earth$test_plot <- g

  }

  auto_earth$model <- best_model

  return(auto_earth)

}


#' train_ranger_model
#'
#' @param .data A data frame with `date` and numeric cols.
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
    .grid_size       = 10L,
    .num_cores       = 1L,
    .model_type      = "regression",
    .surrogate_model = FALSE
) {

  # Check if .data inherits from either "data.frame", "tbl" or 'tbl_df"
  if (!inherits(.data, c("data.frame", "tbl", "tbl_df"))) {
    stop(
      "Argument \".data\" must be a data.frame, tbl or tbl_df.",
      call. = FALSE
    )
  }

  # Check if .target is a string
  if (!is.character(.target)) {
    stop(
      "Argument \".target\" must be a string.",
      call. = FALSE
    )
  }

  # Check if .target is in .data
  if (!.target %in% colnames(.data)) {
    stop(
      "Argument \".target\" must be a column in \".data\".",
      call. = FALSE
    )
  }

  # Check if .prop is numeric
  if (!is.numeric(.prop)) {
    stop(
      "Argument \".prop\" must be numeric.",
      call. = FALSE
    )
  }

  # Check if .prop is between 0 and 1
  if (.prop < 0 || .prop > 1) {
    stop(
      "Argument \".prop\" must be between 0 and 1.",
      call. = FALSE
    )
  }

  # Check if .strat is logical
  if (!is.logical(.strat)) {
    stop(
      "Argument \".strat\" must be logical.",
      call. = FALSE
    )
  }

  # Check if .tune is logical
  if (!is.logical(.tune)) {
    stop(
      "Argument \".tune\" must be logical.",
      call. = FALSE
    )
  }

  # Check if .grid_size is integer
  if (!is.integer(.grid_size)) {
    stop(
      "Argument \".grid_size\" must be integer.",
      call. = FALSE
    )
  }

  # Check if .num_cores is integer
  if (!is.integer(.num_cores)) {
    stop(
      "Argument \".num_cores\" must be integer.",
      call. = FALSE
    )
  }

  # Check if .model_type is either "regression" or "classification"
  if (!.model_type %in% c("regression", "classification")) {
    stop(
      "Argument \".model_type\" must be either \"regression\" or \"classification\".", # nolint: line_length_linter.
      call. = FALSE
    )
  }

  # Check if .surrogate_model is logical
  if (!is.logical(.surrogate_model)) {
    stop(
      "Argument \".surrogate_model\" must be logical.",
      call. = FALSE
    )
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

  rec_obj <- healthyR.ai::hai_ranger_data_prepper(train, f)

  best_metric <- switch(
    .model_type,
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

  if (.model_type == "regression") {

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
      ggplot2::geom_smooth(col = "green", lty = 2, lwd = 1, method = "lm") +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::coord_fixed(ratio = 1) +
      ggpp::geom_table(
        data = inset_tbl,
        ggplot2::aes(x = x, y = y, label = tb)
      )

    auto_ranger$test_plot <- g

  }

  auto_ranger$model <- best_model

  return(auto_ranger)

}


#' train_knn_model
#'
#' @param .data A data frame with `date` and numeric cols.
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
    .grid_size       = 10L,
    .num_cores       = 1L,
    .model_type      = "regression",
    .surrogate_model = FALSE
) {

  # Check if .data inherits from either "data.frame", "tbl" or 'tbl_df"
  if (!inherits(.data, c("data.frame", "tbl", "tbl_df"))) {
    stop(
      "Argument \".data\" must be a data.frame, tbl or tbl_df.",
      call. = FALSE
    )
  }

  # Check if .target is a string
  if (!is.character(.target)) {
    stop(
      "Argument \".target\" must be a string.",
      call. = FALSE
    )
  }

  # Check if .target is in .data
  if (!.target %in% colnames(.data)) {
    stop(
      "Argument \".target\" must be a column in \".data\".",
      call. = FALSE
    )
  }

  # Check if .prop is numeric
  if (!is.numeric(.prop)) {
    stop(
      "Argument \".prop\" must be numeric.",
      call. = FALSE
    )
  }

  # Check if .prop is between 0 and 1
  if (.prop < 0 || .prop > 1) {
    stop(
      "Argument \".prop\" must be between 0 and 1.",
      call. = FALSE
    )
  }

  # Check if .strat is logical
  if (!is.logical(.strat)) {
    stop(
      "Argument \".strat\" must be logical.",
      call. = FALSE
    )
  }

  # Check if .tune is logical
  if (!is.logical(.tune)) {
    stop(
      "Argument \".tune\" must be logical.",
      call. = FALSE
    )
  }

  # Check if .grid_size is integer
  if (!is.integer(.grid_size)) {
    stop(
      "Argument \".grid_size\" must be integer.",
      call. = FALSE
    )
  }

  # Check if .num_cores is integer
  if (!is.integer(.num_cores)) {
    stop(
      "Argument \".num_cores\" must be integer.",
      call. = FALSE
    )
  }

  # Check if .model_type is either "regression" or "classification"
  if (!.model_type %in% c("regression", "classification")) {
    stop(
      "Argument \".model_type\" must be either \"regression\" or \"classification\".", # nolint: line_length_linter.
      call. = FALSE
    )
  }

  # Check if .surrogate_model is logical
  if (!is.logical(.surrogate_model)) {
    stop(
      "Argument \".surrogate_model\" must be logical.",
      call. = FALSE
    )
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

  rec_obj <- healthyR.ai::hai_knn_data_prepper(train, f)

  best_metric <- switch(
    .model_type,
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

  if (.model_type == "regression") {

    # Prep the recipe to compute statistics
    prepped_rec <- recipes::prep(rec_obj, training = train)

    # Example of accessing the mean and sd for the target variable
    means <- prepped_rec$steps[[4]]$means[[.target]]
    sds <- prepped_rec$steps[[4]]$sds[[.target]]

    # Check performance
    test_pred <- stats::predict(
      best_model$fit$fit,
      new_data = recipes::bake(prepped_rec, new_data = test)
    )

    test_pred_unnormalized <- test_pred$.pred * sds + means

    df_test <- tibble::tibble(
      actual = test[[.target]],
      pred   = test_pred_unnormalized
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
      ggplot2::geom_smooth(col = "green", lty = 2, lwd = 1, method = "lm") +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::coord_fixed(ratio = 1) +
      ggpp::geom_table(
        data = inset_tbl,
        ggplot2::aes(x = x, y = y, label = tb)
      )

    auto_knn$test_plot <- g

  }

  auto_knn$model <- best_model

  return(auto_knn)

}


#' train_glmnet_model
#'
#' @param .data A data frame with `date` and numeric cols.
#' @param .target The name (string) of the target variable.
#' @param .prop The proportion of data to be retained for modeling/analysis.
#' @param .strat Logical, whether or not to conduct stratified sampling by
#' '.target'
#' @param .tune Default is TRUE, this will create a tuning grid and tuned
#' workflow.
#' @param .grid_size Default is 10.
#' @param .num_cores Default is 1.
#' @param .model_type Must be `classification.`
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
    .grid_size       = 10L,
    .num_cores       = 1L,
    .model_type      = "classification",
    .surrogate_model = FALSE
) {

  # Check if .data inherits from either "data.frame", "tbl" or 'tbl_df"
  if (!inherits(.data, c("data.frame", "tbl", "tbl_df"))) {
    stop(
      "Argument \".data\" must be a data.frame, tbl or tbl_df.",
      call. = FALSE
    )
  }

  # Check if .target is a string
  if (!is.character(.target)) {
    stop(
      "Argument \".target\" must be a string.",
      call. = FALSE
    )
  }

  # Check if .target is in .data
  if (!.target %in% colnames(.data)) {
    stop(
      "Argument \".target\" must be a column in \".data\".",
      call. = FALSE
    )
  }

  # Check if .prop is numeric
  if (!is.numeric(.prop)) {
    stop(
      "Argument \".prop\" must be numeric.",
      call. = FALSE
    )
  }

  # Check if .prop is between 0 and 1
  if (.prop < 0 || .prop > 1) {
    stop(
      "Argument \".prop\" must be between 0 and 1.",
      call. = FALSE
    )
  }

  # Check if .strat is logical
  if (!is.logical(.strat)) {
    stop(
      "Argument \".strat\" must be logical.",
      call. = FALSE
    )
  }

  # Check if .tune is logical
  if (!is.logical(.tune)) {
    stop(
      "Argument \".tune\" must be logical.",
      call. = FALSE
    )
  }

  # Check if .grid_size is integer
  if (!is.integer(.grid_size)) {
    stop(
      "Argument \".grid_size\" must be integer.",
      call. = FALSE
    )
  }

  # Check if .num_cores is integer
  if (!is.integer(.num_cores)) {
    stop(
      "Argument \".num_cores\" must be integer.",
      call. = FALSE
    )
  }

  # Check if .model_type is either "regression" or "classification"
  if (.model_type != "classification") {
    stop(
      "Argument \".model_type\" must be \"classification\".",
      call. = FALSE
    )
  }

  # Check if .surrogate_model is logical
  if (!is.logical(.surrogate_model)) {
    stop(
      "Argument \".surrogate_model\" must be logical.",
      call. = FALSE
    )
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

  rec_obj <- healthyR.ai::hai_glmnet_data_prepper(train, f)

  best_metric <- "accuracy"

  auto_glmnet <- healthyR.ai::hai_auto_glmnet(
    .data        = train,
    .rec_obj     = rec_obj,
    .tune        = .tune,
    .grid_size   = .grid_size,
    .num_cores   = .num_cores,
    .model_type  = .model_type,
    .best_metric = best_metric
  )

  auto_glmnet$model <- auto_glmnet$model_info$fitted_wflw

  return(auto_glmnet)

}


#' train_c50_model
#'
#' @param .data A data frame with `date` and numeric cols.
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
    .grid_size       = 10L,
    .num_cores       = 1L,
    .model_type      = "classification",
    .surrogate_model = FALSE
) {

  # Check if .data inherits from either "data.frame", "tbl" or 'tbl_df"
  if (!inherits(.data, c("data.frame", "tbl", "tbl_df"))) {
    stop(
      "Argument \".data\" must be a data.frame, tbl or tbl_df.",
      call. = FALSE
    )
  }

  # Check if .target is a string
  if (!is.character(.target)) {
    stop(
      "Argument \".target\" must be a string.",
      call. = FALSE
    )
  }

  # Check if .target is in .data
  if (!.target %in% colnames(.data)) {
    stop(
      "Argument \".target\" must be a column in \".data\".",
      call. = FALSE
    )
  }

  # Check if .prop is numeric
  if (!is.numeric(.prop)) {
    stop(
      "Argument \".prop\" must be numeric.",
      call. = FALSE
    )
  }

  # Check if .prop is between 0 and 1
  if (.prop < 0 || .prop > 1) {
    stop(
      "Argument \".prop\" must be between 0 and 1.",
      call. = FALSE
    )
  }

  # Check if .strat is logical
  if (!is.logical(.strat)) {
    stop(
      "Argument \".strat\" must be logical.",
      call. = FALSE
    )
  }

  # Check if .tune is logical
  if (!is.logical(.tune)) {
    stop(
      "Argument \".tune\" must be logical.",
      call. = FALSE
    )
  }

  # Check if .grid_size is integer
  if (!is.integer(.grid_size)) {
    stop(
      "Argument \".grid_size\" must be integer.",
      call. = FALSE
    )
  }

  # Check if .num_cores is integer
  if (!is.integer(.num_cores)) {
    stop(
      "Argument \".num_cores\" must be integer.",
      call. = FALSE
    )
  }

  # Check if .model_type is either "regression" or "classification"
  if (!.model_type %in% c("regression", "classification")) {
    stop(
      "Argument \".model_type\" must be either \"regression\" or \"classification\".", # nolint: line_length_linter.
      call. = FALSE
    )
  }

  # Check if .surrogate_model is logical
  if (!is.logical(.surrogate_model)) {
    stop(
      "Argument \".surrogate_model\" must be logical.",
      call. = FALSE
    )
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

  rec_obj <- healthyR.ai::hai_c50_data_prepper(train, f)

  best_metric <- switch(
    .model_type,
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

  if (.model_type == "regression") {

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
      ggplot2::geom_smooth(col = "green", lty = 2, lwd = 1, method = "lm") +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::coord_fixed(ratio = 1) +
      ggpp::geom_table(
        data = inset_tbl,
        ggplot2::aes(x = x, y = y, label = tb)
      )

    auto_c50$test_plot <- g

  }

  auto_c50$model <- best_model

  return(auto_c50)

}


#' train_svm_rbf_model
#'
#' @param .data A data frame with `date` and numeric cols.
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
train_svm_rbf_model <- function(
    .data,
    .target,
    .prop            = 0.8,
    .strat           = FALSE,
    .tune            = TRUE,
    .grid_size       = 10L,
    .num_cores       = 1L,
    .model_type      = "regression",
    .surrogate_model = FALSE
) {

  # Check if .data inherits from either "data.frame", "tbl" or 'tbl_df"
  if (!inherits(.data, c("data.frame", "tbl", "tbl_df"))) {
    stop(
      "Argument \".data\" must be a data.frame, tbl or tbl_df.",
      call. = FALSE
    )
  }

  # Check if .target is a string
  if (!is.character(.target)) {
    stop(
      "Argument \".target\" must be a string.",
      call. = FALSE
    )
  }

  # Check if .target is in .data
  if (!.target %in% colnames(.data)) {
    stop(
      "Argument \".target\" must be a column in \".data\".",
      call. = FALSE
    )
  }

  # Check if .prop is numeric
  if (!is.numeric(.prop)) {
    stop(
      "Argument \".prop\" must be numeric.",
      call. = FALSE
    )
  }

  # Check if .prop is between 0 and 1
  if (.prop < 0 || .prop > 1) {
    stop(
      "Argument \".prop\" must be between 0 and 1.",
      call. = FALSE
    )
  }

  # Check if .strat is logical
  if (!is.logical(.strat)) {
    stop(
      "Argument \".strat\" must be logical.",
      call. = FALSE
    )
  }

  # Check if .tune is logical
  if (!is.logical(.tune)) {
    stop(
      "Argument \".tune\" must be logical.",
      call. = FALSE
    )
  }

  # Check if .grid_size is integer
  if (!is.integer(.grid_size)) {
    stop(
      "Argument \".grid_size\" must be integer.",
      call. = FALSE
    )
  }

  # Check if .num_cores is integer
  if (!is.integer(.num_cores)) {
    stop(
      "Argument \".num_cores\" must be integer.",
      call. = FALSE
    )
  }

  # Check if .model_type is either "regression" or "classification"
  if (!.model_type %in% c("regression", "classification")) {
    stop(
      "Argument \".model_type\" must be either \"regression\" or \"classification\".", # nolint: line_length_linter.
      call. = FALSE
    )
  }

  # Check if .surrogate_model is logical
  if (!is.logical(.surrogate_model)) {
    stop(
      "Argument \".surrogate_model\" must be logical.",
      call. = FALSE
    )
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

  rec_obj <- healthyR.ai::hai_svm_rbf_data_prepper(train, f)

  best_metric <- switch(
    .model_type,
    "regression"     = "rmse",
    "classification" = "accuracy"
  )

  auto_svm <- healthyR.ai::hai_auto_svm_rbf(
    .data        = train,
    .rec_obj     = rec_obj,
    .tune        = .tune,
    .grid_size   = .grid_size,
    .num_cores   = .num_cores,
    .model_type  = .model_type,
    .best_metric = best_metric
  )

  best_model <- auto_svm$model_info$fitted_wflw

  if (.model_type == "regression") {

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
      ggplot2::geom_smooth(col = "green", lty = 2, lwd = 1, method = "lm") +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::coord_fixed(ratio = 1) +
      ggpp::geom_table(
        data = inset_tbl,
        ggplot2::aes(x = x, y = y, label = tb)
      )

    auto_svm$test_plot <- g

  }

  auto_svm$model <- best_model

  return(auto_svm)

}


#' train_svm_poly_model
#'
#' @param .data A data frame with `date` and numeric cols.
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
train_svm_poly_model <- function(
    .data,
    .target,
    .prop            = 0.8,
    .strat           = FALSE,
    .tune            = TRUE,
    .grid_size       = 10L,
    .num_cores       = 1L,
    .model_type      = "regression",
    .surrogate_model = FALSE
) {

  # Check if .data inherits from either "data.frame", "tbl" or 'tbl_df"
  if (!inherits(.data, c("data.frame", "tbl", "tbl_df"))) {
    stop(
      "Argument \".data\" must be a data.frame, tbl or tbl_df.",
      call. = FALSE
    )
  }

  # Check if .target is a string
  if (!is.character(.target)) {
    stop(
      "Argument \".target\" must be a string.",
      call. = FALSE
    )
  }

  # Check if .target is in .data
  if (!.target %in% colnames(.data)) {
    stop(
      "Argument \".target\" must be a column in \".data\".",
      call. = FALSE
    )
  }

  # Check if .prop is numeric
  if (!is.numeric(.prop)) {
    stop(
      "Argument \".prop\" must be numeric.",
      call. = FALSE
    )
  }

  # Check if .prop is between 0 and 1
  if (.prop < 0 || .prop > 1) {
    stop(
      "Argument \".prop\" must be between 0 and 1.",
      call. = FALSE
    )
  }

  # Check if .strat is logical
  if (!is.logical(.strat)) {
    stop(
      "Argument \".strat\" must be logical.",
      call. = FALSE
    )
  }

  # Check if .tune is logical
  if (!is.logical(.tune)) {
    stop(
      "Argument \".tune\" must be logical.",
      call. = FALSE
    )
  }

  # Check if .grid_size is integer
  if (!is.integer(.grid_size)) {
    stop(
      "Argument \".grid_size\" must be integer.",
      call. = FALSE
    )
  }

  # Check if .num_cores is integer
  if (!is.integer(.num_cores)) {
    stop(
      "Argument \".num_cores\" must be integer.",
      call. = FALSE
    )
  }

  # Check if .model_type is either "regression" or "classification"
  if (!.model_type %in% c("regression", "classification")) {
    stop(
      "Argument \".model_type\" must be either \"regression\" or \"classification\".", # nolint: line_length_linter.
      call. = FALSE
    )
  }

  # Check if .surrogate_model is logical
  if (!is.logical(.surrogate_model)) {
    stop(
      "Argument \".surrogate_model\" must be logical.",
      call. = FALSE
    )
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

  rec_obj <- healthyR.ai::hai_svm_poly_data_prepper(train, f)

  best_metric <- switch(
    .model_type,
    "regression"     = "rmse",
    "classification" = "accuracy"
  )

  auto_svm <- healthyR.ai::hai_auto_svm_poly(
    .data        = train,
    .rec_obj     = rec_obj,
    .tune        = .tune,
    .grid_size   = .grid_size,
    .num_cores   = .num_cores,
    .model_type  = .model_type,
    .best_metric = best_metric
  )

  best_model <- auto_svm$model_info$fitted_wflw

  if (.model_type == "regression") {

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
      ggplot2::geom_smooth(col = "green", lty = 2, lwd = 1, method = "lm") +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::coord_fixed(ratio = 1) +
      ggpp::geom_table(
        data = inset_tbl,
        ggplot2::aes(x = x, y = y, label = tb)
      )

    auto_svm$test_plot <- g

  }

  auto_svm$model <- best_model

  return(auto_svm)

}


#' coeteris_paribus
#'
#' @param .model A fitted model from one of the `nexaverser::train` functions.
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

  model <- .model$model

  # Check if .model is a valid model
  if (!is(model, "workflow")) {
    stop(
      "Argument \".model\" must contain a \"workflow\".",
      call. = FALSE
    )
  }

  # Check if .newdata is a data frame or a tibble
  if (!inherits(.newdata, c("data.frame", "tbl", "tbl_df"))) {
    stop(
      "Argument \".newdata\" must be a data frame or a tibble.",
      call. = FALSE
    )
  }

  # Check if .target is a character
  if (!is.character(.target)) {
    stop(
      "Argument \".target\" must be a character.",
      call. = FALSE
    )
  }

  # create custom predict function
  pred <- function(model, newdata) {
    results <- stats::predict(model, newdata) |> dplyr::pull(.pred)
    return(results)
  }

  # create an explainer for the model
  x <- setdiff(names(.newdata), .target)
  y <- .target

  explainer <- DALEX::explain(
    model            = model,
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


# custom mean
my_mean <- function(x, na.rm = TRUE) {
  mean(x, na.rm = na.rm)
}


#' optimize_with_jaya
#'
#' @param .model A fitted model from one of the `nexaverser::train` functions.
#' @param .vars A character vector with the names of the input variables.
#' @param .lower A vector of lower bounds for the vaiables in the function.
#' @param .upper A vector of upper bounds for the vaiables in the function.
#' @param .maxiter The number of iterations to run for finding a solution.
#' @param .option A string, either "maximize" or "minimize" the function.
#' @param .seed An integer vector containing the random number generator state.
#'
#' @return A `tibble`.
#' @export
#'
optimize_with_jaya <- function(
    .model,
    .vars,
    .lower,
    .upper,
    .maxiter = 10L,
    .option  = "minimize",
    .seed    = NULL
) {

  model <- .model$model

  # Check if .model is a valid model
  if (!is(model, "workflow")) {
    stop(
      "Argument \".model\" must contain a \"workflow\".",
      call. = FALSE
    )
  }

  # Check if .vars is a character vector
  if (!is.character(.vars)) {
    stop("The variables must be a character vector.")
  }

  # Check if .lower is a numeric vector whith the same length as .vars
  if (!is.numeric(.lower) || length(.lower) != length(.vars)) {
    stop(
      stringr::str_glue("The lower bounds must be a numeric vector with length equal to ({length(.vars)}).") # nolint: line_length_linter.
    )
  }

  # Check if .upper is a numeric vector whith the same length as .vars
  if (!is.numeric(.upper) || length(.upper) != length(.vars)) {
    stop(
      stringr::str_glue("The upper bounds must be a numeric vector with length equal to ({length(.vars)}).") # nolint: line_length_linter.
    )
  }

  # Check if .maxiter is an integer
  if (!is.integer(.maxiter)) {
    stop("The maximum number of iterations must be an integer.")
  }

  # Check if .option is a string
  if (!is.character(.option)) {
    stop("The option must be a string.")
  }

  # Check if .option is either "maximize" or "minimize"
  if (!.option %in% c("maximize", "minimize")) {
    stop("The option must be either 'maximize' or 'minimize'.")
  }

  # Check if .seed is an integer vector
  if (!is.null(.seed)) {
    if (!is.integer(.seed)) {
      stop("The seed must be an integer vector.")
    }
  }

  ## create custom predict function
  pred <- function(x) {

    newdata <- t(x) |>
      as.data.frame() |>
      purrr::set_names(.vars)

    results <- stats::predict(model, newdata) |> dplyr::pull(.pred)

    return(results)

  }

  ## solve problem
  S <- Jaya::jaya(fun = pred, lower = .lower, upper = .upper,
                  maxiter = .maxiter, n_var = length(.vars), opt = .option,
                  seed = .seed)

  names(S$best) <- c(.vars, "f(x)")

  return(S$best |> t() |> tibble::enframe())

}


#' optimize_with_cobyla
#'
#' @param .model A fitted model from one of the `nexaverser::train` functions.
#' @param .vars A character vector with the names of the input variables.
#' @param .lower A vector of lower bounds for the vaiables in the function.
#' @param .upper A vector of upper bounds for the vaiables in the function.
#' @param .x0 The starting point for searching the optimum.
#' @param .maxiter The number of iterations to run for finding a solution.
#' @param .option A string, either "maximize" or "minimize" the function.
#'
#' @return A `tibble`.
#' @export
#'
optimize_with_cobyla <- function(
    .model,
    .vars,
    .lower,
    .upper,
    .x0,
    .maxiter = 10L,
    .option  = "minimize"
) {

  model <- .model$model

  # Check if .model is a valid model
  if (!is(model, "workflow")) {
    stop(
      "Argument \".model\" must contain a \"workflow\".",
      call. = FALSE
    )
  }

  # Check if .vars is a character vector
  if (!is.character(.vars)) {
    stop("The variables must be a character vector.")
  }

  # Check if .lower is a numeric vector whith the same length as .vars
  if (!is.numeric(.lower) || length(.lower) != length(.vars)) {
    stop(
      stringr::str_glue("The lower bounds must be a numeric vector with length equal to ({length(.vars)}).") # nolint: line_length_linter.
    )
  }

  # Check if .upper is a numeric vector whith the same length as .vars
  if (!is.numeric(.upper) || length(.upper) != length(.vars)) {
    stop(
      stringr::str_glue("The upper bounds must be a numeric vector with length equal to ({length(.vars)}).") # nolint: line_length_linter.
    )
  }

  # Check if .x0 is a numeric vector whith the same length as .vars
  if (!is.numeric(.x0) || length(.x0) != length(.vars)) {
    stop(
      stringr::str_glue("The starting point must be a numeric vector with length equal to ({length(.vars)}).") # nolint: line_length_linter.
    )
  }

  # Check if .maxiter is an integer
  if (!is.integer(.maxiter)) {
    stop("The maximum number of iterations must be an integer.")
  }

  # Check if .option is a string
  if (!is.character(.option)) {
    stop("The option must be a string.")
  }

  # Check if .option is either "maximize" or "minimize"
  if (!.option %in% c("maximize", "minimize")) {
    stop("The option must be either 'maximize' or 'minimize'.")
  }

  ## create custom predict function
  pred <- function(x) {

    newdata <- t(x) |>
      as.data.frame() |>
      purrr::set_names(.vars)

    results <- stats::predict(model, newdata) |> dplyr::pull(.pred)

    mult <- switch(
      .option,
      "minimize" = 1,
      "minimise" = 1,
      "min"      = 1,
      "Min"      = 1,
      -1
    )

    return(mult * results)

  }

  ## solve problem
  S <- nloptr::cobyla(x0 = .x0, fn = pred, lower = .lower, upper = .upper,
                      control=list(maxeval = .maxiter))

  sol <- c(S$par, ifelse(S$value < 0, -S$value, S$value))

  names(sol) <- c(.vars, "f(x)")

  return(sol |> tibble::enframe())

}


#' optimize_with_spaceballs_princess
#'
#' @param .model A fitted model from one of the `nexaverser::train` functions.
#' @param .vars A character vector with the names of the input variables.
#' @param .lower A vector of lower bounds for the vaiables in the function.
#' @param .upper A vector of upper bounds for the vaiables in the function.
#' @param .eps A convergence control parameter: if the maximum st.dev. of the
#' parameters of the elite individuals divided by its average value is smaller
#' than this number, the method considers that it converged.
#' @param .maxiter The number of iterations to run for finding a solution.
#' @param .option A string, either "maximize" or "minimize" the function.
#' @param .use_all_cores A flag to indicate if the user wants to use all the
#' cores fromm the compute instance to run the fitness functions.
#'
#' @return A `tibble`.
#' @export
#'
optimize_with_spaceballs_princess <- function(
    .model,
    .vars,
    .lower,
    .upper,
    .eps           = 0.3,
    .maxiter       = 10L,
    .option        = "minimize",
    .use_all_cores = FALSE
) {

  model <- .model$model

  # Check if .model is a valid model
  if (!is(model, "workflow")) {
    stop(
      "Argument \".model\" must contain a \"workflow\".",
      call. = FALSE
    )
  }

  # Check if .vars is a character vector
  if (!is.character(.vars)) {
    stop("The variables must be a character vector.")
  }

  # Check if .lower is a numeric vector whith the same length as .vars
  if (!is.numeric(.lower) || length(.lower) != length(.vars)) {
    stop(
      stringr::str_glue("The lower bounds must be a numeric vector with length equal to ({length(.vars)}).") # nolint: line_length_linter.
    )
  }

  # Check if .upper is a numeric vector whith the same length as .vars
  if (!is.numeric(.upper) || length(.upper) != length(.vars)) {
    stop(
      stringr::str_glue("The upper bounds must be a numeric vector with length equal to ({length(.vars)}).") # nolint: line_length_linter.
    )
  }

  # Check if .eps is numeric
  if (!is.numeric(.eps)) {
    stop("The convergence control parameter must be numeric.")
  }

  # Check if .maxiter is an integer
  if (!is.integer(.maxiter)) {
    stop("The maximum number of iterations must be an integer.")
  }

  # Check if .option is a string
  if (!is.character(.option)) {
    stop("The option must be a string.")
  }

  # Check if .option is either "maximize" or "minimize"
  if (!.option %in% c("maximize", "minimize")) {
    stop("The option must be either 'maximize' or 'minimize'.")
  }

  #Check if .use_all_cores is a logical
  if (!is.logical(.use_all_cores)) {
    stop("The use_all_cores flag must be a logical.")
  }

  ## create custom predict function
  pred <- function(x) {

    newdata <- t(x) |>
      as.data.frame() |>
      purrr::set_names(.vars)

    results <- stats::predict(model, newdata) |> dplyr::pull(.pred)

    return(results)

  }

  minimize <- switch(
    .option,
    "minimize" = TRUE,
    "minimise" = TRUE,
    "min"      = TRUE,
    "Min"      = TRUE,
    FALSE
  )

  ## solve problem
  po <- RCEIM::ceimOpt(OptimFunction = "pred", maxIter = .maxiter,
                       epsilon = .eps, nParams = length(.vars), verbose = FALSE,
                       boundaries = cbind(.lower, .upper), minimize = minimize,
                       parallelVersion = .use_all_cores)

  names(po$BestMember) <- c(.vars, "f(x)")
  po_tbl <- po$BestMember |> tibble::enframe()

  return(po_tbl)

}
