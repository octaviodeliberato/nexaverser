# DATA --------------------------------------------------------------------

tag_tbl <- readxl::read_xlsx(path = "data-raw/BANCO DADOS - RTO.xlsx",
                             sheet = 1, skip = 5)
tag_tbl <- tag_tbl |> janitor::clean_names()
tag_tbl[[1]] <- NULL
tag_tbl <- tag_tbl |> dplyr::rename(date = data_final)
tag_tbl <- tag_tbl |> dplyr::relocate(tq_10, .after = last_col())
names(tag_tbl) |>
  stringr::str_detect(string = _, pattern = "^teor_[a-z]{1}f", negate = TRUE) |>
  which() -> .

tag_tbl <- tag_tbl[, .]

tag_tbl |> nexaverser::plot_tag_data()

# from visual inspection
tag_imp <- tag_tbl |>
  timetk::filter_by_time(
    .date_var   = date,
    .start_date = "2021-07-01",
    .end_date   = "2022-01-31"
  )

tag_imp |> nexaverser::plot_tag_data()

y <- "tq_10"

vars_to_remove_anomalies <- c(
  "nivel_rg_i_c",
  "nivel_rg_ii_c",
  "nivel_rg_iii_c",
  "nivel_cl_i_c",
  "nivel_cl_ii_c",
  "nivel_cl_iii_c",
  "rougher_i_w",
  "rougher_ii_w",
  "rougher_iii_w",
  "vazao_bulk",
  "vazao_c",
  "vazao_w",
  "vazao_cfw",
  "vazao_cfc",
  "vazao_cfbulk",
  "percent_solidos_cfw",
  "percent_solidos_cfc",
  "percent_solidos_cfbulk",
  "p_h_rg1_c"
)

bad_dates <- list()

for (i in seq_along(vars_to_remove_anomalies)) {

  tag_df <- tag_imp |>
    dplyr::select(all_of(c("date", vars_to_remove_anomalies[i])))

  names(tag_df) <- c("date", "value")

  anom_dates <- tag_df |>
    dplyr::mutate(value =  timetk::ts_impute_vec(value)) |>
    timetk::tk_anomaly_diagnostics(.date_var = date, .value = value) |>
    dplyr::filter(anomaly == "Yes") |>
    dplyr::pull(date)

  bad_dates[[i]] <- anom_dates

}

bad_dates <- bad_dates |>
  do.call(args = _, rlist::list.append) |>
  unique()

tag_imp[tag_imp$date %in% bad_dates, vars_to_remove_anomalies] <- NA

# impute missings
tag_imp <- tag_imp |> nexaverser::impute_missing_values()

tag_imp |> nexaverser::plot_tag_data()


# TOOLS -------------------------------------------------------------------

nexaverser::fz_data |> nexaverser::plot_tag_data()

ds <- nexaverser::fz_data[, 1:2]

ds |> utils::tail(60) |> nexaverser::forecast_tag()

at <- nexaverser::assess_tag(ds |> utils::tail(120), .imp = TRUE,
                             .clean = TRUE)

at$trend_data |> dplyr::select(date, value) |> utils::tail(60) |>
  nexaverser::forecast_tag()


# FEATURE SELECTION -------------------------------------------------------

# * Feature Selection 1 ----
sel_features_1 <- nexaverser::select_features_with_boruta(
  .tag_dat        = tag_imp,
  .target         = y,
  .balance        = FALSE,
  .with_tentative = FALSE,
  .return_data    = FALSE,
  .task           = "regression"
)

# * Feature Selection 2 ----
sel_features_2 <- nexaverser::select_features_with_trex(
  .tag_dat        = tag_imp,
  .target         = y,
  .balance        = FALSE,
  .return_data    = FALSE,
  .task           = "regression"
)

# * Feature Selection 3 ----
sel_features_3 <- nexaverser::select_features_with_pps(
  .tag_dat     = tag_imp,
  .target      = y,
  .balance     = FALSE,
  .return_data = FALSE,
  .task        = "regression"
)

# * Feature Selection 4 ----
sel_features_4 <- nexaverser::run_causation_analysis(
  .tag_dat = tag_imp |>
    dplyr::slice_tail(n = 365*6/2),
  .target  = y,
  .max_lag = 6
)

selected_features <- sel_features_2$selected_features

df <- tag_imp |>
  dplyr::select(dplyr::all_of(c(selected_features, y)))

f <- as.formula(stringr::str_glue("{y} ~ ."))

rec <- recipes::recipe(formula = f, data = df)

corr_filter <- rec |>
  recipes::step_corr(recipes::all_numeric_predictors(), threshold = .7)

selected_features <- corr_filter |>
  recipes::prep() |>
  recipes::juice() |>
  names() |>
  setdiff(y = y)

df <- tag_imp |>
  dplyr::select(dplyr::all_of(c(selected_features, y)))


# MODELING 1 --------------------------------------------------------------

tictoc::tic()
cubist_model <- nexaverser::train_cubist_model(
  .data            = df,
  .target          = y,
  .strat           = FALSE,
  .tune            = FALSE,
  .surrogate_model = FALSE
)
tictoc::toc()

cubist_vip <- cubist_model$model$fit$fit$fit |> vip::vip()

# 2nd round
xvar <- cubist_vip$data$Variable[1]

yvar <- cubist_vip$data$Variable[2]

tictoc::tic()
cubist_model_2d <- nexaverser::train_cubist_model(
  .data            = df[, c(xvar, yvar, y)],
  .target          = y,
  .strat           = FALSE,
  .tune            = FALSE,
  .surrogate_model = FALSE
)
tictoc::toc()

cubist_model_2d$model$fit$fit$fit |> vip::vip()

# 3nd round
vars <- cubist_vip$data$Variable[1:4]

tictoc::tic()
cubist_model_4d <- nexaverser::train_cubist_model(
  .data            = df[, c(vars, y)],
  .target          = y,
  .strat           = FALSE,
  .tune            = FALSE,
  .surrogate_model = FALSE
)
tictoc::toc()

cubist_model_4d$model$fit$fit$fit |> vip::vip()


# MODELING 2 --------------------------------------------------------------

# 1st round
tictoc::tic()
xgb_model <- nexaverser::train_xgboost_model(
  .data            = df,
  .target          = y,
  .strat           = FALSE,
  .tune            = FALSE,
  .surrogate_model = FALSE
)
tictoc::toc()

xgb_vip <- xgb_model$model$fit$fit$fit |> vip::vip()

# 2nd round

tictoc::tic()
xgb_model_2 <- nexaverser::train_xgboost_model(
  .data            = df,
  .target          = y,
  .strat           = FALSE,
  .tune            = TRUE,
  .surrogate_model = FALSE,
  .num_cores       = parallelly::availableCores() - 1
)
tictoc::toc()

xgb_model_2$model$fit$fit$fit |> vip::vip()

# 3rd round

tictoc::tic()
xgb_model_3 <- nexaverser::train_xgboost_model(
  .data            = df,
  .target          = y,
  .strat           = TRUE,
  .tune            = TRUE,
  .surrogate_model = FALSE,
  .num_cores       = parallelly::availableCores() - 1
)
tictoc::toc()

xgb_model_3$model$fit$fit$fit |> vip::vip()


# MODELING 3 --------------------------------------------------------------

tictoc::tic()
mars_model <- nexaverser::train_mars_model(
  .data            = df,
  .target          = y,
  .strat           = FALSE,
  .tune            = FALSE,
  .surrogate_model = FALSE
)
tictoc::toc()

mars_vip <- mars_model$model$fit$fit$fit |> vip::vip()


# MODELING 4 --------------------------------------------------------------

tictoc::tic()
rf_model <- nexaverser::train_ranger_model(
  .data            = df,
  .target          = y,
  .strat           = FALSE,
  .tune            = FALSE,
  .surrogate_model = FALSE
)
tictoc::toc()

rf_vip <- rf_model$model$fit$fit$fit |> vip::vip()

# 2nd round
vars <- rf_vip$data$Variable[1:3]

tictoc::tic()
rf_model_3d <- nexaverser::train_ranger_model(
  .data            = df[, c(vars, y)],
  .target          = y,
  .strat           = FALSE,
  .tune            = TRUE,
  .surrogate_model = FALSE
)
tictoc::toc()

rf_model_3d$model$fit$fit$fit |> vip::vip()


# MODELING 5 --------------------------------------------------------------

data <- df

# Splits
set.seed(1)
splits <- rsample::initial_split(data, prop = 0.8, strata = all_of(y))
train  <- rsample::training(splits)
test   <- rsample::testing(splits)

# Start H2O Cluster
h2o::h2o.init()

train <- h2o::as.h2o(train)
test  <- h2o::as.h2o(test)

# Take a look at the training set
h2o::h2o.describe(train)

# Identify the predictor columns (remove response and ID column)
x <- setdiff(names(train), y)

n_mod <- 10
# Execute an AutoML run for n_mod models
aml <- h2o::h2o.automl(
  y                                 = y,
  x                                 = x,
  training_frame                    = train,
  project_name                      = "nexaverser",
  max_models                        = n_mod,
  seed                              = 1,
  exclude_algos                     = c("DeepLearning"),
  keep_cross_validation_predictions = TRUE
)

# The leader model is stored at `aml@leader` and the leaderboard is stored at `aml@leaderboard`.
lb <- aml@leaderboard

# Get model ids for all models in the AutoML Leaderboard
model_ids <- as.data.frame(aml@leaderboard$model_id)[, 1]
# Pick a model
best <- h2o::h2o.getModel(model_ids[3])

# Variable importance
h2o::h2o.varimp_plot(best)

# Save for later
h2o::h2o.saveModel(object = best, path = "data-raw", force = TRUE,
                   filename = "best-model")

best <- h2o::h2o.loadModel(path = "data-raw/best-model")

# Check performance
test_pred <- predict(best, newdata = test) |>
  tibble::as_tibble()

df_test <- tibble::tibble(
  actual = test |> tibble::as_tibble() |> dplyr::pull(y),
  pred   = test_pred$predict
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

ggplot2::ggplot(
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

h2o::h2o.shutdown(prompt = FALSE)


# MODEL ANALYSIS ----------------------------------------------------------

model   <- rf_model$model # global var to pred
var_imp <- rf_vip


# * CETERIS PARIBUS -------------------------------------------------------

xvar <- var_imp$data$Variable[1]

yvar <- var_imp$data$Variable[2]

cp_plt <- nexaverser::coeteris_paribus(
  .model   = model,
  .newdata = df,
  .target  = y
)

cp_plt[[xvar]]
cp_plt[[yvar]]


# * PLANT PERFORMANCE MAPS ------------------------------------------------

xvar <- var_imp$data$Variable[1]

yvar <- var_imp$data$Variable[2]

zvar <- y

res  <- 16 # 3d plots resolution

ppm <- nexaverser::plant_performance_map(
  .model = model,
  .data  = df,
  .xvar  = xvar,
  .yvar  = yvar,
  .zvar  = zvar,
  .res   = res
)

ppm


# * OPTIMIZATION ----------------------------------------------------------

# ** Setup ----

n <- 16
## to define a grid
x <- seq(min(df[[xvar]]), max(df[[xvar]]), length.out = n)
y <- seq(min(df[[yvar]]), max(df[[yvar]]), length.out = n)
## create custom predict function
pred <- function(x) {

  newdata <- t(x) |>
    as.data.frame() |>
    purrr::set_names(xvar, yvar)

  results <- stats::predict(model, newdata) |> dplyr::pull(.pred)

  return(results)

}

pred(c(x[1], y[1]))
## evaluate on each grid point
xy <- expand.grid(x, y)

library(future.apply)
plan(multisession)

tictoc::tic()
z <- future.apply::future_apply(
  X        = xy,
  MARGIN   = 1,
  FUN      = pred,
  simplify = TRUE
)
tictoc::toc()

plan(sequential)

z

# ** General Purpose Methods ----

## wrapper for all methods of optim
optims <- function(x, x0, meth = "Nelder-Mead", lb = -Inf, ub = Inf) {
  sol <- matrix(ncol = 3, nrow = 21)
  sol[1, ] <- c(x0, pred(x0))
  for (i in 2:20) {
    S <- optim(par = x0, pred, method = meth,
               lower = lb, upper = ub,
               control = list(maxit = i))
    sol[i, ] <- c(S$par, S$value)
  }
  S <- optim(par = x0, pred, method = meth,
             lower = lb, upper = ub,
             control = list(maxit = 100))
  sol[21, ] <- c(S$par, S$value)
  points(x0[1], x0[2], pch = 20, cex = 2)
  points(sol[21, 1], sol[21, 2], pch = 20, col = "red", cex = 3)
  lines(sol[, 1], sol[, 2], type = "o", pch = 3)
  return(sol)
}

## plot lines for all methods
lower <- apply(xy, 2, min)
upper <- apply(xy, 2, max)
par(mar = c(4, 4, 0.5, 0.5))
contour(x, y,  matrix(z, length(x)), xlab = "x", ylab = "y", nlevels = 20)
xo <- c(36, 25) # starting point
optims(x0 = xo)  # Nelder-Mead
optims("L-BFGS-B", x0 = xo, lb = lower, ub = upper)
optims("SANN", x0 = xo)
optims("Brent", x0 = xo, lb = lower, ub = upper)

# ** COBYLA ----
library(nloptr)

## wrapper for COBYLA
cobylas <- function(x, x0, lb = -Inf, ub = Inf) {
  sol <- matrix(ncol = 3, nrow = 21)
  sol[1, ] <- c(x0, pred(x0))
  for (i in 2:20) {
    S <- cobyla(x0 = x0, fn = pred, lower = lb, upper = ub,
                control=list(maxeval = i))
    sol[i, ] <- c(S$par, S$value)
  }
  S <- cobyla(x0 = x0, fn = pred, lower = lb, upper = ub,
              control=list(maxeval = 21))
  sol[21, ] <- c(S$par, S$value)
  points(x0[1], x0[2], pch = 20, cex = 2)
  points(sol[21, 1], sol[21, 2], pch = 20, col = "red", cex = 3)
  lines(sol[, 1], sol[, 2], type = "o", pch = 3)
  return(sol)
}

## plot lines for all methods
lower <- apply(xy, 2, min)
upper <- apply(xy, 2, max)
par(mar = c(4, 4, 0.5, 0.5))
contour(x, y,  matrix(z, length(x)), xlab = "x", ylab = "y", nlevels = 20)
xo <- c(700.0, 3.6) # starting point
sol_cobyla <- cobylas(x0 = xo, lb = lower, ub = upper)
sol_cobyla
xo <- c(1200.0, 3.7) # starting point
sol_cobyla <- cobylas(x0 = xo, lb = lower, ub = upper)
sol_cobyla

# ** Jaya ----
library(Jaya)

## wrapper for Jaya
victory <- function(x, lb = -Inf, ub = Inf, opt = "minimize") {
  sol <- matrix(ncol = 3, nrow = 21)
  for (i in 1:20) {
    sol[i, ] <- jaya(fun = pred, lower = lb, upper = ub, maxiter = i,
                     n_var = 2, opt = opt)$best |> as.matrix()
  }
  sol[21, ] <- jaya(fun = pred, lower = lb, upper = ub, maxiter = 21,
                    n_var = 2, opt = opt)$best |> as.matrix()
  points(sol[1, 1], sol[1, 2], pch = 20, cex = 2)
  points(sol[21, 1], sol[21, 2], pch = 20, col = "red", cex = 3)
  lines(sol[, 1], sol[, 2], type = "o", pch = 3)
  return(sol)
}

## plot lines for all methods
lower <- apply(xy, 2, min)
upper <- apply(xy, 2, max)
par(mar = c(4, 4, 0.5, 0.5))
contour(x, y,  matrix(z, length(x)), xlab = "x", ylab = "y", nlevels = 20)
sol_jaya <- victory(lb = lower, ub = upper, opt = "maximize")
sol_jaya

# ** Jaya, more complex problem ----

vars  <- sel_features$selected_features

model   <- rf_model$model # global var to pred
lower <- apply(df |> dplyr::select(all_of(vars)), 2, min)
upper <- apply(df |> dplyr::select(all_of(vars)), 2, max)

sol <- nexaverser::optimize_with_jaya(
  .model   = model,
  .vars    = vars,
  .lower   = lower,
  .upper   = upper,
  .maxiter = 5,
  .option  = "minimize"
)

sol

# ** CEIM ----

po <- nexaverser::optimize_with_spaceballs_princess(
  .model         = model,
  .vars          = vars,
  .lower         = lower,
  .upper         = upper,
  .eps           = 0.3,
  .maxiter       = 5,
  .option        = "minimize",
  .use_all_cores = FALSE
)

po

# ** COBYLA, once more ----

S <- nexaverser::optimize_with_cobyla(
  .model   = model,
  .vars    = vars,
  .lower   = lower,
  .upper   = upper,
  .x0      = sol$value[1:length(vars)],
  .maxiter = 100,
  .option  = "minimize"
)

S


# CLUSTERING --------------------------------------------------------------

clusters <- nexaverser::cluster_ts(tag_imp, 3)


# MORE FORECASTING --------------------------------------------------------

# * Data ----

data_prepared_tbl <- tag_imp |>
  dplyr::select(all_of(c("date", "teor_zn"))) |>
  purrr::set_names(c("date", "value")) |>
  dplyr::mutate(value = timetk::ts_clean_vec(value)) |>
  tibble::as_tibble()

# * Recipes ----

# rec_spec <- ts_auto_recipe(
#   .data                   = data_prepared_tbl,
#   .date_col               = date,
#   .pred_col               = value,
#   .step_ts_sig            = TRUE,
#   .step_ts_rm_misc        = TRUE,
#   .step_ts_dummy          = TRUE,
#   .step_ts_fourier        = TRUE,
#   .step_ts_fourier_period = 25,
#   .K                      = 1,
#   .step_ts_yeo            = FALSE,
#   .step_ts_nzv            = TRUE
# )
#
# rec_spec$rec_base |> recipes::prep() |> recipes::juice() |> dplyr::glimpse()
# rec_spec$rec_date |> recipes::prep() |> recipes::juice() |> dplyr::glimpse()

# * Time series splits ----

splits <- timetk::time_series_split(
  data       = data_prepared_tbl,
  date_var   = date,
  assess     = "4 hours",
  cumulative = TRUE
)

splits |>
  timetk::tk_time_series_cv_plan() |>
  timetk::plot_time_series_cv_plan(date, value)

# * Basic Prophet ----

prophet_fit <- nexaverser::forecast_prophet(
  .tag_dat = data_prepared_tbl,
  .assess  = "6 hours",
  .horiz   = 6
)

attributes(prophet_fit)[["plot"]]

# * NNETAR ----

nnetar_fit <- nexaverser::forecast_nnetar(
  .tag_dat         = data_prepared_tbl,
  .assess          = "15 days",
  .horiz           = 15,
  .seasonal_period = "auto"
)

attributes(nnetar_fit)[["plot"]]

# * TBATS Model ----

tbats_fit <- nexaverser::forecast_tbats(
  .tag_dat           = data_prepared_tbl,
  .assess            = "15 days",
  .horiz             = 15,
  .seasonal_period_1 = "auto"
)

attributes(tbats_fit)[["plot"]]

# * STLM ETS Model ----

stlm_ets_fit <- nexaverser::forecast_stlm(
  .tag_dat           = data_prepared_tbl,
  .assess            = "15 days",
  .horiz             = 15,
  .seasonal_period_1 = "auto",
  .algo              = "ets"
)

attributes(stlm_ets_fit)[["plot"]]

# * STLM ARIMA Model ----

stlm_arima_fit <- nexaverser::forecast_stlm(
  .tag_dat           = data_prepared_tbl,
  .assess            = "15 days",
  .horiz             = 15,
  .seasonal_period_1 = "auto",
  .algo              = "arima"
)

attributes(stlm_arima_fit)[["plot"]]


# EVALUATION --------------------------------------------------------------

# * Modeltime ----

model_tbl <- modeltime::modeltime_table(
  attributes(prophet_fit)[["model_fit"]],
  attributes(nnetar_fit)[["model_fit"]],
  attributes(tbats_fit)[["model_fit"]],
  attributes(stlm_ets_fit)[["model_fit"]],
  attributes(stlm_arima_fit)[["model_fit"]]
)

model_tbl |>
  modeltime::modeltime_residuals(rsample::testing(splits)) |>
  modeltime::plot_modeltime_residuals()

# * Calibration ----

calibration_tbl <- model_tbl |>
  modeltime::modeltime_calibrate(rsample::testing(splits))

# * Forecast Test ----

calibration_tbl |>
  modeltime::modeltime_forecast(
    new_data    = rsample::testing(splits),
    actual_data = data_prepared_tbl
  ) |>
  modeltime::plot_modeltime_forecast(
    .conf_interval_fill = "gray",
    .conf_interval_alpha = 0.1
  )

# * Accuracy Test ----

# calibration_tbl |> modeltime_accuracy()

calibration_tbl |>
  modeltime::modeltime_accuracy() |>
  modeltime::table_modeltime_accuracy(resizable = TRUE, bordered = TRUE)

# * Refit ----

refit_tbl <- calibration_tbl |>
  modeltime::modeltime_refit(data = data_prepared_tbl)

refit_tbl |>
  modeltime::modeltime_forecast(
    h = 24,
    actual_data = data_prepared_tbl
  ) |>
  modeltime::plot_modeltime_forecast(
    .conf_interval_show = FALSE
  )


# ENSEMBLE ----------------------------------------------------------------

library(modeltime.ensemble)

# * Mean ----
ensemble_fit_mean <- calibration_tbl %>%
  ensemble_average(type = "mean")

modeltime_table(
  ensemble_fit_mean
) %>%
  modeltime_accuracy(testing(splits))

# * Median ----
ensemble_fit_median <- calibration_tbl %>%
  ensemble_average("median")

modeltime_table(
  ensemble_fit_mean,
  ensemble_fit_median
) %>%
  modeltime_accuracy(testing(splits))

# * Weighted ----
loadings_tbl <- calibration_tbl %>%
  modeltime_accuracy() %>%
  mutate(rank = min_rank(-rmse)) %>%
  select(.model_id, rank)

ensemble_fit_wt <- calibration_tbl %>%
  ensemble_weighted(loadings = loadings_tbl$rank)

ensemble_fit_wt$fit$loadings_tbl

modeltime_table(
  ensemble_fit_wt
) %>%
  modeltime_accuracy(testing(splits))

ensemble_fit_median |>
  modeltime_table() |>
  modeltime_forecast(
    h = 24,
    actual_data = data_prepared_tbl
  ) |>
  plot_modeltime_forecast(
    .conf_interval_show = FALSE
  )
