# DATA --------------------------------------------------------------------

WHICH_DATASET <- "RTO"

if (WHICH_DATASET == "FZ") {
  tag_tbl <- nexaverser::fz_data
  y <- "lb_fz_filtros033silw_zn"
} else {
  tag_tbl <- readRDS("data-raw/rto_flotacao.rds") |>
    dplyr::select(-x7210_lab_prod_cf_zinco_zn)
  y <- "lab_flot_cf_wil_zn"
}

# tag_imp <- nexaverser::impute_missing_values(tag_tbl)

tag_imp <- tidyr::drop_na(tag_tbl)


# TOOLS -------------------------------------------------------------------

nexaverser::fz_data |> nexaverser::plot_tag_data()

ds <- nexaverser::fz_data[, 1:2]

ds |> utils::tail(120) |> nexaverser::forecast_tag()

at <- nexaverser::assess_tag(ds |> utils::tail(120), .imp = TRUE,
                             .clean = TRUE)

at$trend_data |> dplyr::select(date, value) |> utils::tail(120) |>
  nexaverser::forecast_tag()


# FEATURE SELECTION -------------------------------------------------------

# * Feature Selection 1 ----
sel_features_1 <- nexaverser::select_features_with_boruta(
  .tag_dat        = tag_imp,
  .target         = y,
  .balance        = TRUE,
  .with_tentative = FALSE,
  .return_data    = TRUE,
  .task           = "regression"
)

# * Feature Selection 2 ----
sel_features_2 <- nexaverser::select_features_with_trex(
  .tag_dat        = tag_imp,
  .target         = y,
  .balance        = TRUE,
  .return_data    = TRUE,
  .task           = "regression"
)

# * Feature Selection 3 ----
sel_features_3 <- nexaverser::select_features_with_pps(
  .tag_dat     = tag_imp,
  .target      = y,
  .balance     = FALSE,
  .return_data = TRUE,
  .task        = "regression"
)


# MODELING 1 --------------------------------------------------------------

df <- sel_features_2$data |>
  dplyr::select(dplyr::all_of(c(sel_features_2$selected_features, y)))

tictoc::tic()
cubist_model <- nexaverser::train_cubist_model(
  .data            = df,
  .target          = y,
  .strat           = FALSE,
  .tune            = FALSE,
  .surrogate_model = TRUE
)
tictoc::toc()

cubist_vip <- cubist_model$model$fit$fit$fit |> vip::vip()


# MODELING 2 --------------------------------------------------------------

df <- sel_features_3$data |>
  dplyr::select(dplyr::all_of(c(sel_features_3$selected_features, y)))

tictoc::tic()
xgb_model <- nexaverser::train_xgboost_model(
  .data            = df,
  .target          = y,
  .strat           = FALSE,
  .tune            = FALSE,
  .surrogate_model = TRUE
)
tictoc::toc()

xgb_vip <- xgb_model$model$fit$fit$fit |> vip::vip()


# MODELING 3 --------------------------------------------------------------

df <- sel_features_3$data |>
  dplyr::select(dplyr::all_of(c(sel_features_3$selected_features, y)))

tictoc::tic()
mars_model <- nexaverser::train_mars_model(
  .data            = df,
  .target          = y,
  .strat           = FALSE,
  .tune            = FALSE,
  .surrogate_model = TRUE
)
tictoc::toc()

mars_vip <- mars_model$model$fit$fit$fit |> vip::vip()


# MODELING 4 --------------------------------------------------------------

data <- df[, c(selected_vars, y)]

# Splits
set.seed(1)
splits <- rsample::initial_split(data, prop = 0.8, strata = y)
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
best <- h2o::h2o.getModel(model_ids[1])

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


# CETERIS PARIBUS ---------------------------------------------------------

cp_plt <- nexaverser::coeteris_paribus(
  .model   = xgb_model$model,
  .newdata = df,
  .target  = y
)

xvar <- xgb_vip$data$Variable[1]

yvar <- xgb_vip$data$Variable[2]

cp_plt[[xvar]]
cp_plt[[yvar]]
cp_plt$ait_44003
cp_plt$lab_pb_ag_al_flot_bulk_zn


# PLANT PERFORMANCE MAPS --------------------------------------------------

xvar <- mars_vip$data$Variable[1]

yvar <- mars_vip$data$Variable[2]

zvar <- y

res  <- 100 # 3d plots resolution

ppm <- nexaverser::plant_performance_map(
  .model = mars_model$model,
  .data  = df,
  .xvar  = xvar,
  .yvar  = yvar,
  .zvar  = zvar,
  .res   = 100
)

ppm


# CLUSTERING --------------------------------------------------------------

clusters <- nexaverser::cluster_ts(tag_imp, 3)


# MORE FORECASTING --------------------------------------------------------

# * Data ----

data_prepared_tbl <- tag_imp[, 1:2] |>
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

# splits <- timetk::time_series_split(
#   data       = data_prepared_tbl,
#   date_var   = date,
#   assess     = "24 hours",
#   cumulative = TRUE
# )
#
# splits |>
#   timetk::tk_time_series_cv_plan() |>
#   timetk::plot_time_series_cv_plan(date, value)

# * Basic Prophet ----

prophet_fit <- nexaverser::forecast_prophet(
  .tag_dat = data_prepared_tbl,
  .assess  = "24 hours",
  .horiz   = 24
)

attributes(prophet_fit)[["plot"]]

# * NNETAR ----

nnetar_fit <- nexaverser::forecast_nnetar(
  .tag_dat         = data_prepared_tbl,
  .assess          = "24 hours",
  .horiz           = 24,
  .seasonal_period = "auto"
)

attributes(nnetar_fit)[["plot"]]

# * TBATS Model ----

tbats_fit <- nexaverser::forecast_tbats(
  .tag_dat = data_prepared_tbl,
  .assess            = "24 hours",
  .horiz             = 24,
  .seasonal_period_1 = "auto"
)

attributes(tbats_fit)[["plot"]]

# * STLM ETS Model ----

model_fit_stlm_ets <- modeltime::seasonal_reg(
  seasonal_period_1 = 2,
  seasonal_period_2 = 4,
  seasonal_period_3 = 8
) |>
  parsnip::set_engine("stlm_ets") |>
  parsnip::fit(value ~ date, rsample::training(splits))

model_fit_stlm_ets$fit$models$model_1$stl |> ggplot2::autoplot()

calibrate_and_plot(model_fit_stlm_ets, .splits = splits,
                   .actual_data = data_prepared_tbl)

# * STLM ARIMA Model ----

model_fit_stlm_arima <- modeltime::seasonal_reg(
  seasonal_period_1 = 2,
  seasonal_period_2 = 4,
  seasonal_period_3 = 8
) |>
  parsnip::set_engine("stlm_arima") |>
  parsnip::fit(value ~ date, rsample::training(splits))

model_fit_stlm_arima

calibrate_and_plot(model_fit_stlm_arima, .splits = splits,
                   .actual_data = data_prepared_tbl)


# EVALUATION --------------------------------------------------------------

# * Modeltime ----

model_tbl <- modeltime::modeltime_table(
  model_fit_prophet,
  model_fit_nnetar,
  model_fit_tbats,
  model_fit_stlm_ets,
  model_fit_stlm_arima
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
