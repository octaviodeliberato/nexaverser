# TESTS -------------------------------------------------------------------

nexaverser::fz_data |> nexaverser::plot_tag_data()

df <- nexaverser::fz_data[, 1:2]

df |> utils::tail(120) |> nexaverser::forecast_tag()

at <- nexaverser::assess_tag(df |> utils::tail(120), .imp = TRUE,
                             .clean = TRUE)

at$trend_data |> dplyr::select(date, value) |> utils::tail(120) |>
  nexaverser::forecast_tag()


# FEATURE SELECTION -------------------------------------------------------

# data
tag_tbl <- nexaverser::fz_data

# tag_imp <- nexaverser::impute_missing_values(tag_tbl)

tag_imp <- tidyr::drop_na(tag_tbl)

# * Feature Selection 1 ----
sel_features_1 <- nexaverser::select_features_with_boruta(
  .tag_dat        = tag_imp,
  .target         = "lb_fz_filtros033silw_zn",
  .balance        = TRUE,
  .with_tentative = TRUE,
  .return_data    = FALSE,
  .task           = "regression"
)

# * Feature Selection 2 ----
sel_features_2 <- nexaverser::select_features_with_trex(
  .tag_dat        = tag_imp,
  .target         = "lb_fz_filtros033silw_zn",
  .balance        = TRUE,
  .with_tentative = TRUE,
  .return_data    = TRUE,
  .task           = "regression"
)


# MODELING 1 --------------------------------------------------------------

df <- sel_features_2$data

cubist_model <- nexaverser::train_cubist_model(
  .data   = df,
  .target = "lb_fz_filtros033silw_zn",
  .strat  = FALSE,
  .tune   = FALSE,
  .surrogate_model = TRUE
)

# MODELING 2 --------------------------------------------------------------

y <- "lb_fz_filtros033silw_zn"

data <- df[, c(selected_vars, y)]

# Splits
set.seed(1)
splits <- rsample::initial_split(data, prop = 0.8, strata = y)
train  <- rsample::training(splits)
test   <- rsample::testing(splits)

rec_obj <- healthyR.ai::hai_xgboost_data_prepper(train, f)

auto_xgboost <- healthyR.ai::hai_auto_xgboost(
  .data        = train,
  .rec_obj     = rec_obj,
  .model_type  = "regression",
  .best_metric = "rmse",
  .tune        = TRUE
)

best_model <- auto_xgboost$model_info$fitted_wflw

# Check performance
test_pred <- predict(best_model, new_data = test)

df_test <- tibble::tibble(
  actual = test[[y]],
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


# MODELING 3 --------------------------------------------------------------

y <- "lb_fz_filtros033silw_zn"

data <- df[, c(selected_vars, y)]

# Splits
set.seed(1)
splits <- rsample::initial_split(data, prop = 0.8, strata = y)
train  <- rsample::training(splits)
test   <- rsample::testing(splits)

rec_obj <- healthyR.ai::hai_earth_data_prepper(train, f)

auto_earth <- healthyR.ai::hai_auto_earth(
  .data        = train,
  .rec_obj     = rec_obj,
  .model_type  = "regression",
  .best_metric = "rmse",
  .tune        = TRUE
)

best_model <- auto_earth$model_info$fitted_wflw

# Check performance
test_pred <- predict(best_model, new_data = test)

df_test <- tibble::tibble(
  actual = test[[y]],
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


# MODELING 4 --------------------------------------------------------------

y <- "lb_fz_filtros033silw_zn"

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
