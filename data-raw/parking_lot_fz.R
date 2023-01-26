# DATA --------------------------------------------------------------------

tag_tbl <- nexaverser::fz_data

y <- "lb_fz_filtros033silw_zn"

# keep <- colMeans(is.na(tag_tbl)) <= 0.3
# table(keep)
# which(!keep)
keep <- names(tag_tbl)

rescale_date <- function(date) {

  dt <- lubridate::ymd_hms(date, truncated = 3)
  dt_num <- lubridate::as.duration(dt - min(dt))

  return(1 + as.numeric(dt_num) / 86400)

}

# xx <- rescale_date(spring1$Day)

tag_tbl$date_num <- tag_tbl$date |>
 rescale_date()

tag_tbl$IDE <- "FZ"

# --- With Optimisation on parameters
param1 <- list(m0        = NULL,
               mm        = NULL,
               pp        = NULL,
               aa        = 0.001,
               expertMin = 0.1,
               expertMax = 3.0,
               sigma2_m0 = 1,
               sigma2_mm = 0.05,
               sigma2_pp = 1,
               K         = 5,
               seqp      = seq(0.5, 0.7, 0.1))

resu1 <- kfino::kfino_fit(datain  = tag_tbl,
                          Tvar    = "date_num", Yvar = y,
                          param   = param1,
                          doOptim = TRUE,
                          method  = "ML",
                          verbose = TRUE)

# flags are qualitative
kfino::kfino_plot(resuin = resu1, typeG = "quali",
                  Tvar = "date", Yvar = y, Ident = "IDE")

kfino::kfino_plot(resuin = resu1, typeG = "quanti",
                  Tvar = "date", Yvar = y, Ident = "IDE")

kfino::kfino_plot(resuin = resu1, typeG = "prediction",
                  Tvar = "date", Yvar = y, Ident = "IDE")

resu1$detectOutlier[, c("date", y)] |>
  utils::tail(120) |>
  nexaverser::forecast_tag()

resu1$detectOutlier[, c("date", "prediction")] |>
  utils::tail(120) |>
  nexaverser::forecast_tag()

tag_tbl <- resu1$detectOutlier |>
  dplyr::filter(flag == "OK") |>
  dplyr::select(dplyr::all_of(keep))

visdat::vis_miss(tag_tbl)

N <- (colMeans(!is.na(tag_tbl)) |> min()) * nrow(tag_tbl)

tag_imp <- tag_tbl[, keep] |>
  utils::tail(N) |>
  nexaverser::impute_missing_values() |>
  purrr::map_df(\(x) {
    if (class(x) != "Date") {
      timetk::ts_clean_vec(x)
    } else { x }
  })

tag_imp |> nexaverser::plot_tag_data()

assmnt_y <- tag_imp |>
  dplyr::select(dplyr::all_of(c("date", y))) |>
  nexaverser::assess_tag(.smooth = F, .anom = T, .chg_pts = T)


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
  .tag_dat = tag_imp |> utils::tail(120),
  .target  = y,
  .max_lag = 7
)


df <- tag_imp |>
  dplyr::select(dplyr::all_of(c(
    union(sel_features_1$selected_features, sel_features_4), y
  )))


# MODELING 1 --------------------------------------------------------------

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

# 1st round
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

tictoc::tic()
rf_model <- nexaverser::train_ranger_model(
  .data            = df,
  .target          = y,
  .strat           = FALSE,
  .tune            = FALSE,
  .surrogate_model = TRUE
)
tictoc::toc()

rf_vip <- rf_model$model$fit$fit$fit |> vip::vip()


# * OPTIMIZATION ----------------------------------------------------------V

# ** Jaya, more complex problem ----

vars  <- setdiff(names(df), y)

model   <- xgb_model$model
lower <- apply(df |> dplyr::select(all_of(vars)), 2, min)
upper <- apply(df |> dplyr::select(all_of(vars)), 2, max)

sol <- nexaverser::optimize_with_jaya(
  .model   = model,
  .vars    = vars,
  .lower   = lower,
  .upper   = upper,
  .maxiter = 10,
  .option  = "minimize"
)

sol
