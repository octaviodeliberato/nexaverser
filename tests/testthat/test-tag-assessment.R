test_that("plot_tag_data works", {
  dat <- fz_data[1:360, ]
  expect_type(
    plot_tag_data(dat),
    "list"
  )
})


test_that("plot_mavg_data works", {
  dat <- fz_data[1:360, 1:2] |>
    generate_trend_analysis_data(tag_dat = _, value_col = fz_s_0033_zn2)
  expect_type(
    plot_mavg_data(dat),
    "list"
  )
})


make_cases <- function() {
  expand.grid(
    pad     = c(FALSE, TRUE),
    imp     = c(TRUE),
    clean   = c(FALSE, TRUE),
    chg_pts = c(FALSE, TRUE),
    anom    = c(FALSE, TRUE)
  )
}


with_parameters_test_that(
  "assess_tag works",
  {
    expect_no_error(
      assess_tag(
        .tag_dat = fz_data[1:90, 1:2],
        .pad     = pad,
        .imp     = imp,
        .clean   = clean,
        .chg_pts = chg_pts,
        .anom    = anom
      )
    )
  },
  .cases = make_cases()
)


test_that("forecast_tag works", {
  expect_no_error(
    forecast_tag(.tag_dat = fz_data[1:90, 1:2])
  )
})


test_that("impute_missing_values works", {
  expect_no_error(
    impute_missing_values(.tag_dat = fz_data[1:360, ])
  )
})


dat <- fz_data[1:360, ] |>
  tidyr::replace_na(data = _, replace = list(fz_s_0033_zn2 = 0))

test_that("extract_anomalies works", {
  expect_no_error(
    extract_anomalies(
      data      = dat,
      date_col  = "date",
      value_col = "fz_s_0033_zn2",
      anomaly   = "Both"
    )
  )
})
