## code to prepare `fz_data` dataset goes here
fz_data_1 <- readxl::read_xlsx("data-raw/TAGS FZ.xlsx", sheet = 1, skip = 2)

names(fz_data_1) |>
  grep(pattern = "...", fixed = TRUE) -> .

value_col_names <- setdiff(names(fz_data_1), names(fz_data_1)[.])

value_col_names |>
  grep(pattern = "^[0-9]", invert = TRUE) -> .

value_col_names <- value_col_names[.] |>
  janitor::make_clean_names()

cols_to_keep <- colSums(is.na(fz_data_1)) < nrow(fz_data_1)

rows_to_keep <- rowSums(is.na(fz_data_1)) < ncol(fz_data_1)

dates <- fz_data_1[rows_to_keep, 2] |> dplyr::pull(1)

fz_data_1 <- fz_data_1[rows_to_keep, cols_to_keep] |>
  dplyr::select(tidyselect::where(is.character)) |>
  dplyr::mutate(date = as.Date(dates)) |>
  janitor::clean_names() |>
  dplyr::relocate(date) |>
  purrr::set_names(c("date", value_col_names)) |>
  dplyr::mutate(dplyr::across(tidyselect::where(is.character), as.numeric)) |>
  timetk::summarise_by_time(
    .date_var = date,
    .by = "day",
    dplyr::across(tidyselect::where(is.numeric), ~mean(.x, na.rm = TRUE))
  )

fz_data_2 <- readxl::read_excel("data-raw/Dados-FZ-5-anos.xlsx")

fz_data_2 <- fz_data_2 |>
  dplyr::mutate(Data = as.Date(Data)) |>
  dplyr::rename(date = Data) |>
  timetk::filter_by_time(.date_var = date, .start_date = fz_data_1$date[1],
                         .end_date = fz_data_1$date |> utils::tail(1)) |>
  janitor::clean_names()

fz_data_3 <- purrr::map2_df(
  .x = fz_data_1 |> dplyr::select(-date),
  .y = fz_data_2 |> dplyr::select(-date),
  .f = \(.x, .y) {
    dt <- tibble::tibble(x = .x, y = .y)
    dm <- apply(X = dt, MARGIN = 1, FUN = mean, na.rm = TRUE)
    return(dm)
  }
)

fz_data_4 <- readxl::read_xlsx("data-raw/TAGS FZ COMPLEMENTARES.xlsx",
                               sheet = 2, skip = 1)

names(fz_data_4) |>
  grep(pattern = "...", fixed = TRUE) -> .

value_col_names <- setdiff(names(fz_data_4), names(fz_data_4)[.])

value_col_names <- value_col_names |>
  janitor::make_clean_names()

cols_to_keep <- colSums(is.na(fz_data_4)) < nrow(fz_data_4)

rows_to_keep <- rowSums(is.na(fz_data_4)) < ncol(fz_data_4)

dates <- fz_data_4[rows_to_keep, 1] |> dplyr::pull(1)

fz_data_4 <- fz_data_4[rows_to_keep, cols_to_keep] |>
  dplyr::select(tidyselect::where(\(x) is.character(x) || is.numeric(x))) |>
  dplyr::mutate(date = as.Date(dates)) |>
  janitor::clean_names() |>
  dplyr::relocate(date) |>
  purrr::set_names(c("date", value_col_names)) |>
  dplyr::mutate(dplyr::across(tidyselect::where(is.character), as.numeric)) |>
  timetk::summarise_by_time(
    .date_var = date,
    .by = "day",
    dplyr::across(tidyselect::where(is.numeric), ~mean(.x, na.rm = TRUE))
  ) |>
  dplyr::select(-fz_fit_006)

fz_data <- dplyr::bind_cols(date = fz_data_1$date, fz_data_3) |>
  dplyr::left_join(
    x  = _,
    y  = fz_data_4,
    by = "date"
  ) |>
  dplyr::select(-lx_b021_fe_t) |>
  dplyr::relocate(lb_fz_filtros033silw_zn, .after = tidyselect::last_col())

visdat::vis_miss(fz_data)

# fz_data <- readxl::read_excel("data-raw/lb_fz_filtros033silw_zn.xlsx")

usethis::use_data(fz_data, overwrite = TRUE)
