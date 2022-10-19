## code to prepare `fz_data` dataset goes here
fz_data <- rio::import("data-raw/Dados-FZ-5-anos.xlsx")

fz_data <- fz_data |>
  janitor::clean_names() |>
  dplyr::rename(date = data)

perc_na <- round(colSums(is.na(fz_data)) / nrow(fz_data) * 100, 0)

keep <- perc_na < 30

fz_data <- fz_data[keep]

usethis::use_data(fz_data, overwrite = TRUE)
