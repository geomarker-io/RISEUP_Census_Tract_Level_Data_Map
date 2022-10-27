library(tidyverse)

download_weather <- function(var, year) {
  download.file(glue::glue("https://aqs.epa.gov/aqsweb/airdata/daily_{var}_{year}.zip"),
                destfile = glue::glue("./raw-data/{var}_{year}.zip"))
  unzip(glue::glue("./raw-data/{var}_{year}.zip"), exdir = "./raw-data")
  unlink(glue::glue("./raw-data/{var}_{year}.zip"))
  read_csv(glue::glue("./raw-data/daily_{var}_{year}.csv")) |>
    filter(`State Code` == "39",
           `County Code` == "061",
           `Site Num` == "0040") |>
    select(date = `Date Local`,
           name = `Parameter Name`,
           value = `Arithmetic Mean`)
}

var <- c("WIND", "TEMP", "RH_DP")

weather_21 <- purrr::map2(var, 2021, download_weather)
weather_22 <-purrr::map2(var, 2022, download_weather)

weather <- bind_rows(weather_21, weather_22)

weather <- weather |>
  pivot_wider(names_from = name,
              values_from = value)

saveRDS(weather, "data/weather.rds")

