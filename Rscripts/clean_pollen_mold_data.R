library(tidyverse)

d <- readxl::read_excel("data/pollen_mold_2022-10-11.xlsx",
                        sheet = 1,
                        col_names = FALSE)

d <- t(d)
d <- as_tibble(d, .name_repair = "unique")
d[1,1] <- "date"
colnames(d) <- d[1,]
d <- d[-1,]

d_2021 <- readxl::read_excel("data/pollen_mold_2021.xlsx",
                        sheet = 1,
                        col_names = FALSE)

d_2021 <- t(d_2021)
d_2021 <- as_tibble(d_2021, .name_repair = "unique")
d_2021[1,1] <- "date"
colnames(d_2021) <- d_2021[1,]
d_2021 <- d_2021[-1,]

# make separate tables
d_pollen <- select(d_2021, 1:29) |>
  bind_rows(select(d, 1:29)) |>
  mutate(date = as.numeric(date),
         date = as.character(as.Date(date, origin = "1899-12-30")),
         across(.cols = 2:29, .fns = as.numeric))

d_pollen_calculations <- select(d_2021, 1, 56:83) |>
  bind_rows(select(d, 1, 56:83)) |>
  mutate(date = as.numeric(date),
         date = as.character(as.Date(date, origin = "1899-12-30")),
         across(.cols = 2:29, .fns = as.numeric))

d_mold <- select(d_2021, 1, 30:53) |>
  bind_rows(select(d, 1, 30:53)) |>
  mutate(date = as.numeric(date),
         date = as.character(as.Date(date, origin = "1899-12-30")),
         across(.cols = 2:25, .fns = as.numeric))

d_mold_calculations <- select(d_2021, 1, 85:108) |>
  bind_rows(select(d, 1, 85:108)) |>
  mutate(date = as.numeric(date),
         date = as.character(as.Date(date, origin = "1899-12-30")),
         across(.cols = 2:25, .fns = as.numeric))

saveRDS(d_pollen, "data/pollen.rds")
saveRDS(d_pollen_calculations, "data/pollen_calculations.rds")
saveRDS(d_mold, "data/mold.rds")
saveRDS(d_mold_calculations, "data/mold_calculations.rds")
