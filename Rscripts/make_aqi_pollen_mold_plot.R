library(tidyverse)

aqi.hamilton <- readRDS("data/aqi_hamilton.rds") |>
  filter(Date < as.Date("2022-12-31"),
         Date > "2020-12-31")

pollen <- readRDS("data/pollen_calculations.rds") |>
  mutate(date = as.Date(date)) |>
  select(date, Total) |>
  filter(!is.na(Total))

mold <- readRDS("data/mold_calculations.rds")|>
  mutate(date = as.Date(date)) |>
  select(date, Total) |>
  filter(!is.na(Total))


d <- aqi.hamilton |>
  select(date = Date,
         AQI) |>
  left_join(pollen, by = "date") |>
  rename(pollen = Total) |>
  left_join(mold, by = "date") |>
  rename(mold = Total) |>
  filter(!is.na(pollen)) |>
  pivot_longer(cols = c(AQI, pollen, mold),
               names_to = "type",
               values_to = "value")
ggplot() +
  geom_line(aes(x = date,
                y = value,
                group = type,
                color = type),
            data = d) +
  facet_grid(rows = vars(type),
             scales = "free") +
  theme(legend.position = "none",
        axis.title = element_blank())

ggsave("aqi_pollen_mold_ts.png")
