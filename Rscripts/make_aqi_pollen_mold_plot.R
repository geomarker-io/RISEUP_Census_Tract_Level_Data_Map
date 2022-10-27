library(tidyverse)

aqi.hamilton <- readRDS("data/aqi_hamilton.rds") |>
  filter(Date < as.Date("2022-12-31"),
         Date > "2020-12-31") |>
  rename(date = Date)

pollen <- readRDS("data/pollen_calculations.rds") |>
  mutate(date = as.Date(date)) |>
  select(date,
         pollen = Total) |>
  filter(!is.na(pollen),
         !is.na(date),
         date < max(aqi.hamilton$date))

mold <- readRDS("data/mold_calculations.rds")|>
  mutate(date = as.Date(date)) |>
  select(date,
         mold = Total) |>
  filter(!is.na(mold),
         !is.na(date),
         date < max(aqi.hamilton$date))

weather <- readRDS("data/weather.rds")

p <- ggplot() +
  geom_line(data = weather,
            aes(x = date, y = `Outdoor Temperature`),
            alpha = 0.5) +
  geom_line(data = weather,
            aes(x = date, y = `Relative Humidity`),
            alpha = 0.5,
            color = "darkred") +
  geom_line(data = aqi.hamilton,
            aes(x = date, y = AQI),
            color = "steelblue",
            alpha = 0.5) +
  geom_line(data = pollen,
            aes(x = date, y = log(pollen + 0.0001) + 50),
            color = "goldenrod",
            alpha = 0.5) +
  geom_line(data = mold,
            aes(x = date, y = log(mold + 0.0001) + 50),
            color = "darkgreen",
            alpha = 0.5)

plotly::ggplotly(p)

d <- aqi.hamilton |>
  select(date,
         AQI) |>
  left_join(pollen, by = "date") |>
  mutate(pollen = log(pollen + 0.0001)) |>
  left_join(mold, by = "date") |>
  left_join(weather, by = "date")
  # pivot_longer(cols = c(AQI, pollen, mold),
  #              names_to = "type",
  #              values_to = "value")

GGally::ggpairs(d, columns = 2:8)

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

# log transformations
pollen <- mutate(pollen,
                 pollen_log = log(Total + 0.001, 2))

mold <- mutate(mold,
               mold_log = log(Total + 0.001))

ggplot() +
  geom_line(data = aqi.hamilton |>
              filter(Date < as.Date("2022-01-01")),
            aes(x = Date,
                y = log(AQI + 0.001, 2)),
            alpha = 0.5) +
  geom_line(data = pollen |>
              filter(date < as.Date("2022-01-01")),
            aes(x = date,
                y = log(Total + 0.001, 2)),
            color = "blue",
            alpha = 0.5) +
  geom_line(data = mold |>
              filter(date < as.Date("2022-01-01")),
            aes(x = date,
                y = log(Total + 0.001, 2)),
            color = "darkgreen",
            alpha = 0.5)

d_wide <- aqi.hamilton |>
  select(date = Date,
         AQI) |>
  left_join(pollen, by = "date") |>
  rename(pollen = Total) |>
  left_join(mold, by = "date") |>
  rename(mold = Total)


ggplot() +
  geom_point(data = d_wide,
             aes(x = AQI,
                 y = pollen))

ggplot() +
  geom_point(data = d_wide,
             aes(x = AQI,
                 y = log(pollen + 0.001, 2)))

ggplot() +
  geom_point(data = d_wide,
             aes(x = AQI,
                 y = mold))

ggplot() +
  geom_point(data = d_wide,
             aes(x = AQI,
                 y = log(mold + 0.001, 2)))

