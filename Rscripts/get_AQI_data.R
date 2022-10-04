

setwd("U:/Investigator Folders/DBE/Cole Brokamp/RISEUP/AQI")

# download all files and stack to one

year <- 2015:2022

dwnld_url <- glue::glue('https://aqs.epa.gov/aqsweb/airdata/daily_aqi_by_county_{year}.zip')
                        
dest <- glue::glue("data/{year}.zip")

purrr::walk2(dwnld_url, dest, ~func_download(.x, .y))

func_download <- function(url, dest){
  download.file(url, destfile=dest, mode = "wb")
  
  unzip(dest, exdir = 'data/raw')
  unlink(dest)
}

file <- glue::glue("data/raw/daily_aqi_by_county_{year}.csv")

aqi <- purrr::map(file, ~read_csv(.x))

aqi <- bind_rows(aqi) 

aqi.hamilton <- aqi %>% 
  filter(`State Name` == "Ohio" & `county Name` == "Hamilton")

saveRDS(aqi.hamilton, "data/aqi_hamilton.rds")

# plot five year data
ggplot(aqi.hamilton, aes(x=Date, y=AQI)) +
  geom_line( color="steelblue") + 
  xlab("Year") +
  ylab("Air Qualtiy Index") +
  theme_ipsum() +
  scale_x_date(limit=c(as.Date("2015-01-01"),as.Date("2022-03-31"))) +
  annotate("rect",
           xmin = as.Date("2015-01-01"), xmax = as.Date("2022-03-31"),
           ymin = 0, ymax = 50,
           fill = "green",
           alpha = 0.2) + 
  annotate("rect",
           xmin = as.Date("2015-01-01"), xmax = as.Date("2022-03-31"),
                ymin = 51, ymax = 100,
            fill = "yellow",
            alpha = 0.2) + 
  annotate("rect",
           xmin = as.Date("2015-01-01"), xmax = as.Date("2022-03-31"),
            ymin = 101, ymax = 150,
            fill = "orange",
            alpha = 0.2) + 
  annotate("rect",
           xmin = as.Date("2015-01-01"), xmax = as.Date("2022-03-31"),
            ymin = 151, ymax = 200,
            fill = "red",
            alpha = 0.2) 



