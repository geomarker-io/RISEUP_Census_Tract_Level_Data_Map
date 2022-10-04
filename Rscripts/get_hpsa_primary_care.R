library(tidyverse)
library(sf)


options(timeout = 1000)
download.file('https://data.hrsa.gov//DataDownload/DD_Files/HPSA_PNTPC_SHP.zip',
              destfile = 'data/hpsa_pc.zip')
unzip('data/hpsa_pc.zip', exdir = 'data')
unlink('data/hpsa_pc.zip')

d <- sf::st_read('data/HPSA_PNTPC_SHP_DET_CUR_VX.shp')

d <- d %>%
  select(HpsNM) %>%
  st_make_valid()

states <- tigris::states() %>%
  st_drop_geometry() %>%
  select(NAME) %>%
  filter(!NAME %in% c('Alaska', 'Hawaii', 'United States Virgin Islands',
                      'Commonwealth of the Northern Mariana Islands',
                      'Guam', 'American Samoa', 'Puerto Rico'))

all_tracts <- map(states$NAME, ~tigris::tracts(state = .x, year=2010)) %>%
  bind_rows() %>%
  st_transform(st_crs(d)) %>%
  select(census_tract_id = GEOID10) %>%
  st_make_valid()

tract_hpsa_pc <- st_join(all_tracts, d, join = st_intersects) %>%
  filter(!duplicated(census_tract_id)) %>%
  mutate(hpsa_pc = ifelse(is.na(HpsNM), "no", "yes")) %>%
  select(census_tract_id, hpsa_pc)
  # %>% st_drop_geometry()

saveRDS(tract_hpsa_pc, "data/hpsa_pc.rds")