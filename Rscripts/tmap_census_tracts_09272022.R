
library(tidyverse)
library(tmap)
library(leaflet)


### get data -----------------------------------------------

census_mega_data <- readRDS("census_mega_dataset-main/census_mega_data_0.2.rds")
tract_hpsa_pc <- readRDS("data/hpsa_pc.rds")
mua <- readRDS("data/mua.rds") %>% 
  rename(census_tract_id = census_tract_fips)

d <- census_mega_data %>% 
  filter(census_tract_vintage == 2010) %>% 
  select(census_tract_id, census_tract_vintage,
         dep_index, fraction_assisted_income, fraction_high_school_edu, median_income,       
         fraction_no_health_ins, fraction_poverty, fraction_vacant_housing,
         adi,
         walkability_index,
         ice,
         lead_paint, diesel_pm, resp_hazard_ind, traffic_proximity, ozone_conc, pm_conc,
         coi, coi_education, coi_health_env, coi_social_econ,
         pct_1or2_risk_factors, pct_3ormore_risk_factors,
         hpsa_mh = hpsa,
         ) %>% 
  left_join(., tract_hpsa_pc, by='census_tract_id') %>%  # primary care professional shortage areas
  left_join(., mua, by='census_tract_id') %>%     # medically underserved areas
  mutate(hpsa_mh = replace(hpsa_mh,
                           is.na(hpsa_mh),
                           'no'),
         hpsa_pc = replace(hpsa_pc,
                           is.na(hpsa_pc),
                           'no'),
         mua = ifelse(mua == 0 | is.na(mua),
                      'no',
                      'yes')
         )

hamilton <- read_csv("data/hamilton_tract_to_cincy_neighborhood.csv") %>% 
  mutate(census_tract_id = as.character(fips_tract_id)) %>% 
  select(-fips_tract_id)

d.h <- hamilton %>% 
  left_join(., d, by="census_tract_id") %>% 
  st_sf(.) %>% 
  mutate(id = paste(neighborhood, census_tract_id, sep="_"))

saveRDS(d.h, file="data/hamilton_data.rds")

### mapping --------------------------------------------------

d.h <- d.h %>% 
  mutate(id = paste(neighborhood, census_tract_id, sep="_"))

tmap_mode('view')
tmap_options(max.categories = 81)


legend_labels <-  c('Community Material Deprivation Index' = 'dep_index', #1
                    'Neighborhood Atlas Area Deprivation Index' = 'adi', #2
                    'Index of Concentration at the Extremes' = 'ice', #3
                    '% Poverty' = 'fraction_poverty', #4
                    '% Assisted Income' = 'fraction_assisted_income', #5
                    '% No Health Insurance' = 'fraction_no_health_ins', #6
                    '% Vacant Housing' = 'fraction_vacant_housing', #7
                    'EPA EJScreen - Lead Paint Indicator' = 'lead_paint', #8
                    'EPA EJScreen - Concentration of Diesel Particulate Matter in Air (ug/m3)' = 'diesel_pm', #9
                    'EPA EJScreen - Air Toxics Respiratory Hazard Index' = 'resp_hazard_ind', #10
                    'EPA EJScreen - Traffic Proximity and Volume' = 'traffic_proximity', #11
                    'EPA EJScreen - Ozone Seasonal Average of Daily Maximum 8-Hour Concentration (ppb)' = 'ozone_conc', #12
                    'EPA EJScreen - Annual Average PM2.5 Level in Air (ug/m3)' = 'pm_conc', #13
                    "Census' Community Resilience Index - Rate of Individuals with One or Two Risk Factors" = 'pct_1or2_risk_factors', #14
                    "Census' Community Resilience Index - Rate of Individuals with Three or More Risk Factors" = 'pct_3ormore_risk_factors', #15

                    '% High School Education' = 'fraction_high_school_edu', #16
                    'Median Income' = 'median_income', #17
                    'National Walkability Index' = 'walkability_index', #18
                    'Child Opportunity Index' = 'coi', #19
                    'Child Opportunity Index - Education Domain' = 'coi_education', #20
                    'Child Opportunity Index - Health and Environment Domain' = 'coi_health_env', #21
                    'Child Opportunity Index - Social and Economic Domain' = 'coi_social_econ', #22
                    
                    'Mental Health Professional Shortage Areas' = 'hpsa_mh', #23
                    'Primary Care Professional Shortage Areas' = 'hpsa_pc', #24
                    'Medically Underserved Areas' = 'mua' #25
                    )    
data_col = c('dep_index', 'adi', 'ice', 'fraction_poverty', 'fraction_assisted_income', 'fraction_no_health_ins', 'fraction_vacant_housing', 
             'lead_paint', 'diesel_pm', 'resp_hazard_ind', 'traffic_proximity', 'ozone_conc', 'pm_conc', 
             'pct_1or2_risk_factors', 'pct_3ormore_risk_factors', 
             'fraction_high_school_edu', 'median_income', 
             'walkability_index', 
             'coi', 'coi_education', 'coi_health_env', 'coi_social_econ', 
             'hpsa_mh', 'hpsa_pc', 'mua')

tm <- tm_shape(d.h, 
               name = 'Neiborhoods') +
  tm_polygons(col="neighborhood", 
              alpha = 0.2,
              legend.show = FALSE,
              id = "neighborhood",
              palette = "grey50",
              popup.vars = legend_labels) +
  tm_shape(d.h) +
  tm_polygons(col = data_col[1:2], 
              title = names(legend_labels)[1:2],
              alpha = 0.5, 
              palette = '-viridis', 
              style = "quantile", 
              id = "id",
              popup.vars = legend_labels) +
  tm_shape(d.h) +
  tm_polygons(col = data_col[4:7], 
              title = names(legend_labels)[4:7],
              alpha = 0.5, 
              palette = '-viridis', 
              style = "quantile", 
              id = "id",
              popup.vars = legend_labels) +
  tm_shape(d.h) +
  tm_polygons(col = data_col[16:17], 
              title = names(legend_labels)[16:17],
              alpha = 0.5, 
              palette = 'viridis', 
              style = "quantile", 
              id = "id",
              popup.vars = legend_labels) +
  tm_shape(d.h) +
  tm_polygons(col = data_col[8:15], 
              title = names(legend_labels)[8:15],
              alpha = 0.5, 
              palette = '-viridis', 
              style = "quantile", 
              id = "id",
              popup.vars = legend_labels) +
  tm_shape(d.h) +
  tm_polygons(col = data_col[c(3, 18:22)], 
              title = names(legend_labels)[c(3, 18:22)],
              alpha = 0.5, 
              palette = 'viridis', 
              style = "quantile", 
              id = "id",
              popup.vars = legend_labels) +
  tm_shape(d.h) +
  tm_polygons(col=data_col[23:25], 
              title = names(legend_labels)[23:25], 
              alpha = 0.2,
              legend.show = FALSE,
              id = "neighborhood",
              palette = c("green", "purple"),
              popup.vars = legend_labels) +
  tm_facets(as.layers = TRUE, 
            free.scales.fill = TRUE) 
  
tm
