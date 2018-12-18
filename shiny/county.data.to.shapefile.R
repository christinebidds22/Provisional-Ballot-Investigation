library(sp)
library(tidyverse)
library(rgdal)

# all fips
states <- c("Delaware", "District of Columbia", "Maryland", "New York", "New Jersey", "Ohio",
           "Pennsylvania", "Virginia")

state_fips <- c("11", "10", "24", "34", "36", "39", "42", "51")

counties <- readOGR("cb_2017_us_county_500k.shp")

sandy_vars <- read.csv("~/Desktop/ads_shiny/sandy_vars.csv", stringsAsFactors = FALSE,
                       colClasses = c("fips" = "character")) %>%
   select(-c(county, state)) %>%
   gather(variable, value, total_eligible_voters:no_excuse_absentee) %>%
   mutate(value = round(value, 3))

counties <- counties[counties@data$STATEFP %in% state_fips, ]

counties@data <- counties@data %>% mutate(fips = str_c(as.character(GEOID), "00000"))

counties <- sp::merge(counties, sandy_vars, duplicateGeoms = TRUE)

counties@data <- counties@data %>% replace_na(list(year = 0000, variable = "none", value = 0.0))

writeOGR(obj = counties, dsn = getwd(), layer = "ads_counties", driver = "ESRI Shapefile")
