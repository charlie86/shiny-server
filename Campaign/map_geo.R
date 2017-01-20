library(chuckr)
library(dplyr)
library(leaflet)
library(sp)
library(tidyr)
library(rgdal)
library(broom)
library(maps)

setwd('C:/Users/Charlie/Documents/Side Projects/midnight barber/shiny-server/Campaign/')

county <- readOGR(dsn = 'tl_2015_us_county', layer = 'tl_2015_us_county') 
twitts <- cQuery('select * from twitts where place_lat is not null limit 200')
    
coordinates(twitts) <- c('place_lon', 'place_lat')
proj4string(twitts) <- proj4string(county)

twitts$county_fips_full <- as.numeric(over(twitts, county)$GEOID)
twitts$county_fips <- as.character(over(twitts, county)$COUNTYFP)
twitts$county_name <- as.character(over(twitts, county)$NAME)
twitts$state_fips <- as.character(over(twitts, county)$STATEFP)

twitts <- as.data.frame(twitts)

twitts$place_lat

leaflet() %>% 
    addTiles() %>% 
    addMarkers(twitts$place_lon, twitts$place_lat, popup = twitts$county_name)


cQuery('select count(*) from twitts where place_lat is not null and `State.name` is null')

