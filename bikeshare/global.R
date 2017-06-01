library(highcharter)
library(jsonlite)
library(tidyverse)
library(shiny)
library(leaflet)
library(zoo)
library(httr)
library(RMySQL)
library(rvest)
library(ggthemes)
library(lubridate)
library(stringr)
library(shinydashboard)
library(XML)

setwd('C:/Users/Charlie/Documents/Side Projects/midnight barber/shiny-server/bikeshare/')

conn <- dbConnect(MySQL(), 
                  user = 'rcharlie',
                  password = 'charlie86', 
                  dbname = 'rcharlie',
                  host = "rcharlie.ch74fm7hgclb.us-west-2.rds.amazonaws.com")


test <- dbGetQuery(conn, "
                   SELECT
                    bsl.neighborhood
                    , hour(bd.last_updated) as hour
                    , sum(bd.bikes) / sum(bd.bikes + bd.docks) AS pct_full
                   FROM bikeshare_station_lookup bsl
                   INNER JOIN bikeshare_deduped bd
                   ON bsl.station_id = bd.station_id
                    AND bsl.neighborhood IS NOT NULL
                    AND bd.last_updated IS NOT NULL
                   group by 1,2;")

df <- test %>% 
    mutate(hour = hour(last_updated)) %>%
    filter(!is.na(neighborhood))

base_table <- expand.grid(station_id = unique(df$station_id), hour = seq(0,24), stringsAsFactors = F)

tots <- base_table %>% 
    left_join(df, by = c('station_id', 'hour')) %>% 
    group_by(station_id) %>% 
    arrange(hour) %>% 
    slice(min(which(!is.na(bikes))):n()) %>%
    mutate_all(na.locf) %>% 
    group_by(station_id, hour) %>% 
    filter(last_updated == min(last_updated)) %>% 
    ungroup



tots %>% 
    filter(station_id == 31100) %>% 
    sel
    
    arrange(hour) %>% 
    select(hour, bikes, bikes_all) %>% 
    mutate(bikes_all = na.locf(bikes)) %>%
    group_by(station_name, hour) %>% 
    filter(last_updated == min(last_updated)) %>% 
    group_by(neighborhood, hour) %>% 
    summarise(pct_full = sum(bikes) / (sum(docks) + sum(bikes)))

library(rgdal)
library(jsonlite)
library(pxweb)
geojson_raw <- readOGR('neighorhoods.geojson.txt', 'OGRGeoJSON')


library(geojsonio)
myjson = fromJSON('https://opendata.arcgis.com/datasets/f6c703ebe2534fc3800609a07bad8f5b_17.geojson')
myjson$features$properties$fillColor <- 'blue'
t_myjson = geojsonio::as.json(myjson)

mapdata <- get_data_from_map(download_map_data("countries/us/us-all"))
glimpse(mapdata)
data_fake <- mapdata %>% 
    select(code = `hc-a2`) %>% 
    mutate(value = 1e5 * abs(rt(nrow(.), df = 10)))



glimpse(data_fake)

highchart(type = 'map') %>% 
    hc_add_series(mapData = t_myjson, type = 'mapline') %>%
    hc_plotOptions(mapline = list(fillColor = 'blue'))

?hc_add_series.geo_json

geojson_raw$capacity <- map(as.character(geojson_raw$subhood), function(x) {
    nine_am$pct_full[nine_am$neighborhood == x]
}) %>% as.numeric

highchart() %>% 
    hc_add_series(mapData = geojson_raw)

rm(list = ls())

map(dbListConnections(MySQL()), dbDisconnect)

conn <- dbConnect(MySQL(),
                  user = 'rcharlie',
                  password = 'charlie86',
                  dbname = 'rcharlie',
                  host = "rcharlie.ch74fm7hgclb.us-west-2.rds.amazonaws.com")

stations <- dbReadTable(conn, 'bikeshare_station_lookup')

source('hc_boxplot.R')

bike_icon <- makeIcon(iconUrl = 'https://cdn1.iconfinder.com/data/icons/bicycle-bike-sport/154/geo-point-bike-bicycle-cycle-sport-512.png', iconWidth = 40)
days_of_week <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')
dates_df <- expand.grid(mytime = seq(0, 24, by = .01),
                        date = seq(as.Date('2017-03-21'), as.Date('2017-05-06'), by = 'day'))

res <- GET('https://www.capitalbikeshare.com/data/stations/bikeStations.xml') %>% xmlParse %>% xmlToList

snapshot <- map_df(seq_len(length(res) - 1), function(x) {
    res[[x]] %>%
        unlist %>%
        t %>%
        as.data.frame(stringsAsFactors = F)
}) %>%
    mutate_at(vars(lastCommWithServer, latestUpdateTime), funs((as.numeric(.) / 1000) %>% as.POSIXct(origin = '1970-01-01'))) %>%
    mutate_at(vars(installed, locked, temporary, public), funs(. == 'true')) %>%
    mutate_at(vars(lat, long, nbBikes, nbEmptyDocks, terminalName), as.numeric) %>%
    rowwise %>%
    mutate(popup = HTML(paste0('<h3>', name, '</h3>',
                               '<p><b>Bikes available: </b>', nbBikes, '</p>',
                               '<p><b>Empty docks: </b>', nbEmptyDocks, '</p>'))) %>%
    ungroup

library(rgdal)
geojson_raw <- readOGR('neighorhoods.geojson.txt', 'OGRGeoJSON')

time_df <- tots %>%
    group_by(neighborhood, hour) %>% 
    summarise(pct_full = sum(bikes) / (sum(docks) + sum(bikes)))


map(seq(0, 23), function(x) {
    ggdata <- fortify(geojson_raw) %>% 
        mutate(id = as.numeric(id)) %>% 
        left_join(geojson_raw@data, by = 'id') %>% 
        mutate_at(c('quadrant', 'subhood'), as.character) %>% 
        left_join(test %>% filter(hour == x), by = c('subhood' = 'neighborhood'))
    
    ggplot() +
        geom_polygon(data = ggdata, aes(x = long, y = lat, group = id, fill = pct_full), color = 'black') +
        scale_fill_gradient(low = 'white', high = 'blue') +
        theme_tufte() +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank()) +
        ggtitle(x)
})

map(0:23, function(i) {
    geojson_raw$capacity <- map(as.character(geojson_raw$subhood), function(x) {
        test$pct_full[test$neighborhood == x & test$hour == i]
    }) %>% as.numeric
    pal <<- colorNumeric(
        palette = 'Blues',
        domain = geojson_raw$capacity)
    leaflet() %>% 
        addPolygons(data = geojson_raw, color = ~pal(capacity), weight = 1, opacity = 1, fillOpacity = 1)
})

tots %>% 
    mutate(pct_full = bikes / docks) %>% 
    filter(station_name == 'Columbia Rd & Belmont St NW') %>% 
    select(hour) %>% 
    unique

?ggplot

pointsDF <- stations %>% 
    select(longitude, latitude)

# Convert pointsDF to a SpatialPoints object 
pointsSP <- SpatialPoints(pointsDF, 
                          proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Use 'over' to get _indices_ of the Polygons object containing each point 
indices <- over(pointsSP, geojson_raw)
stations$hood <- indices$subhood
stations$quadrant <- indices$quadrant
stations$tooltip <- paste(stations$name, '-', stations$hood)
