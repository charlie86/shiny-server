library(httr)
library(dplyr)
library(leaflet)
library(tidyr)
library(lubridate)
library(rgdal)
library(DT)
library(shiny)
library(shinydashboard)
library(rjson)
library(RMySQL)

icon_url <- 'https://www.wmata.com/systemimages/sprites/icons/rail-colors-se811fd61b6.png'

conn <- dbConnect(MySQL(), 
                  user = 'rcharlie',
                  password = 'charlie86', 
                  dbname = 'rcharlie',
                  host = "rcharlie.ch74fm7hgclb.us-west-2.rds.amazonaws.com")

pull <- function(url) {
    
    query <- content(GET(url, query = list(api_key = 'c69abd486c4343879f5e73f9272008dd')))[[1]]
    
    my_time <- Sys.time()
    
    do.call(rbind, lapply(seq_len(length(query)), function(x) {
        tmp <- query[[x]]
        lapply(names(tmp), function(y) {
            if (is.null(tmp[[y]])) {
                tmp[[y]] <<- NA
            }
        }) %>% invisible
        tmp %>% as.data.frame() %>% 
            mutate(time = my_time)
    }))
}

metrics <- c('metro_incidents' = 'https://api.wmata.com/Incidents.svc/json/Incidents',
             'metro_predictions' = 'https://api.wmata.com/StationPrediction.svc/json/GetPrediction/All',
             'metro_line_info' = 'https://api.wmata.com/Rail.svc/json/jLines',
             'metro_stations' = 'https://api.wmata.com/Rail.svc/json/jStations')

map_lines <- readOGR('lines.geojson', 'OGRGeoJSON')

train_icon <- makeIcon(
    iconUrl = 'http://icons.iconarchive.com/icons/icons8/android/512/Transport-Train-icon.png',
    iconWidth = 20
)
