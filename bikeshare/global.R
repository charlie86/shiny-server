library(highcharter)
library(tidyverse)
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

# setwd('C:/Users/Charlie/Documents/Side Projects/midnight barber/shiny-server/bikeshare/')
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

# res <- GET('https://www.capitalbikeshare.com/data/stations/bikeStations.xml') %>% xmlParse %>% xmlToList

# snapshot <- map_df(seq_len(length(res) - 1), function(x) {
#     res[[x]] %>% 
#         unlist %>% 
#         t %>% 
#         as.data.frame(stringsAsFactors = F)
# }) %>%
#     mutate_at(vars(lastCommWithServer, latestUpdateTime), funs((as.numeric(.) / 1000) %>% as.POSIXct(origin = '1970-01-01'))) %>% 
#     mutate_at(vars(installed, locked, temporary, public), funs(. == 'true')) %>% 
#     mutate_at(vars(lat, long, nbBikes, nbEmptyDocks, terminalName), as.numeric) %>% 
#     rowwise %>% 
#     mutate(popup = HTML(paste0('<h3>', name, '</h3>', 
#                                '<p><b>Bikes available: </b>', nbBikes, '</p>', 
#                                '<p><b>Empty docks: </b>', nbEmptyDocks, '</p>'))) %>% 
#     ungroup
