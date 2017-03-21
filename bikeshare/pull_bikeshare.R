library(shiny)
library(httr)
library(XML)
library(tidyverse)
library(RMySQL)

conn <- dbConnect(MySQL(), 
                  user = 'rcharlie',
                  password = 'charlie86', 
                  dbname = 'rcharlie',
                  host = "rcharlie.ch74fm7hgclb.us-west-2.rds.amazonaws.com")

res <- GET('https://www.capitalbikeshare.com/data/stations/bikeStations.xml') %>% xmlParse %>% xmlToList

bikeshare_stations <- map_df(seq_len(length(res) - 1), function(x) {
    res[[x]] %>% 
        unlist %>% 
        t %>% 
        as.data.frame(stringsAsFactors = F)
}) %>%
    mutate_at(vars(lastCommWithServer, latestUpdateTime), funs((as.numeric(.) / 1000) %>% as.POSIXct(origin = '1970-01-01'))) %>% 
    mutate_at(vars(installed, locked, temporary, public), funs(. == 'true')) %>% 
    mutate_at(vars(lat, long, nbBikes, nbEmptyDocks), as.numeric) %>% 
    rowwise %>% 
    mutate(popup = HTML(paste0('<h3>', name, '</h3>', 
                               '<p><b>Bikes available: </b>', nbBikes, '</p>', 
                               '<p><b>Empty docks: </b>', nbEmptyDocks, '</p>'))) %>% 
    ungroup %>% 
    mutate(date_pulled = Sys.time())

dbWriteTable(conn, 'bikeshare_stations', bikeshare_stations, append = T, overwrite = F)
dbDisconnect(conn)
