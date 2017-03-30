library(httr)
library(shiny)
library(shinyjs)
library(tidyverse)
library(stringr)
library(tibble)
library(highcharter)
library(RColorBrewer)
library(shinydashboard)
library(purrr)
library(htmltools)
library(lubridate)
library(lazyeval)
library(ggplot2)
library(ggthemes)
library(plotly)
library(DT)

source('helpers.R')

client_id <- 'c857dcec62a74825985e4749ef531abe'
client_secret <- '54af922e8c7a44f28eb339adb0f23656'
access_token <- POST('https://accounts.spotify.com/api/token',
                     accept_json(), authenticate(client_id, client_secret),
                     body = list(grant_type='client_credentials'),
                     encode = 'form', httr::config(http_version=2)) %>% content %>% .$access_token

jscode <-
    '$(document).on("shiny:connected", function(e) {
  var jsWidth = screen.width;
  Shiny.onInputChange("GetScreenWidth",jsWidth);
});
'
base_url <- 'https://api.spotify.com/v1/'

famous_users <- list(
    'Barack Obama' = 'barackobama', 
    'Snoop Dogg' = 'snoopdogg',
    'Jonny Greenwood' = 'mrmarmite',
    'Jamie xx' = 'jamiexx',
    'Daft Punk' = 'daftpunkofficial',
    'Kendrick Lamar' = 'kendricklamar'
)


tol14rainbow <- c("#882E72", "#B178A6", "#D6C1DE", "#1965B0", "#5289C7", "#7BAFDE", "#4EB265", "#90C987", "#CAE0AB", "#F7EE55", "#F6C141", "#F1932D", "#E8601C", "#DC050C")
tol15rainbow <- c("#114477", "#4477AA", "#77AADD", "#117755", "#44AA88", "#99CCBB", "#777711", "#AAAA44", "#DDDD77", "#771111", "#AA4444", "#DD7777", "#771144", "#AA4477", "#DD77AA")
tol18rainbow <- c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")
tol21rainbow <- c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")

pca_vars <- c('danceability', 'energy', 'loudness', 'speechiness', 'acousticness', 'instrumentalness', 'liveness', 'valence', 'tempo', 'duration_ms')