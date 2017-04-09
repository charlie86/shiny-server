library(httr)
library(shiny)
library(shinyjs)
library(shinyBS)
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
    'Snoop Dogg' = 'snoopdogg',
    'Barack Obama' = 'barackobama', 
    'Jonny Greenwood' = 'mrmarmite',
    'Trent Reznor' = 'treznor',
    'Jamie xx' = 'jamiexx',
    'Daft Punk' = 'daftpunkofficial',
    'Kendrick Lamar' = 'kendricklamar',
    'Ashton Kutcher' = 'aplusk'
)

neon_colors <- c(
    '#84DE02'
    , '#FF4466'
    , '#4BC7CF'
    , '#FF85CF'
    , '#FFDF46'
    , '#391285'
    , '#E88E5A'
    , '#DDE26A'
    , '#C53151'
    , '#B05C52'
    , '#FD5240'
    , '#FF4681'
    , '#FF6D3A'
    , '#FF404C'
    , '#A0E6FF'
)


pca_vars <- c('danceability', 'energy', 'loudness', 'speechiness', 'acousticness', 'instrumentalness', 'liveness', 'valence', 'tempo', 'duration_ms')