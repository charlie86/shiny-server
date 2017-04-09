library(shiny)
library(tidyverse)
library(highcharter)
library(scales)

setwd('C:/Users/Charlie/Documents/Side Projects/midnight barber/shiny-server/coachella/')

load('tracks.RData')
load('events.RData')

tol21rainbow <- c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")
feature_vars <- c('danceability', 'energy', 'loudness', 'speechiness', 'acousticness', 'instrumentalness', 'liveness', 'valence', 'tempo', 'duration_ms', 'mode', 'key')
