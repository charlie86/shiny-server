library(tidyverse)
library(leaflet)
library(shiny)
library(highcharter)
library(glue)
library(shinydashboard)
library(countrycode)

rm(list = ls())

load('data/countries.RData')
load('data/country_features.RData')
load('data/geo_tracks.RData')

feature_vars <- c('danceability', 'energy', 'loudness', 'speechiness', 'acousticness', 'instrumentalness', 'liveness', 'valence', 'tempo', 'duration_ms')

navbarPageWithText <- function(..., text) {
    navbar <- navbarPage(...)
    textEl <- tags$p(class = "navbar-text", text)
    navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
        navbar[[3]][[1]]$children[[1]], textEl)
    navbar
}