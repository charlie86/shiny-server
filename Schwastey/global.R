# library(albersusa) # devtools::install_github("hrbrmstr/hrbrmisc")
# library(rgeos)
# library(maptools)
# library(ggplot2)
# library(ggalt)
# library(ggthemes)
# library(jsonlite)
# library(xlsx)
# library(XLConnect)
library(shinydashboard)
library(tidyr)
library(dplyr)
# library(purrr)
# library(rvest)
library(magrittr)
library(RColorBrewer)
library(leaflet)
library(readxl)

# setwd('F:/2016-15-master')
# setwd('C:/Users/B1GRU/Documents/R/Fun/Proyectos/Data Viz/3 - Waste/2016-15-master/Schwasted')

load('wasteData.Rdata')
load('state_shape.Rdata')

mydf <- test %>% 
    mutate(state_name = ifelse(site.state == 'DC', 'District of Columbia', state.name[match(site.state, state.abb)]),
           site.id = row_number()) %>% 
    filter(!is.na(longitude))

num_locations <- mydf %>% 
    group_by(state_name) %>% 
    summarise(count = n())

states_og$count <- num_locations$count[match(states_og$NAME, num_locations$state_name)]
states_og$count <- ifelse(is.na(states_og$count), 0, states_og$count)

pal_states <- colorBin(palette = 'Reds', bins = c(0, 1, 5, 10, 20, 40, 50), domain = states_og$count)


previewColors(pal_states, values = states_og$diff)

pal_sites <- colorFactor(palette = 'Dark2', levels = unique(mydf$site.rating), unique(mydf$site.rating))

# Read in epa contaminant data
# getSheets(loadWorkbook('list10-excel-11-01-2013.xlsx'))

# epa <- do.call(rbind, lapply(1:10, function(x) {
#     read_excel('list10-excel-11-01-2013.xlsx', sheet = x)
# }))
# 
# epa %>% 
#     filter(!is.na(rconc_units_desc), !is.na(cc_max_conc_value_nmbr)) %>% 
#     group_by(rhs_name, rconc_units_desc) %>% 
#     summarise(amount = sum(cc_max_conc_value_nmbr)) %>% 
#     arrange(-amount)
# 
# epa %>% 
#     group_by(rmedia_desc) %>% 
#     summarise(count = n()) %>% 
#     arrange(-count)
# 
# mydf <- mydf %>% 
#     mutate(cap_site_name = toupper(site.name))
# 
# test <- mydf %>% 
#     left_join(epa, by = c('cap_site_name' = 'site_name'))

