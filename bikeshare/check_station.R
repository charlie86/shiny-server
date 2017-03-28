library(highcharter)
library(tidyverse)
library(RMySQL)
library(lubridate)

conn <- dbConnect(MySQL(), 
                  user = 'rcharlie',
                  password = 'charlie86', 
                  dbname = 'rcharlie',
                  host = "rcharlie.ch74fm7hgclb.us-west-2.rds.amazonaws.com")

df_raw <- dbGetQuery(conn, "select * from bikeshare_stations where name = 'Columbia Rd & Belmont St NW'")

df <- df_raw %>% 
    mutate(date_pulled = as.POSIXct(date_pulled) - hours(4)) %>% 
    arrange(date_pulled)

# hchart(df %>% group_by(name) %>% filter(max(nbBikes) > 25), hcaes(x = as.character(date_pulled), y = nbBikes, group = name), type = 'line')
hchart(df %>% arrange(date_pulled), hcaes(x = as.character(date_pulled), y = nbBikes), type = 'line')

ggplot(df, aes(x = date_pulled, y = nbBikes, group = name)) +
    geom_line()