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
    mutate(date_pulled = as.POSIXct(date_pulled) - hours(4),
           date = as.Date(date_pulled),
           time = strftime(round(date_pulled, 'mins'), format="%H:%M:%S"),
           hour = as.numeric(substr(time,1,2))) %>%
    filter(hour >= 7, hour <= 10) %>% 
    arrange(date_pulled)





# hchart(df %>% group_by(name) %>% filter(max(nbBikes) > 25), hcaes(x = as.character(date_pulled), y = nbBikes, group = name), type = 'line')
hchart(df, hcaes(x = as.character(time), y = nbBikes, group = date), type = 'line')

ggplot(df, aes(x = date_pulled, y = nbBikes, group = name)) +
    geom_line()