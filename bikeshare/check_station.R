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
dbGetQuery(conn, "select count(*) from bikeshare_stations")

df <- df_raw %>% 
    mutate(last_comm_w_server = as.POSIXct(lastCommWithServer) - hours(4),
           date = as.Date(last_comm_w_server),
           dow = weekdays(date),
           time = strftime(round(last_comm_w_server, 'mins'), format="%H:%M:%S"),
           hour = as.numeric(substr(time,1,2)),
           minute = as.numeric(substr(time, 4, 5)),
           second = as.numeric(substr(time, 7, 8)),
           mytime = (hour*3600) + (minute*60) + second) %>%
    filter(hour >= 7, hour <= 10) %>% 
    arrange(mytime) %>% 
    group_by(date) %>% 
    mutate(empty_time = min(ifelse(nbBikes == 0, last_comm_w_server, NA), na.rm = T)) %>% 
    ungroup

dow <- df %>%
    select(dow, date, empty_time) %>% 
    unique %>% 
    arrange(date) %>% 
    mutate(empty_time = as.POSIXct(empty_time, origin = '1970-01-01'))

hchart(df, hcaes(x = mytime, y = nbBikes, group = date), type = 'line')
hchart(df, hcaes(x = dow, y = nbBikes, group = date), type = 'line')

ggplot(df, aes(x = time, y = nbBikes, group = as.factor(date), color = as.factor(date))) +
    geom_line()
