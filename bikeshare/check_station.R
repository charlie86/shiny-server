library(highcharter)
library(tidyverse)
library(leaflet)
library(RMySQL)
library(zoo)
library(lubridate)
library(stringr)
library(prophet)

setwd('C:/Users/Charlie/Documents/Side Projects/midnight barber/shiny-server/bikeshare/')

conn <- dbConnect(MySQL(), 
                  user = 'rcharlie',
                  password = 'charlie86', 
                  dbname = 'rcharlie',
                  host = "rcharlie.ch74fm7hgclb.us-west-2.rds.amazonaws.com")

# df_raw <- dbGetQuery(conn, "select * from bikeshare_stations where name = 'Columbia Rd & Belmont St NW'")
# dbGetQuery(conn, "select count(*) from bikeshare_stations")
# dbGetQuery(conn, "select max(lastCommWithServer) from bikeshare_stations")

dbSendQuery(conn, "DROP TABLE IF EXISTS bikeshare_station_lookup;")
dbSendQuery(conn, "
CREATE TABLE bikeshare_station_lookup (
    station_id INT,
    name VARCHAR(50),
    latitude FLOAT,
    longitude FLOAT,
    last_updated TIMESTAMP,
    last_comm_with_server TIMESTAMP);")

dbSendQuery(conn, "
INSERT INTO bikeshare_station_lookup
SELECT 
    `terminalName`
    , `name`
    , `lat`
    , `long`
    , max(`latestUpdateTime`)
    , max(`lastCommWithServer`)
FROM bikeshare_stations 
GROUP BY 1,2,3,4;")

dbSendQuery(conn, "DROP TABLE IF EXISTS bikeshare_deduped;")
dbSendQuery(conn, "
CREATE TABLE bikeshare_deduped (
    station_id INT
    , bikes INT
    , docks INT
    , last_updated TIMESTAMP);")
dbSendQuery(conn, "CREATE INDEX station_id ON bikeshare_deduped (station_id);")

dbSendQuery(conn, "
INSERT INTO bikeshare_deduped 
SELECT 
    DISTINCT `terminalName` AS station_id
    , `nbBikes` AS bikes
    , `nbEmptyDocks` AS docks
    , `latestUpdateTime` AS last_updated
FROM bikeshare_stations;")

dbGetQuery(conn, 'select count(*) from bikeshare_deduped')

station_name = 'Columbia Rd & Belmont St NW'
dbGetQuery(conn, "select * from bikeshare_station_lookup where name = 'Columbia Rd & Belmont St NW'")
test <- dbGetQuery(conn, paste0("
SELECT
    bsl.station_id
    , bsl.name AS station_name
    , bd.bikes
    , bd.docks
    , bd.last_updated
FROM bikeshare_station_lookup bsl
INNER JOIN bikeshare_deduped bd
    ON bsl.station_id = bd.station_id
WHERE bsl.station_id = '", 31113, "';"))

head(test)
head(test)
library(ggthemes)
test %>% 
    mutate(last_updated = as.POSIXct(last_updated)) %>% 
    ggplot(aes(x = last_updated, y = bikes)) +
        geom_line() +
        theme_tufte()



days_of_week <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')

endpoints <- round.POSIXt(range(test$last_updated), 'mins')
dates_df <- tibble(
    timestamp = seq.POSIXt(endpoints[1], endpoints[2], by = 'min')
) %>% 
    mutate(date = as.Date(timestamp),
           weekday = weekdays(date),
           weekday_num = match(weekday, days_of_week),
           hour = hour(timestamp), 
           minute = minute(timestamp),
           mytime = round(hour + (minute/60), 2))


mydf <- test %>% 
    mutate(last_updated_og = as.POSIXct(last_updated) - hours(4),
           last_updated = as.POSIXct(strftime(round(last_updated_og, 'mins')))) %>%
    group_by(last_updated) %>%
    filter(last_updated_og == max(last_updated_og)) %>%
    filter(row_number() == max(row_number())) %>%
    ungroup %>% 
    select(-last_updated_og) %>%
    right_join(dates_df, by = c('last_updated' = 'timestamp')) %>% 
    group_by(date) %>% 
    slice(min(which(!is.na(bikes))):n_distinct(mytime)) %>% 
    mutate_all(funs(na.locf(.))) %>% 
    ungroup

ts <- mydf %>% 
    select(last_updated, bikes)

myts <- ts(mydf$bikes, frequency = 60*24)

fit <- stl(myts, s.window="period")
plot(fit)

library(forecast)
fit <- HoltWinters(myts)
plot(forecast(fit, 60))
accuracy(fit)

mydf %>% 
    filter(weekday == 'Monday') %>%
    filter(mytime <= 10, mytime >= 6) %>%
    group_by(weekday, weekday_num, mytime) %>% 
    summarise(bikes = median(bikes)) %>% 
    ungroup %>% 
    arrange(weekday_num) %>% 
    hchart(hcaes(x = mytime, y = bikes, group = weekday), type = 'line')

head(mydf)

n_distinct(mydf)
nrow(mydf)


ggplot(data = mydf %>% filter(weekday == 'Monday'), aes(x = mytime, y = bikes, group = date)) +
    geom_line()
stations <- dbReadTable(conn, 'bikeshare_station_lookup')

dbGetQuery(conn, 'select max(last_updated) from bikeshare_deduped')

source('C:/Users/Charlie/Documents/Side Projects/midnight barber/shiny-server/bikeshare/hc_boxplot.R')

plotit('Columbia Rd & Belmont St NW')

plotit <- function(station_name) {

station_name = 'Columbia Rd & Belmont St NW'
test = dbGetQuery(conn, paste0("
SELECT
    bsl.station_id
    , bd.last_updated
    , bd.bikes
    , bd.docks
FROM bikeshare_station_lookup bsl
INNER JOIN bikeshare_deduped bd
    ON bsl.station_id = bd.station_id
WHERE bsl.name = '", station_name, "';"))


df <- test %>% 
    mutate(last_updated = as.POSIXct(last_updated) - hours(4),
           date = as.Date(last_updated),
           weekday = weekdays(date),
           weekday_num = match(weekday, days_of_week),
           time = strftime(round(last_updated, 'mins'), format="%H:%M:%S"),
           hour = as.numeric(substr(time,1,2)),
           minute = as.numeric(substr(time, 4, 5)),
           second = as.numeric(substr(time, 7, 8)),
           mytime = (hour*3600) + (minute*60) + second) %>%
    filter(hour >= 7, hour <= 10) %>%
    # arrange(mytime) %>% 
    group_by(date) %>% 
    mutate(empty_time = min(ifelse(bikes == 0, last_updated, NA), na.rm = T)) %>% 
    ungroup

dow <- df %>%
    select(weekday, weekday_num, date, empty_time) %>% 
    unique %>% 
    arrange(weekday_num) %>%  
    mutate(empty_time = as.POSIXct(empty_time, origin = '1970-01-01'),
           mytime = hour(empty_time) + (minute(empty_time) / 60),
           tooltip = paste0(weekday_num, ', ', month(empty_time), '/', day(empty_time), ' - ', hour(empty_time), ':', str_pad(minute(empty_time), 2, pad = 0), 'am'))



highchart() %>%
    hc_boxplot(dow$mytime, dow$dow, dow$dow_n, name = 'Time of Day') %>%
    hc_add_theme(hc_theme_smpl()) %>% 
    hc_title(text = 'When should I wake up?') %>% 
    hc_subtitle(text = 'Time of day my bikeshare runs out of bikes')
}

stations %>% head

library(leaflet)

bike_icon <- makeIcon(iconUrl = 'https://cdn1.iconfinder.com/data/icons/bicycle-bike-sport/154/geo-point-bike-bicycle-cycle-sport-512.png', iconWidth = 40)

leaflet() %>%
    setView(lat = 38.8983, lng = -77.0281, zoom = 15) %>%
    # addProviderTiles('Stamen.TonerLite') %>%
    addTiles() %>% 
    addMarkers(lng = stations$longitude, lat = stations$latitude, popup = stations$name, icon = bike_icon)

# hchart(df, hcaes(x = mytime, y = bikes, group = date), type = 'line')
# hchart(dow, hcaes(x = dow, y = mytime), type = 'scatter') %>% 
#     hc_tooltip(formatter = JS(paste0("function() {return this.point.tooltip;}")))
# 
# hchart(dow %>% arrange(date), hcaes(x = date, y = mytime), type = 'scatter')
# 
# ggplot(df, aes(x = time, y = bikes, group = as.factor(date), color = as.factor(date))) +
#     geom_line()