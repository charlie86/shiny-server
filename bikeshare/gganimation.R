library(ggplot2)
library(gganimate)
library(rgdal)
library(RMySQL)
library(ggthemes)
library(tidyverse)
library(animation)

setwd('C:/Users/Charlie/Documents/Side Projects/midnight barber/shiny-server/bikeshare/')

geojson_raw <- readOGR('neighorhoods.geojson.txt', 'OGRGeoJSON')

conn <- dbConnect(MySQL(), 
                  user = 'rcharlie',
                  password = 'charlie86', 
                  dbname = 'rcharlie',
                  host = "rcharlie.ch74fm7hgclb.us-west-2.rds.amazonaws.com")

dbGetQuery(conn, 'select * from bikeshare_deduped limit 5')

test <- dbGetQuery(conn, "
                   SELECT
                    bsl.neighborhood
                    , dayofweek(date_add(bd.last_updated, INTERVAL -4 HOUR)) as day_of_week
                    , hour(date_add(bd.last_updated, INTERVAL -4 HOUR)) as hour
                    , sum(bd.bikes) as num_bikes
                    , sum(bd.docks) as num_docks
                    , sum(bd.bikes) / sum(bd.bikes + bd.docks) AS pct_full
                   FROM bikeshare_station_lookup bsl
                   INNER JOIN bikeshare_deduped bd
                   ON bsl.station_id = bd.station_id
                    AND bsl.neighborhood IS NOT NULL
                    AND bd.last_updated IS NOT NULL
                   group by 1,2,3;")

test %>% filter(day_of_week == 5) %>% hchart(hcaes(x = hour, y= pct_full), type='line')

test <- mydf %>%
    filter(!is.na(neighborhood)) %>% 
    group_by(weekday, hour, neighborhood) %>%
    summarise(pct_full = sum(bikes) / sum(bikes, docks)) %>% 
    ungroup %>% 
    filter(weekday == 'Wednesday')

ggdata <- fortify(geojson_raw) %>% 
    mutate(id = as.numeric(id)) %>% 
    left_join(geojson_raw@data, by = 'id') %>% 
    mutate_at(c('quadrant', 'subhood'), as.character) %>%
    left_join(test, by = c('subhood' = 'neighborhood'))



magickPath <- shortPathName("C:\\Program Files\\ImageMagick-7.0.5-Q16\\magick.exe")
ani.options(convert=magickPath)

Sys.setenv(PATH = paste("C:/Program Files/ImageMagick-7.0.2-Q16/www/", Sys.getenv("PATH"), sep = ";"))


p = ggplot(data = ggdata, aes(x = long, y = lat, group = id, fill = pct_full, frame = hour)) +
    geom_polygon(color = 'black') +
    scale_fill_gradient(low = 'white', high = 'blue') +
    theme_tufte() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())

gganimate(p)
