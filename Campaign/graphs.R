library(chuckr)
library(scales)
library(tidyr)
library(dplyr)
library(rCharts)
library(lubridate)
library(RColorBrewer)
library(highcharter)

setwd('C:/Users/Charlie/Documents/Side Projects/midnight barber/shiny-server/Campaign')

twitts_og <- cQuery("SELECT 
       candidate,
       # `State.name` AS state_name,
        # `State.FIPS` AS state_fips,
        # `County.name` AS country_name,
        `County.FIPS` AS county_fips,
        avg(positive - negative) AS score
       FROM twitts
        WHERE candidate IS NOT NULL
        AND `State.name` IS NOT NULL
       GROUP BY 1,2")

data(usgeojson)

data(uscountygeojson)

twitts <- twitts_og %>%
    mutate(score = rescale(score, to = c(-1, 1))) %>% 
    spread(candidate, score, fill = 0) %>% 
    mutate(diff = round(Clinton - Trump, 2),
           # name = state_name)
           fips = county_fips)


highchart() %>% 
    hc_add_series_map(
        uscountygeojson, twitts,
        name = "Tweet Score", value = "diff", joinBy = "fips") %>% 
    hc_colorAxis(dataClasses = color_classes(breaks = c(-1, 0, 1), colorRampPalette(c('darkred', 'darkblue'))(3))) %>% 
    hc_legend(layout = "vertical", align = "right", floating = TRUE)

stuff <- cQuery("SELECT
        concat(date(created_at), ' ', hour(created_at), ':00') AS time,
        candidate,
        avg(positive - negative) AS score
        FROM twitts
        WHERE candidate IS NOT NULL
       GROUP BY 1,2")

stuff_plot <- stuff %>% 
    mutate(time = as.numeric(as.POSIXct(time) - hours(4)) * 1000,
           score = round(rescale(score, to = c(-1, 1)), 2)) %>% 
    mutate(color = ifelse(candidate == 'Clinton', 'blue', 'red'))

h1 <- hPlot(x = 'time', y = 'score', group = 'candidate', type = 'spline', data = stuff_plot)

h1$xAxis(type = 'datetime', labels = list(
    format = '{value:%Y-%m-%d %I%p}'
))
h1$yAxis(floor = -1, ceiling = 1)
h1$colors()
