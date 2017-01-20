library(chuckr)
library(highcharter)
library(tidyverse)
library(RMySQL)
library(lubridate)

conn <- dbConnect(MySQL(), user = "chuck", password = "charlie86", 
                  dbname = "tweetdb", host = "tweetdb.ch74fm7hgclb.us-west-2.rds.amazonaws.com")


dbGetQuery(conn, 'select count(*) from twitts')

dbGetQuery(conn, "
           SELECT
            sum(CASE WHEN TIMESTAMPDIFF(SECOND, from_uixtime(time), NOW()) <= 10 THEN 1 ELSE 0 END)
           FROM twitts")

twitts <- dbGetQuery(conn, "
SELECT
    FROM_UNIXTIME(time, '%Y-%m-%d') AS day
    , sum(trump) AS trump
    , sum(clinton) AS clinton
FROM twitts
GROUP BY 1
ORDER BY 1")

twitts %>% 
    mutate(day = as.numeric(as.POSIXct(twitts$day, format = '%Y-%m-%d') - hours(8)) * 1000) %>% 
    gather(candidate, twitts, trump, clinton) %>% 
    hchart(x = day, y = twitts, group = candidate, type = 'spline') %>% 
    hc_xAxis(title = list(text = ''), type = 'datetime', labels = list(format = '{value:%b %d}')) %>% 
    hc_yAxis(title = list(text = 'Number of Tweets')) %>% 
    hc_add_theme(hc_theme_538()) %>% 
    hc_title(text = 'Twitter loves Donald Trump') %>% 
    hc_subtitle(text = 'Number of Twitter mentions per day, by candidate')

max <- dbGetQuery(conn, 'select max(time) from twitts')


twitts <- dbGetQuery(conn, 'SELECT *
                     FROM twitts
                     WHERE TIMESTAMPDIFF(SECOND, from_uixtime(time), NOW()) <= 10')
                     # WHERE TIMESTAMPDIFF(SECOND, TIMESTAMP(time), DATE_SUB(NOW(), INTERVAL 4 HOUR)) <= 10')


ts_twitts <- function() {
    test <- dbGetQuery(conn, 'select * from twitts')
    myplot <- test %>% 
        mutate(time = as.POSIXct(round(as.POSIXct(time, origin = '1970-01-01'), 'hours'))) %>% 
        group_by(time) %>% 
        summarise_at(funs(sum(as.numeric(.), na.rm = T)), .cols = vars(clinton, trump,stein, johnson, kaine, pence)) %>% 
        gather(candidate, count, c(clinton, trump, stein, johnson, kaine, pence))
    hchart(myplot, x = as.numeric(time - hours(4)) * 1000, y = count, group = candidate, type = 'spline') %>% 
        hc_colors(c('blue', 'orange', 'green', 'red', 'pink', 'black')) %>% 
        hc_xAxis(title = list(text = ''), type = 'datetime', labels = list(format = '{value:%H:%M %p}')) %>% 
        hc_yAxis(title = list(text = 'Number of Tweets')) %>% 
        hc_add_theme(hc_theme_538()) %>% 
        hc_title(text = 'Twitter loves Donald Trump') %>% 
        hc_subtitle(text = 'Number of Twitter mentions per minute, by candidate')
}

as.POSIXct(1476152149, origin = '1970-01-01')


as.numeric(as.POSIXct('2016-10-09 19:00:00 EDT'))
as.numeric(as.POSIXct('2016-10-09 23:30:00 EDT'))

as.numeric(as.POSIXct('2016-10-07 16:00:00 EDT'))
as.numeric(as.POSIXct('2016-10-07 23:30:00 EDT'))

test <- dbGetQuery(conn, '
                   SELECT *
                   FROM twitts 
                   WHERE kaine IS NOT NULL 
                   AND time BETWEEN 1476054000 AND 1476063000')

test3 <- dbGetQuery(conn, '
                    SELECT *
                    FROM twitts
                    WHERE time BETWEEN 1475870400 AND 1475897400')

tots <- test3 %>% 
    select(-c(kaine, pence)) %>% 
    unique


twitts <- tots %>% 
    mutate(time = as.POSIXct(round(as.POSIXct(time, origin = '1970-01-01') - hours(4), 'mins'))) %>% 
    filter(time == '2016-10-07 17:48:00')

group_by(time) %>% 
    summarise_at(vars(trump, clinton), sum) %>% 
    ungroup %>% 
    mutate(time = as.numeric(time) * 1000) %>% 
    gather(candidate, tweets, trump, clinton) %>% 
    hchart(x = time, y = tweets, group = candidate, type = 'line') %>% 
    hc_xAxis(title = list(text = ''), type = 'datetime', labels = list(format = '{value:%H:%M %p}'))

tots %>% 
    
    
    hc_xAxis(title = list(text = ''), type = 'datetime', labels = list(format = '{value:%H:%M %p}')) %>% 
    