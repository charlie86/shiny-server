library(dplyr)
library(jsonlite)

setwd("C:/Users/Charlie/Documents/Side Projects/midnight barber/shiny-server/Campaign/")

twitts <- fromJSON(sprintf('[%s]', paste(readLines('twitts.json'), collapse = ',')))

sapply(twitts, class)

tp <- twitts %>% 
    select(text, created_at) %>% 
    mutate(trump = grepl('realdonaldtrump', tolower(text)),
           clinton = grepl('hillaryclinton', tolower(text)),
           johnson = grepl('govgaryjohnson', tolower(text)),
           stein = grepl('drjillstein', tolower(text))) %>% 
    rowwise %>% 
    filter(sum(trump, clinton, johnson, stein) == 1) %>% 
    ungroup %>% 
    select(-text) %>% 
    gather(candidate, mention, trump:stein) %>%
    filter(mention) %>% 
    count(candidate)

library(highcharter)
library(tidyr)

hchart(tp, x = candidate, )


