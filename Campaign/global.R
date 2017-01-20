library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(scales)
library(streamR)
library(sp)
library(tidyr)
library(DT)
library(RMySQL)
library(shinyjs)
library(rgdal)
library(shinydashboard)
# library(RColorBrewer)
library(ROAuth)
# library(rCharts)
library(grid)
library(leaflet)
library(gridExtra)
library(highcharter)
# library(extrafont)
library(png)
# library(tidytext)
library(gtable)
library(chuckr)
library(mongolite)

chuck <- 'C:/Users/B1GRU/Documents/R/Fun/Proyectos/Campaign'
aws <- '/srv/shiny-server/Campaign/'
chuck_ubuntu <- '/srv/midnight-barber/shiny-server/Campaign/'
chuck_vb <- 'C:/Users/Charlie/Documents/Side Projects/midnight barber/shiny-server/Campaign'

if (dir.exists(aws)) {
    setwd(aws)
} else if (dir.exists(chuck)) {
    setwd(chuck)
} else if (dir.exists(chuck_vb)) {
    setwd(chuck_vb)
} else stop('Donde estas?')

# twitter oauth
# load('data/my_oauth.RData')
# load('data/states_sp.RData')

# mysql connection
# mydb <- dbConnect(MySQL(), 
#                   user = 'chuckteezy',
#                   password = 'charlie86',
#                   dbname = 'chuckDB',
#                   host = 'chuckdb.ch74fm7hgclb.us-west-2.rds.amazonaws.com')

# candidate faces
clinton_face <- readPNG('data/pics/clinton.png')
trump_face <- readPNG('data/pics/trump.png')
# 
# clinton_face_ts <- clinton_face %>% rasterGrob()
# trump_face_ts <- trump_face %>% rasterGrob()
# 
# clinton_face_race <- clinton_face %>% #rasterGrob
#     rasterGrob(width = 8)
# trump_face_race <- trump_face %>% # rasterGrob
#     rasterGrob(width = 10)
# 
happy_face <- readPNG('data/pics/happy.png') %>% #rasterGrob
    rasterGrob(width = 8)
angry_face <- readPNG('data/pics/angry.png') %>% # rasterGrob
    rasterGrob(width = 8)

timeframes <- c('Last minute' = 1, 
               'Last five minutes' = 5, 
               'Last hour' = 60)

# format candidate names
# candidates <- data.frame(Name = c('Hillary Clinton', 'Donald Trump')) %>% 
#     mutate(username = str_replace(Name, ' ', ''),
#            username = ifelse(username == 'DonaldTrump', paste0('real', username), username)) #%>% 

### Sentiment data and funcitons
# clinton_pos <- data.frame(candidate = 'Clinton', word = c('imwithher', 'votehillary'), sentiment = 'positive')
# clinton_neg <- data.frame(candidate = 'Clinton', word = c('nohillary', 'hillno', 'donewithher', 'hilliery', 'neverhillary', 'hillaryforprison2016'), sentiment = 'negative')
# 
# trump_pos <- data.frame(candidate = 'Trump', word = c('makemaericagreatagain', 'teamtrump', 'trumptrain', 'americafirst'), sentiment = 'positive')
# trump_neg <- data.frame(candidate = 'Trump', word = c('nevertrump'), sentiment = 'negative')
# 
# candidate_words <- rbind(clinton_pos, clinton_neg, trump_pos, trump_neg)
# candidate_words <- candidate_words %>% 
#     rbind(candidate_words %>% 
#               mutate(candidate = ifelse(candidate == 'Trump', 'Clinton', 'Trump'),
#                      sentiment = ifelse(sentiment == 'positive', 'negative', 'positive'))) %>% 
#     mutate_each(funs(as.character(.)))

# bing <- sentiments %>%
#     filter(lexicon == 'bing') %>%
#     select(word, sentiment)
# 
# nrc <- sentiments %>% 
#     filter(lexicon == 'nrc') %>% 
#     select(word, sentiment)
# 
# make.sentiment <- function(positive, negative) {
#     if (positive > negative) {
#         sent <- 'positive'
#     } else if (positive < negative) {
#         sent <- 'negative'
#     } else if (positive == negative) {
#         sent <- 'neutral'
#     }
#     return(sent)
# }
# 
ggplot_with_subtitle <- function(gg,
                                 label="",
                                 fontfamily=NULL,
                                 fontsize=10,
                                 hjust=0, vjust=0,
                                 bottom_margin=5.5,
                                 newpage=is.null(vp),
                                 vp=NULL,
                                 ...) {

    if (is.null(fontfamily)) {
        gpr <- gpar(fontsize=fontsize, ...)
    } else {
        gpr <- gpar(fontfamily=fontfamily, fontsize=fontsize, ...)
    }

    subtitle <- textGrob(label, x=unit(hjust, "npc"), y=unit(hjust, "npc"),
                         hjust=hjust, vjust=vjust,
                         gp=gpr)

    data <- ggplot_build(gg)

    gt <- ggplot_gtable(data)
    gt <- gtable_add_rows(gt, grobHeight(subtitle), 2)
    gt <- gtable_add_grob(gt, subtitle, 3, 4, 3, 4, 8, "off", "subtitle")
    gt <- gtable_add_rows(gt, grid::unit(bottom_margin, "pt"), 3)

    if (newpage) grid.newpage()

    if (is.null(vp)) {
        grid.draw(gt)
    } else {
        if (is.character(vp)) seekViewport(vp) else pushViewport(vp)
        grid.draw(gt)
        upViewport()
    }

    invisible(data)

}

# get tweets function
# get.live.twitts <- function(file.name = '', track = candidates$username, timeout = 5, oauth = my_oauth) {
#     twitts <- filterStream(file.name = file.name, track = track, timeout = timeout, oauth = my_oauth)
#     
#     if (length(twitts) > 10) {
#         twitts <- twitts %>%
#             parseTweets %>% 
#             filter(lang == 'en') %>% 
#             select(text, created_at, id_str)
#         invisible(vapply(word(candidates$Name, 2), function(x) {
#             twitts[, x] <<- str_detect(tolower(str_replace_all(twitts$text, '[^[:graph:]]', ' ')), tolower(x))
#         }, logical(nrow(twitts))))
#         twitts <- twitts %>%
#             rowwise %>% 
#             filter(sum(Clinton, Trump) == 1) %>% 
#             ungroup %>% 
#             gather(candidate, mention, c(Clinton, Trump)) %>% 
#             filter(mention == T) %>% 
#             select(-mention) %>% 
#             mutate(created_at = as.POSIXct(created_at, format = '%a %b %d %H:%M:%S'),
#                    time = max(created_at))
#     } else {
#         twitts <- data.frame()
#     }
#     
#     return(twitts)
# }

# aggregate sentiment plot
agg.sent <- function(time) {

        
    
    
    g <- ggplot(data = plot_df, aes(x = score, y = yint)) +
        geom_hline(yintercept = 0) +
        geom_point() +
        scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(-0, 1, by = .2), labels = percent) +
        annotation_custom(angry_face, xmin = -.11, xmax = -.09) +
        annotation_custom(happy_face, xmin = 1.09, xmax = 1.11) +
        annotation_custom(clinton_face_race,
                          xmin = plot_df$pct_pos[plot_df$candidate == 'Clinton'] - .01,
                          xmax = plot_df$pct_pos[plot_df$candidate == 'Clinton'] + .01) +
        annotation_custom(trump_face_race,
                          xmin = plot_df$pct_pos[plot_df$candidate == 'Trump'] - .01,
                          xmax = plot_df$pct_pos[plot_df$candidate == 'Trump'] + .01) +
        theme_tufte() +
        labs(x = NULL, y = NULL, title = 'Live Twitter candidate sentiment') +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(size = 20),
              axis.ticks.length = unit(.4, 'cm'),
              plot.title = element_text(hjust = 0,
                                        family = 'FranklinGothic-Heavy',
                                        size = 30,
                                        margin = margin(b = 10)))
    
    ggplot_with_subtitle(g, paste0('Percentage of tweets sent in the ', tolower(time), ' with positive sentiment'), 
                         fontfamily = 'FranklinGothic-Book',
                         fontsize = 18,
                         bottom_margin = 20, lineheight = .9)
}

# time series sentiment plot
# ts.sent <- function(plot_sent) {
#     
#     plot_df <- plot_df %>%
#         mutate(text = str_replace_all(text, '[[:punct:]]|[[:cntrl:]]|\\d+', ''),
#                text = iconv(text, 'UTF-8', 'ASCII')) %>%
#         unnest_tokens(word, text) %>%
#         filter(!is.na(candidate)) %>%
#         anti_join(stop_words, by = 'word') %>%
#         left_join(bing, by = 'word') %>%
#         left_join(candidate_words, by = c('candidate', 'word')) %>%
#         mutate(sentiment = ifelse(!is.na(sentiment.y), sentiment.y, sentiment.x),
#                sentiment = ifelse(is.na(sentiment), 'neutral', sentiment)) %>%
#         count(id_str, candidate, sentiment, time) %>%
#         spread(sentiment, n, fill = 0) %>%
#         mutate(sentiment = make.sentiment(positive, negative)) %>%
#         group_by(candidate, time) %>%
#         summarise(pct_pos = mean(sentiment == 'positive'),
#                   pct_neg = mean(sentiment == 'negative')) %>%
#         ungroup %>%
#         arrange(time)
#     
#     options(scipen = 13)
#     h1 <- hPlot(x = 'time', y = plot_sent, group = 'candidate', data = plot_df %>% mutate(time = as.numeric(as.POSIXct(time - hours(8))) * 1000))
#     h1$colors(c('#232066', '#E91D0E'))
#     h1$xAxis(type = 'datetime', labels = list(
#         format = '{value:%I:%M:%S %p EDT}'
#     ))
#     h1$yAxis(max = 1)
#     
#     return(h1)
#     
# }

# bar plot of tweet count
# bar.plot <- function() {
#     
#     plot_df <- plot_df %>%
#         mutate(text = str_replace_all(text, '[[:punct:]]|[[:cntrl:]]|\\d+', ''),
#                text = iconv(text, 'UTF-8', 'ASCII')) %>%
#         unnest_tokens(word, text) %>%
#         filter(!is.na(candidate)) %>%
#         anti_join(stop_words, by = 'word') %>%
#         left_join(nrc, by = 'word') %>%
#         left_join(candidate_words, by = c('candidate', 'word')) %>%
#         mutate(sentiment = ifelse(!is.na(sentiment.y), sentiment.y, sentiment.x),
#                sentiment = ifelse(is.na(sentiment), 'neutral', sentiment)) %>%
#         filter(sentiment != 'neutral') %>% 
#         count(candidate, sentiment) %>%
#         ungroup
#     
#     b1 <- hPlot(y = 'n', x = 'candidate', type = 'column', group = 'sentiment', data = plot_df)
# 
# }

# raw twitts
raw.twitts <- function(sentiment) {
    
    text_df <- plot_df %>% 
        group_by(candidate) %>%
        filter(time == max(time)) %>%
        ungroup %>% 
        mutate(text = str_replace_all(text, '[[:punct:]]|[[:cntrl:]]|\\d+', ''),
               text = iconv(text, 'UTF-8', 'ASCII')) %>%
        unnest_tokens(word, text) %>%
        filter(!is.na(candidate)) %>% 
        anti_join(stop_words, by = 'word') %>%
        left_join(bing, by = 'word') %>%
        mutate(sentiment = ifelse(is.na(sentiment), 'neutral', sentiment)) %>% 
        count(id_str, candidate, sentiment) %>%
        spread(sentiment, n, fill = 0) %>% 
        group_by(candidate) %>% 
        filter_(paste(sentiment, '==', 'max(', sentiment, ')'),
                paste(sentiment, '> 0')) %>% 
        slice(1) %>% 
        ungroup %>% 
        left_join(plot_df %>% select(text, id_str), by = 'id_str')
    
    HTML(
        paste0('<b>', 'Clinton', ': </b>', text_df$text[text_df$candidate == 'Clinton'], '<br>',
               '<b>', 'Trump', ': </b>', text_df$text[text_df$candidate == 'Trump'], '<br>')
    )
}

#function to get states from longitude and latitude
# get_states = function(lon, lat) {
#     coords = data.frame(cbind(test$place_lon, test$place_lat))
#     points_sp = SpatialPoints(coords)
#     proj4string(points_sp) = proj4string(states_sp)
#     i = over(points_sp, states_sp)
#     names = sapply(states_sp@polygons, function(x) x@ID)
#     return (names[i])
# }



