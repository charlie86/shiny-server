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
library(shinyjs)
library(rgdal)
library(shinydashboard)
library(RColorBrewer)
library(ROAuth)
library(grid)
library(gridExtra)
library(png)
library(tidytext)
# library(ggalt)

# setwd('C:/Users/B1GRU/Documents/R/Fun/Proyectos/Campaign')

# twitter oauth
load('data/my_oauth.RData')
load('data/states_sp.RData')

# CSS for loading screen
# appCSS <- "
# #loading-content {
#   position: absolute;
#   background: #000000;
#   opacity: 0.9;
#   z-index: 100;
#   left: 0;
#   right: 0;
#   height: 100%;
#   text-align: center;
#   color: #FFFFFF;
# }
# "

# read in good/bad words
good_text <- scan('data/positive-words.txt', what = 'character', comment.char = ';')
bad_text <- scan('data/negative-words.txt', what = 'character', comment.char = ';')
good_text <- c(good_text, 'upgrade', ':)', '#iVoted', 'voted')
bad_text <- c(bad_text, 'wtf', 'epicfail', 'douchebag')

# candidate faces
clinton_face <- readPNG('data/pics/clinton.png') 
sanders_face <- readPNG('data/pics/sanders.png')
trump_face <- readPNG('data/pics/trump.png') 
# cruz_face <- readPNG('data/pics/cruz.png')
# kasich_face <- readPNG('data/pics/kasich.png')

clinton_face_ts <- clinton_face %>% rasterGrob()
sanders_face_ts <- sanders_face %>% rasterGrob()
trump_face_ts <- trump_face %>% rasterGrob()
# cruz_face_ts <- cruz_face %>% rasterGrob()
# kasich_face_ts <- kasich_face %>% rasterGrob()

clinton_face_race <- clinton_face %>% rasterGrob(width = 10)
sanders_face_race <- sanders_face %>% rasterGrob(width = 10)
trump_face_race <- trump_face %>% rasterGrob(width = 12)
# cruz_face_race <- cruz_face %>% rasterGrob(width = 12)
# kasich_face_race <- kasich_face %>% rasterGrob(width = 9)

happy_face <- readPNG('data/pics/happy.png') %>% rasterGrob(width = 10)
angry_face <- readPNG('data/pics/angry.png') %>% rasterGrob(width = 10)

normalize <- function(x) {
    2 * ((x - min(x)) / (max(x) - min(x))) - 1
}

# sentiment scoring function
score.sentiment <- function(sentences, good_text, bad_text) {
    scores <- lapply(sentences, function(sentence, good_text, bad_text) {
        sentence <- str_replace_all(sentence, '[[:punct:]]|[[:cntrl:]]|\\d+', '')
        sentence <- tolower(iconv(sentence, 'UTF-8', 'ASCII'))
        word.list <- str_split(sentence, '\\s+')
        words <- unlist(word.list)
        
        pos.matches <- !is.na(match(words, good_text))
        neg.matches <- !is.na(match(words, bad_text))
        
        score <- sum(pos.matches) - sum(neg.matches)
        
        return(score)
    }, good_text, bad_text)
}

twitt.sentiment <- function(sentences, good_text, bad_text) {
    scores <- lapply(sentences, function(sentence, good_text, bad_text) {
        sentence <- str_replace_all(sentence, '[[:punct:]]|[[:cntrl:]]|\\d+', '')
        sentence <- tolower(iconv(sentence, 'UTF-8', 'ASCII'))
        word.list <- str_split(sentence, '\\s+')
        words <- unlist(word.list)
        
        pos.matches <- !is.na(match(words, good_text))
        neg.matches <- !is.na(match(words, bad_text))
        
        score <- sum(pos.matches) - sum(neg.matches)
        
        if (score > 0) {
            sentiment <- 'positive'
        } else if (score < 0) {
            sentiment <- 'negative'
        } else {
            sentiment <- 'neutral'
        }
        
        return(sentiment)
    }, good_text, bad_text)
}

# lapply(clinton_pos, function(x) {
#     grepl(x, 'this is a sentence')
# }) %>% unlist %>% sum
# 
# 
# clinton_pos <- c('imwithher', 'votehillary')
# clinton_neg <- c('nohillary', 'hillno', 'donewithher', 'hilliery', 'neverhillary')
# 
# sanders_pos <- c('feelthebern')
# sanders_neg <- c('bernedout')
# 
# trump_pos <- c('makemaericagreatagain', 'teamtrump', 'trumptrain')
# trump_neg <- c('nevertrump')
# 
# cruz_pos <- c('cruzcrew', 'unitewithcruz')
# cruz_neg <- c('nevercruz')

# add positive tweets of candidates to negative tweets of opposing candidates

# format candidate names
candidates <- data.frame(Name = c('Bernie Sanders', 'Hillary Clinton', 'Donald Trump')) %>% 
    mutate(username = str_replace(Name, ' ', ''),
           username = ifelse(username == 'DonaldTrump', paste0('real', username), username)) #%>% 
# filter(Name %in% c('Donald Trump', 'Hillary Clinton'))

# get tweets function
get.live.twitts <- function(file.name = '', track = candidates$username, timeout = 5, oauth = my_oauth) {
    twitts <- filterStream(file.name = file.name, track = track, timeout = timeout, oauth = my_oauth)
    
    if (length(twitts) > 10) {
        twitts <- twitts %>% 
            parseTweets %>% 
            filter(lang == 'en') %>% 
            select(text, created_at, screen_name)
        
        vapply(word(candidates$Name, 2), function(x) {
            twitts[, x] <<- str_detect(tolower(str_replace_all(twitts$text, '[^[:graph:]]', ' ')), tolower(x))
        }, logical(nrow(twitts)))
        
        twitts <- twitts %>% 
            rowwise %>%
            filter(sum(Sanders, Clinton, Trump) == 1) %>%
            mutate(score = as.numeric(score.sentiment(text, good_text, bad_text)),
                   sentiment = as.character(twitt.sentiment(text, good_text, bad_text))) %>% 
            ungroup %>%
            gather(candidate, mention, Trump, Clinton, Sanders) %>%
            filter(mention) %>%
            mutate(created_at = as.POSIXct(created_at, format = '%a %b %d %H:%M:%S'),
                   time = max(created_at)) %>%
            select(-mention)
    } else {
        twitts <- data.frame()
    }
    
    return(twitts)
}

test <- get.live.twitts()

# geom_lollipop

"%||%" <- function(a, b) {
    if (!is.null(a)) a else b
}

GeomLollipop <- ggproto("GeomLollipop", Geom,
                        required_aes = c("x", "y"),
                        non_missing_aes = c("size", "shape", "point.colour", "point.size", "horizontal"),
                        default_aes = aes(
                            shape = 19, colour = "black", size = 0.5, fill = NA,
                            alpha = NA, stroke = 0.5
                        ),
                        
                        setup_data = function(data, params) {
                            if (params$horizontal) {
                                transform(data, yend = y, xend = 0)
                            } else {
                                transform(data, xend = x, yend = 0)
                            }
                        },
                        
                        draw_group = function(data, panel_scales, coord,
                                              point.colour = NULL, point.size = NULL,
                                              horizontal = FALSE) {
                            
                            points <- data
                            points$colour <- point.colour %||% data$colour
                            points$size <- point.size %||% (data$size * 2.5)
                            
                            gList(
                                ggplot2::GeomSegment$draw_panel(data, panel_scales, coord),
                                ggplot2::GeomPoint$draw_panel(points, panel_scales, coord)
                            )
                            
                        },
                        
                        draw_key = draw_key_point
)

geom_lollipop <- function(mapping = NULL, data = NULL, ...,
                          horizontal = FALSE,
                          point.colour = NULL, point.size = NULL,
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
    
    layer(
        data = data,
        mapping = mapping,
        stat = "identity",
        geom = GeomLollipop,
        position = "identity",
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            horizontal = horizontal,
            point.colour = point.colour,
            point.size = point.size,
            ...
        )
    )
}



# aggregate sentiment plot
agg.sent <- function() {
    plot_df <- plot_df %>% 
        group_by(candidate) %>%
        summarise(count = n(),
                  score = mean(score)) %>%
        ungroup %>%
        mutate(yint = 0,
               score = normalize(score))
    
    g <- ggplot(data = plot_df, aes(x = score, y = yint)) +
        geom_hline(yintercept = 0) +
        geom_point() +
        annotation_custom(angry_face, xmin = -1.11, xmax = -1.09) +
        annotation_custom(happy_face, xmin = 1.09, xmax = 1.11) +
        annotation_custom(clinton_face_race,
                          xmin = plot_df$score[plot_df$candidate == 'Clinton'] - .01,
                          xmax = plot_df$score[plot_df$candidate == 'Clinton'] + .01) +
        annotation_custom(sanders_face_race,
                          xmin = plot_df$score[plot_df$candidate == 'Sanders'] - .01,
                          xmax = plot_df$score[plot_df$candidate == 'Sanders'] + .01) +
        annotation_custom(trump_face_race,
                          xmin = plot_df$score[plot_df$candidate == 'Trump'] - .01,
                          xmax = plot_df$score[plot_df$candidate == 'Trump'] + .01) +
        xlim(-1.069, 1.075) +
        theme_tufte() +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank())
    
    print(g)
}

# time series sentiment plot
ts.sent <- function() {
    
    plot_df <- plot_df %>% 
        group_by(candidate, time) %>%
        summarise(count = n(),
                  score = mean(score)) %>% 
        group_by(time) %>% 
        mutate(score = normalize(score)) %>% 
        ungroup %>%
        mutate(yint = 0)
    
    # need two 
    invisible(
        sapply(unique(plot_df$candidate), function(x) {
            plot_df$score[plot_df$candidate == x] <<- predict(loess(score~as.numeric(time), plot_df[plot_df$candidate == x, ]),
                                                               plot_df$time[plot_df$candidate == x])
        })
    )
    
    g <- ggplot(data = plot_df, aes(group = candidate, x = time, y = score, color = candidate)) +
        geom_line(size = 2) +
        scale_x_datetime(limits = c(min(plot_df$time), max(plot_df$time) + seconds(10))) +
        scale_y_continuous(limits = c(-1.2, 1.2), breaks = seq(-1, 1, by = .2), labels = round(seq(-1, 1, .2), 2)) +
        scale_color_manual(guide = F, values = c('Clinton' = '#232066', 'Sanders' = '#232066', 'Trump' = '#E91D0E')) +
        annotation_custom(clinton_face_ts,
                          ymin = plot_df$score[plot_df$time == max(plot_df$time[plot_df$candidate == 'Clinton']) & plot_df$candidate == 'Clinton'] - .2,
                          ymax = plot_df$score[plot_df$time == max(plot_df$time[plot_df$candidate == 'Clinton']) & plot_df$candidate == 'Clinton'] + .2,
                          xmin = as.numeric(max(plot_df$time[plot_df$candidate == 'Clinton'])) * .99,
                          xmax = as.numeric(max(plot_df$time[plot_df$candidate == 'Clinton'])) * 1.01) +
        annotation_custom(sanders_face_ts,
                          ymin = plot_df$score[plot_df$time == max(plot_df$time[plot_df$candidate == 'Sanders']) & plot_df$candidate == 'Sanders'] - .2,
                          ymax = plot_df$score[plot_df$time == max(plot_df$time[plot_df$candidate == 'Sanders']) & plot_df$candidate == 'Sanders'] + .2,
                          xmin = as.numeric(max(plot_df$time[plot_df$candidate == 'Sanders'])) * .99,
                          xmax = as.numeric(max(plot_df$time[plot_df$candidate == 'Sanders'])) * 1.01) +
        annotation_custom(trump_face_ts,
                          ymin = plot_df$score[plot_df$time == max(plot_df$time[plot_df$candidate == 'Trump']) & plot_df$candidate == 'Trump'] - .2,
                          ymax = plot_df$score[plot_df$time == max(plot_df$time[plot_df$candidate == 'Trump']) & plot_df$candidate == 'Trump'] + .2,
                          xmin = as.numeric(max(plot_df$time[plot_df$candidate == 'Trump'])) * .99,
                          xmax = as.numeric(max(plot_df$time[plot_df$candidate == 'Trump'])) * 1.01) +
        # annotation_custom(happy_face,
        #                   ymin = .9,
        #                   ymax = 1.1,
        #                   xmin = as.numeric(min(plot_df$time)) * .9,
        #                   xmax = as.numeric(min(plot_df$time)) * 1.1) +
        # annotation_custom(angry_face,
        #                   ymin = -1.1,
        #                   ymax = -.9,
        #                   xmin = as.numeric(min(plot_df$time)) * .9,
        #                   xmax = as.numeric(min(plot_df$time)) * 1.1) +
        theme_tufte() +
        labs(x = 'Time', y = 'Sentiment Score') +
        theme(axis.title.x = element_text())
    
    print(g)
    
}

# bar plot of tweet count
bar.plot <- function() {
    
    plot_df <- plot_df %>% 
        group_by(candidate) %>%
        summarise(count = n()) %>% 
        ungroup %>% 
        mutate(yint = 0)
    
    g <- ggplot(data = plot_df, aes(x = candidate, y = count, color = candidate)) +
        scale_color_manual(guide = F, values = c('Clinton' = '#232066', 'Sanders' = '#232066', 'Trump' = '#E91D0E')) +
        geom_lollipop(size = 2) +
        theme_tufte() +
        annotation_custom(clinton_face_ts, xmin = .7, xmax = 1.3,
                          ymin = plot_df$count[plot_df$candidate == 'Clinton'] * .9,
                          ymax = plot_df$count[plot_df$candidate == 'Clinton'] * 1.1) +
        annotation_custom(sanders_face_ts, xmin = 1.7, xmax = 2.3,
                          ymin = plot_df$count[plot_df$candidate == 'Sanders'] * .9,
                          ymax = plot_df$count[plot_df$candidate == 'Sanders'] * 1.1) +
        annotation_custom(trump_face_ts, xmin = 2.7, xmax = 3.3,
                          ymin = plot_df$count[plot_df$candidate == 'Trump'] * .9,
                          ymax = plot_df$count[plot_df$candidate == 'Trump'] * 1.1) +
        ylab('Number of Tweets') +
        ylim(0, max(plot_df$count) * 1.1) +
        coord_flip() +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank())
    
    print(g)
}

# raw twitts
raw.twitts <- function(sentiment) {
    
    text_df <- plot_df %>% 
        group_by(candidate) %>% 
        filter(time == max(time))
        
    if (sentiment == 'positive') {
        text_df <- text_df %>% 
            filter(score == max(score),
                   score > 0)
    } else {
        text_df <- text_df %>% 
            filter(score == min(score),
                   score < 0)
    }
    
    text_df <- text_df %>% 
        slice(1) %>% 
        ungroup %>%
        select(candidate, time, text, screen_name)
    
    HTML(
        paste0('<b>', 'Clinton', ': </b>', text_df$text[text_df$candidate == 'Clinton'], '<br>',
        '<b>', 'Trump', ': </b>', text_df$text[text_df$candidate == 'Trump'], '<br>',
        '<b>', 'Sanders', ': </b>', text_df$text[text_df$candidate == 'Sanders'])
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

my_sent <- sentiments %>%
filter(lexicon == 'bing')

pos <- my_sent$word[my_sent$sentiment == 'positive']
neg <- my_sent$word[my_sent$sentiment == 'negative']

twitts_test <- filterStream(file.name = '', track = candidates$username, timeout = 5, oauth = my_oauth) %>%
    parseTweets

bing <- sentiments %>%
    filter(lexicon == 'bing') %>%
    select(word, sentiment)

make.sentiment <- function(positive, negative) {
    if (positive > negative) {
        sent <- 'positive'
    } else if (positive < negative) {
        sent <- 'negative'
    } else if (positive == negative) {
        sent <- 'neutral'
    }
    return(sent)
}

sent_test <- twitts_test %>%
    filter(lang == 'en') %>% 
    select(text, created_at, id_str)
invisible(vapply(word(candidates$Name, 2), function(x) {
    sent_test[, x] <<- str_detect(tolower(str_replace_all(sent_test$text, '[^[:graph:]]', ' ')), tolower(x))
}, logical(nrow(sent_test))))
sent_test <- sent_test %>%
    rowwise %>% 
    filter(sum(Sanders, Clinton, Trump) == 1) %>% 
    ungroup %>% 
    gather(candidate, check, c(Sanders, Clinton, Trump)) %>% 
    filter(check == T) %>% 
    select(-check) %>% 
    mutate(text = str_replace_all(text, '[[:punct:]]|[[:cntrl:]]|\\d+', ''),
           text = iconv(text, 'UTF-8', 'ASCII')) %>%
    unnest_tokens(word, text) %>%
    filter(!is.na(candidate)) %>% 
    anti_join(stop_words, by = 'word') %>%
    left_join(bing, by = 'word') %>%
    mutate(sentiment = ifelse(is.na(sentiment), 'neutral', sentiment)) %>% 
    count(id_str, candidate, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = make.sentiment(positive, negative)) %>% 
    group_by(candidate) %>% 
    summarise(pct_pos = mean(sentiment == 'positive'))
