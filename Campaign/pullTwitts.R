library(dplyr)
library(stringr)
library(lubridate)
library(streamR)
library(sp)
library(tidyr)
library(tidytext)
library(httr)
library(RMySQL)

chuck <- 'C:/Users/B1GRU/Documents/R/Fun/Proyectos/Campaign'
chuck_vb <- 'C:/Users/Charlie/Documents/Side Projects/midnight barber/shiny-server/Campaign'
aws <- '/srv/shiny-server/Campaign/'

candidates <- data.frame(Name = c('Hillary Clinton', 'Donald Trump')) %>% 
    mutate(username = str_replace(Name, ' ', ''),
           username = ifelse(username == 'DonaldTrump', paste0('real', username), username))

chuckDB <- dbConnect(MySQL(),
                     user = 'chuck',
                     password = 'charlie86',
                     dbname = 'tweetdb',
                     host = 'tweetdb.ch74fm7hgclb.us-west-2.rds.amazonaws.com')

if (dir.exists(aws)) {
    setwd(aws)
} else if (dir.exists(chuck)) {
    setwd(chuck)
} else if (dir.exists(chuck_vb)) {
    setwd(chuck_vb)
} else stop('Donde estas?')

load('data/my_oauth.RData')

twitts <- filterStream(file.name = '', track = candidates$username, timeout = 5, oauth = my_oauth)

if (length(twitts) > 0) {
    
    # format candidate names
    candidates <- data.frame(Name = c('Hillary Clinton', 'Donald Trump')) %>% 
        mutate(username = str_replace(Name, ' ', ''),
               username = ifelse(username == 'DonaldTrump', paste0('real', username), username)) #%>% 
    
    ### Sentiment data and functions
    # clinton_pos <- data.frame(candidate = 'Clinton', word = c('imwithher', 'votehillary'), sentiment = 'positive')
    # clinton_neg <- data.frame(candidate = 'Clinton', word = c('nohillary', 'hillno', 'donewithher', 'hilliery', 'neverhillary', 'hillaryforprison2016'), sentiment = 'negative')
    # 
    # trump_pos <- data.frame(candidate = 'Trump', word = c('makemaericagreatagain', 'teamtrump', 'trumptrain', 'americafirst'), sentiment = 'positive')
    # trump_neg <- data.frame(candidate = 'Trump', word = c('nevertrump'), sentiment = 'negative')
    
    # candidate_words <- rbind(clinton_pos, clinton_neg, trump_pos, trump_neg)
    # candidate_words <- candidate_words %>% 
    #     rbind(candidate_words %>% 
    #               mutate(candidate = ifelse(candidate == 'Trump', 'Clinton', 'Trump'),
    #                      sentiment = ifelse(sentiment == 'positive', 'negative', 'positive'))) %>% 
    #     mutate_each(funs(as.character(.)))
    
    bing <- sentiments %>%
        filter(lexicon == 'bing') %>%
        select(word, sentiment)
    
    nrc <- sentiments %>% 
        filter(lexicon == 'nrc') %>% 
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
    
    twitts <- parseTweets(twitts) %>%
        mutate(created_at = as.POSIXct(created_at, format = '%a %b %d %T +0000 %Y') - hours(4),
               rownum = row_number())
    
    coords <- twitts %>% 
        filter(!is.na(place_lat))
    
    geocodes <- do.call(rbind, lapply(seq_len(nrow(coords)), function(x) {
        
        lat <- coords$place_lat[x]
        lon <- coords$place_lon[x]
        
        geos <- GET(paste0('http://data.fcc.gov/api/block/2010/find?latitude=', lat, '&longitude=', lon, '&format=json')) %>% 
            content %>% 
            unlist %>% 
            t %>% 
            as.data.frame
        
        if (is.null(geos$Block.FIPS)) {
            df <- data.frame(Block.FIPS = NA,
                             County.FIPS = NA,
                             County.name = NA,
                             State.FIPS = NA,
                             State.code = NA,
                             State.name = NA, stringsAsFactors = F)
            
            geos <- cbind(df, geos)
        }
        
        return(geos %>% mutate(rownum = coords$rownum[x]))
        
    }))
    
    
    sent <- twitts %>%
        filter(lang == 'en') %>%
        select(text, created_at, id_str)
    invisible(vapply(word(candidates$Name, 2), function(x) {
        sent[, x] <<- str_detect(tolower(str_replace_all(sent$text, '[^[:graph:]]', ' ')), tolower(x))
    }, logical(nrow(sent))))
    sent <- sent %>%
        rowwise %>%
        filter(sum(Clinton, Trump) == 1) %>%
        ungroup %>%
        gather(candidate, mention, c(Clinton, Trump)) %>%
        filter(mention == T) %>%
        select(-mention) %>%
        mutate(time = max(created_at),
               text = str_replace_all(text, '[[:punct:]]|[[:cntrl:]]|\\d+', ''),
               text = iconv(text, 'UTF-8', 'ASCII')) %>%
        unnest_tokens(word, text) %>%
        filter(!is.na(candidate)) %>%
        anti_join(stop_words, by = 'word') %>%
        left_join(bing, by = 'word') %>%
        mutate(sentiment = ifelse(is.na(sentiment), 'neutral', sentiment)) %>%
        count(id_str, candidate, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = make.sentiment(positive, negative)) %>% 
        ungroup
    
    twitts <- twitts %>% 
        left_join(geocodes, by = 'rownum') %>% 
        left_join(sent, by = 'id_str')
    
    dbWriteTable(conn = chuckDB, name = 'twitts', value = twitts, append = T, overwrite = F)
}