library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(scales)
library(streamR)
library(tidyr)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(png)

setwd('C:/Users/B1GRU/Documents/R/Fun/Proyectos/Campaign')

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

load('data/my_oauth.RData')

good_text <- scan('data/positive-words.txt',
                  what = 'character', comment.char = ';')
bad_text <- scan('data/negative-words.txt',
                 what = 'character', comment.char = ';')
good_text <- c(good_text, 'upgrade', ':)', '#iVoted', 'voted')
bad_text <- c(bad_text, 'wtf', 'epicfail', 'douchebag')

# loop over tweets and extract text
candidates <- data.frame(Name = c('Bernie Sanders', 'Hillary Clinton', 'Donald Trump', 'Ted Cruz')) %>% 
    mutate(username = str_replace(Name, ' ', ''),
           username = ifelse(username == 'DonaldTrump', paste0('real', username), username)) %>% 
    filter(Name %in% c('Donald Trump', 'Hillary Clinton'))

get.live.twitts <- function(file.name = '', track = candidates$username, timeout = 5, oauth = my_oauth) {
    twitts <- filterStream(file.name = file.name, track = track, timeout = timeout, oauth = my_oauth)
    
    if (length(twitts) > 0) {
        twitts <- twitts %>% 
            parseTweets %>% 
            filter(lang == 'en') %>% 
            select(text, created_at)
        
        vapply(word(candidates$Name, 2), function(x) {
            twitts[, x] <<- str_detect(twitts$text, x)
        }, logical(nrow(twitts)))
        
        twitts <- twitts %>% 
            rowwise %>%
            # filter(sum(Sanders, Clinton, Trump, Cruz) == 1) %>%
            filter(sum(Trump, Clinton) == 1) %>%
            mutate(score = as.numeric(score.sentiment(text, good_text, bad_text))) %>% 
            ungroup %>%
            gather(candidate, mention, Trump, Clinton) %>%
            filter(mention) %>%
            mutate(created_at = as.POSIXct(created_at, format = '%a %b %d %H:%M:%S'),
                   time = max(created_at)) %>%
            select(-mention)
    } else {
        twitts <- data.frame()
    }
    
    return(twitts)
}

clinton_face <- readPNG('data/pics/clinton.png') 
sanders_face <- readPNG('data/pics/sanders.png')
trump_face <- readPNG('data/pics/trump.png') 
cruz_face <- readPNG('data/pics/cruz.png')

clinton_face_ts <- clinton_face %>% rasterGrob()
sanders_face_ts <- sanders_face %>% rasterGrob()
trump_face_ts <- trump_face %>% rasterGrob()
cruz_face_ts <- cruz_face %>% rasterGrob()

clinton_face_race <- clinton_face %>% rasterGrob(width = 10)
sanders_face_race <- sanders_face %>% rasterGrob(width = 10)
trump_face_race <- trump_face %>% rasterGrob(width = 12)
cruz_face_race <- cruz_face %>% rasterGrob(width = 12)


happy_face <- readPNG('data/pics/happy.png') %>% rasterGrob(width = 10)
angry_face <- readPNG('data/pics/angry.png') %>% rasterGrob(width = 10)

smiley.race <- function() {
    plot_df <- plot_df %>% 
        group_by(candidate) %>%
        summarise(count = n(),
                  score = mean(score)) %>% 
        ungroup %>% 
        mutate(yint = 0)
    
    g <- ggplot(data = plot_df, aes(x = score, y = yint)) +
        geom_hline(yintercept = 0) +
        geom_point() +
        annotation_custom(angry_face, xmin = -1.11, xmax = -1.09) +
        annotation_custom(happy_face, xmin = 1.09, xmax = 1.11) +
        annotation_custom(clinton_face_race,
                          xmin = plot_df$score[plot_df$candidate == 'Clinton'] - .01,
                          xmax = plot_df$score[plot_df$candidate == 'Clinton'] + .01) +
        # annotation_custom(cruz_face_race,
        #                   xmin = plot_df$score[plot_df$candidate == 'Cruz'] - .01,
        #                   xmax = plot_df$score[plot_df$candidate == 'Cruz'] + .01) +
        # annotation_custom(sanders_face_race,
        #                   xmin = plot_df$score[plot_df$candidate == 'Sanders'] - .01,
        #                   xmax = plot_df$score[plot_df$candidate == 'Sanders'] + .01) +
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

ts.face <- function() {
    
    plot_df <<- plot_df %>% 
        group_by(candidate, time) %>%
        summarise(count = n(),
                  score = mean(score)) %>% 
        ungroup %>% 
        mutate(yint = 0)
    
    # need two 
    invisible(
        sapply(unique(plot_df$candidate), function(x) {
            plot_df$smooth[plot_df$candidate == x] <<- predict(loess(score~as.numeric(time), plot_df[plot_df$candidate == x, ]),
                                                               plot_df$time[plot_df$candidate == x])
        })
    )
    
    g <- ggplot(data = plot_df, aes(group = candidate, x = time, y = smooth, color = candidate)) +
        geom_line(size = 2) +
        scale_x_datetime(limits = c(min(plot_df$time), max(plot_df$time) + seconds(10))) +
        scale_color_discrete(guide = F) +
        annotation_custom(clinton_face_ts,
                          ymin = plot_df$smooth[plot_df$time == max(plot_df$time[plot_df$candidate == 'Clinton']) & plot_df$candidate == 'Clinton'] - .2,
                          ymax = plot_df$smooth[plot_df$time == max(plot_df$time[plot_df$candidate == 'Clinton']) & plot_df$candidate == 'Clinton'] + .2,
                          xmin = as.numeric(max(plot_df$time[plot_df$candidate == 'Clinton'])) * .99,
                          xmax = as.numeric(max(plot_df$time[plot_df$candidate == 'Clinton'])) * 1.01) +
        # annotation_custom(sanders_face_ts,
        #                   ymin = plot_df$smooth[plot_df$time == max(plot_df$time[plot_df$candidate == 'Sanders']) & plot_df$candidate == 'Sanders'] - .2,
        #                   ymax = plot_df$smooth[plot_df$time == max(plot_df$time[plot_df$candidate == 'Sanders']) & plot_df$candidate == 'Sanders'] + .2,
        #                   xmin = as.numeric(max(plot_df$time[plot_df$candidate == 'Sanders'])) * .99,
        #                   xmax = as.numeric(max(plot_df$time[plot_df$candidate == 'Sanders'])) * 1.01) +
        annotation_custom(trump_face_ts,
                          ymin = plot_df$smooth[plot_df$time == max(plot_df$time[plot_df$candidate == 'Trump']) & plot_df$candidate == 'Trump'] - .2,
                          ymax = plot_df$smooth[plot_df$time == max(plot_df$time[plot_df$candidate == 'Trump']) & plot_df$candidate == 'Trump'] + .2,
                          xmin = as.numeric(max(plot_df$time[plot_df$candidate == 'Trump'])) * .99,
                          xmax = as.numeric(max(plot_df$time[plot_df$candidate == 'Trump'])) * 1.01) +
        # annotation_custom(cruz_face_ts,
        #                   ymin = plot_df$smooth[plot_df$time == max(plot_df$time[plot_df$candidate == 'Cruz']) & plot_df$candidate == 'Cruz'] - .2,
        #                   ymax = plot_df$smooth[plot_df$time == max(plot_df$time[plot_df$candidate == 'Cruz']) & plot_df$candidate == 'Cruz'] + .2,
        #                   xmin = as.numeric(max(plot_df$time[plot_df$candidate == 'Cruz'])) * .99,
        #                   xmax = as.numeric(max(plot_df$time[plot_df$candidate == 'Cruz'])) * 1.01) +
        theme_tufte()
    
    print(g)
}

bar.plot <- function() {
    
    plot_df <- plot_df %>% 
        group_by(candidate) %>%
        summarise(count = n(),
                  score = mean(score)) %>% 
        ungroup %>% 
        mutate(yint = 0)
    
    g <- ggplot(data = plot_df, aes(x = candidate, y = count, fill = score)) +
        geom_bar(stat = 'identity') +
        scale_fill_gradient2(low = 'red', mid = 'yellow', high = 'green', breaks = c(-1, 1), labels = c('Negative', 'Positive'),
                             limits = c(-1, 1)) +
        theme_tufte() +
        annotation_custom(clinton_face_ts, xmin = .9, xmax = 1.1,
                          ymin = plot_df$count[plot_df$candidate == 'Clinton'] * .9,
                          ymax = plot_df$count[plot_df$candidate == 'Clinton'] * 1.1) +
        # annotation_custom(cruz_face_ts, xmin = 1.9, xmax = 2.1,
        #                   ymin = plot_df$count[plot_df$candidate == 'Cruz'],
        #                   ymax = plot_df$count[plot_df$candidate == 'Cruz']) +
        # annotation_custom(sanders_face_ts, xmin = 2.9, xmax = 3.1,
        #                   ymin = plot_df$count[plot_df$candidate == 'Sanders'],
        #                   ymax = plot_df$count[plot_df$candidate == 'Sanders']) +
        annotation_custom(trump_face_ts, xmin = 1.9, xmax = 2.1,
                          ymin = plot_df$count[plot_df$candidate == 'Trump'] * .9,
                          ymax = plot_df$count[plot_df$candidate == 'Trump'] * 1.1) +
        ylab('Number of Tweets') +
        ylim(0, max(plot_df$count) * 1.1) +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.x = element_blank())
    
    print(g)
}

x <- 1
df <- get.live.twitts() %>% 
    rbind(get.live.twitts(),
          get.live.twitts())
plot_df <- data.frame()

sapply(1:10000000, function(x) {
    twitts <- get.live.twitts()
    
    if (length(twitts) > 0) {
        
        df <<- rbind(df, twitts)
        
        plot_df <<- df %>% 
            rbind(twitts)
        
        ts.face()
        # smiley.race()
        # bar.plot()
        
    }
    x <- x + 1
})


