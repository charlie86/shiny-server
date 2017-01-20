

test <- stuff %>% 
    dplyr::filter(as.Date(time) <= '2016-07-29',
           candidate == 'Clinton') %>% 
    select(-candidate)

head(test)

test %>% ts


library(timeSeries)
library(xts)

test2 <- xts(test$score, order.by = as.POSIXct(test$time))
attr(test2, 'frequency') <- 24

test3 <- as.ts(test2) %>% stl('per')

plot(test3)

test4 <- as.data.frame(test3$time.series) 
test4$hour <- test$time
test4$score <- test$score

test4 <- test4 %>% select(hour, score, everything()) %>% 
    gather(metrics, value, -hour)

# test5 <- xts(test4[, 2:5],  order.by = as.POSIXct(test4$hour))
# attr(test5, 'frequency') <- 24
# test6 <- ts(test5)
# hchart(test6)

hchart(x = hour, y = value, group = metrics, type = 'spline', object = test4)

hchart(test4)

%>% stl('per') 

%>% hchart

test3 <- stl(test2, 'per')

tester <- cQuery('SELECT 
       date(user_created_at),
       count(*)
       FROM twitts
       GROUP BY 1')

stuff <- cQuery("SELECT
        concat(date(created_at), ' ', hour(created_at), ':00') AS time,
        candidate,
        avg(positive - negative) AS score
        FROM twitts
        WHERE candidate IS NOT NULL
       GROUP BY 1,2")