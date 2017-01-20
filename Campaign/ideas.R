library(RMySQL)
library(twitteR)

m<-dbDriver("MySQL") # Load the driver
con<-dbConnect(m,user='chuckteezy',
               password='charlie86',
               host='mydb.ch74fm7hgclb.us-west-2.rds.amazonaws.com',
               dbname = 'testdb2',
               port = 3306) # Connect to the local instance
res<-dbSendQuery(con, 'select * from mtcars') # Execute the query
data <- fetch(res, n = -1) # Load the retrieved data into a data frame
dbClearResult(dbListResults(con)[[1]]) # Use this to free the connection
dbListTables(con)

dbWriteTable(conn = con, name = '')

consumer_key <- 'EB8kQdOppCzSMvhvMY233ZF9l'
consumer_secret <- 'QHeuYrMnEL8I63RyQeC0pJoBzNjiNoKeNBYKSBwPEg4deiSbcO'
access_token <- '706589608411209729-r0IRzoMhd75x3hbZ2Wj2X1UfxT6C5RC'
access_secret <- '9mVMpoKmfnMXWGoucOtiPDp1mJkeodzDrewTtWRDsFgUQ'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
hildawg <- searchTwitter('@HillaryClinton', since = '2015-04-10', n = 10)

head(plot_df)

twitts <- filterStream(file.name = '', track = '@realDonaldTrump', timeout = 60, oauth = my_oauth)
test <- parseTweets(twitts) %>% 
    filter(!is.na(place_lat) | !is.na(lat))

#get states
#first try place_lon and place_lat
s1 = get_states(test$place_lon, test$place_lat)

#then try lon and lat
#replace na lon and lat to 0 so that the program won't break
#0 (lon, lat) pair will return na in the end
lon = ifelse(is.na(test$lon), 0, test$lon)
lat = ifelse(is.na(test$lat), 0, test$lat)
s2 = get_states(lon, lat)

#combine
test$state = ifelse(is.na(s1), s2, s1)
test = subset(test, !is.na(state))
