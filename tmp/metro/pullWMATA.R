library(httr)
library(dplyr)
library(RMySQL)

metrics <- c('incidents' = 'https://api.wmata.com/Incidents.svc/json/Incidents',
             'predictions' = 'https://api.wmata.com/StationPrediction.svc/json/GetPrediction/All',
             'line_info' = 'https://api.wmata.com/Rail.svc/json/jLines',
             'stations' = 'https://api.wmata.com/Rail.svc/json/jStations')

chuckDB <- dbConnect(MySQL(), 
                  user = 'chuckteezy',
                  password = 'charlie86',
                  dbname = 'chuckDB',
                  host = 'chuckdb.ch74fm7hgclb.us-west-2.rds.amazonaws.com')

dbListTables(chuckDB)

pull <- function(url) {
    
    query <- content(GET(url, query = list(api_key = 'c69abd486c4343879f5e73f9272008dd')))[[1]]
    
    my_time <- Sys.time()
    
    do.call(rbind, lapply(seq_len(length(query)), function(x) {
        tmp <- query[[x]]
        lapply(names(tmp), function(y) {
            if (is.null(tmp[[y]])) {
                tmp[[y]] <<- NA
            }
        }) %>% invisible
        tmp %>% as.data.frame() %>% 
            mutate(time = my_time)
    }))
}

lapply(metrics, function(x) {
    pull(x) %>% dbWriteTable(chuckDB, names(metrics[metrics == x]), ., append = T, overwrite = F)
}) %>% invisible
