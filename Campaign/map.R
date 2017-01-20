library(chuckr)
library(httr)

twitts <- cQuery('SELECT * FROM twitts WHERE TIMESTAMPDIFF(MINUTE, TIMESTAMP(created_at), DATE_SUB(NOW(), INTERVAL 4 HOUR)) <= 5')

coords <- twitts %>% 
    filter(!is.na(place_lat)) %>% 
    select(place_lat, place_lon) 



test <- do.call(rbind, lapply(seq_len(nrow(coords)), function(x) {

    lat <- coords$place_lat[x]
    lon <- coords$place_lon[x]
    
    print(x)
    
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
    
    return(geos)
    
}))

states_og$count <- ploter$n[match(states_og$NAME, ploter$State.name)]
states_og$count <- ifelse(is.na(states_og$count), 0, states_og$count)
states_og$popup <- vapply(seq_len(nrow(states_og)), function(x) {
    paste0('<h3><u>', states_og$NAME[x], '</u></h3>',
           '<p>', states_og$count[x], ' twitts</p>')
}, character(1))

pal_states <- colorBin(palette = 'Reds', bins = c(0, 1, 5, 10, 20, 40, 50), domain = states_og$count)

ploter <- test %>% 
    count(State.name)

leaflet() %>% 
    addTiles() %>% 
    addMarkers(coords$place_lon, coords$place_lat, group = 'twitts') %>% 
    addPolygons(data = states_og, color = 'black', weight = 1, fillColor = ~pal_states(count),
            fillOpacity = 1, group = 'states', layerId = ~NAME, popup = ~popup) %>%
    addLayersControl(overlayGroups = 'twitts', options = layersControlOptions(collapsed = F))

test_map <- twitts %>% 
    filter(lang == 'en') %>%
    select(text, created_at, id_str)
invisible(vapply(word(candidates$Name, 2), function(x) {
    test_map[, x] <<- str_detect(tolower(str_replace_all(test_map$text, '[^[:graph:]]', ' ')), tolower(x))
}, logical(nrow(test_map))))
test_map <- test_map %>%
    rowwise %>%
    filter(sum(Clinton, Trump) == 1) %>%
    ungroup %>%
    gather(candidate, mention, c(Clinton, Trump)) %>%
    filter(mention == T) %>%
    select(-mention) %>%
    mutate(created_at = as.POSIXct(created_at, format = '%a %b %d %H:%M:%S'),
           time = max(created_at))