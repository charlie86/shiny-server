shinyServer(function(input, output, session) {
    
    sesh_start <- Sys.time()
    
    test <<- session$clientData
    
    # twitts <<- dbGetQuery(mydb, "SELECT * FROM twitts WHERE TIMESTAMPDIFF(minute, created_at, NOW()) <= 5")
    
    twitts <<- cQuery('SELECT *
                     FROM twitts
                     WHERE TIMESTAMPDIFF(SECOND, TIMESTAMP(created_at), DATE_SUB(NOW(), INTERVAL 4 HOUR)) <= 10')
    
    # Hide the loading message when the rest of the server function has executed
    # hide(id = 'loading-content', anim = T, animType = 'fade')
    
    # set reactive timer that retrieves new tweets every 5 seconds
    observe({
        autoInvalidate <<- reactiveTimer(5000)
    })
    
    get_input <- reactive({
        autoInvalidate()
            twitts <<- cQuery(paste0('SELECT * 
                                     FROM twitts 
                                     WHERE TIMESTAMPDIFF(MINUTE, TIMESTAMP(created_at), DATE_SUB(NOW(), INTERVAL 4 HOUR)) <= ', 
                                     input$twitt_timeframe))
            
            m <- mongo('twitter_db', collection = 'twitter_collection')
            
            return(m)
    })
    
    # observe({
        # plot_df <<- get_input()
        
        # output$agg_sent <- renderPlot({
        #     agg.sent(time = names(timeframes[timeframes == input$twitt_timeframe]))
        # })
        # 
        # # raw tweets
        # output$pos_twitts <- renderText({
        #     raw.twitts('positive')
        # })
        # output$neg_twitts <- renderText({
        #     raw.twitts('negative')
        # })        
    # })
    
    # output$my_map <- renderLeaflet({            
    #     leaflet(twitts %>% filter(!is.na(place_lat))) %>% 
    #         addTiles() %>% 
    #         addMarkers(lng = ~place_lon, lat = ~place_lat, popup = ~text)
    # })
    # 
    # observe({
    #     leafletProxy('my_map', data = get_input() %>% filter(!is.na(place_lat))) %>%
    #         clearMarkers() %>%
    #         addMarkers(lng = ~place_lon, lat = ~place_lat, popup = ~text)
    # })
    
})


