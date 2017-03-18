shinyServer(function(input, output, session) {
    
    output$select_artist_ui <- renderUI({
        artist_info <<- get_artists(input$artist)
        selectInput('select_artist', 'Choose an Artist', choices = artist_info$artist_name)
    })
    
    observeEvent(input$select_artist, {
        
        artist_name <<- input$select_artist
        
        if (nrow(artist_info) > 0) {
            album_info <<- get_albums(artist_info$artist_uri[artist_info$artist_name == artist_name])
              
            output$album_go_ui <- renderUI({
                actionButton('album_go', 'Get track info')
            })
        }
    })
    
    observeEvent(input$album_go, {
        
        track_info <<- get_tracks(artist_info[artist_info$artist_name == artist_name, ], album_info)
        
        output$albums_ui <- renderUI({
            selectizeInput('albums', 'Choose which albums to include', 
                           choices = unique(track_info$album_name), 
                           selected = unique(album_info$album_name), multiple = T)
        })
    })
    
    observe({
        
        test <<- input$albums
        
        if (exists('track_info')) {
            
            track_info_sub <<- track_info %>% 
                filter(album_name %in% input$albums) %>% 
                mutate(album_rank = dense_rank(album_rank))
            
            output$quadrant_chart <- renderHighchart({
                quadrant_chart(track_info_sub)
            })
            
            # output$track_tbl <- DT::renderDataTable({
            #     table_df <- track_info_sub %>% 
            #         select(album_name, track_name, track_number, album_release_date, album_release_year, everything())
            #     datatable(table_df, rownames = F, escape = F,
            #               options = list(scrollX = T, info = F, ordering = F, processing = F))
            # })
            
            # output$album_feature_chart <- renderHighchart({
            #     album_feature_chart(track_info_sub, input$feature_var)
            # })
            
            
            # output$album_prog_chart <- renderPlotly({
            #     
            #     test <- ggplot(aes(x = track_number, y = valence, text = paste('track_name:', track_name, '<br>new tooltip:test'), group = album_name), data = track_info_sub) +
            #         geom_line() +
            #         facet_wrap(~album_name) +
            #         ggtitle(label = 'Radiohead Sentiment by Album') +
            #         theme_tufte()
            #     pdf(NULL)
            #     ggplotly(test, tooltip = c('x', 'y', 'text'))
            # })
        }
    })
    
})