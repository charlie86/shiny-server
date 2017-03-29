shinyServer(function(input, output, session) {
    
    observe({
        output$screenwidth <- renderText({
            input$GetScreenWidth
        })
    })
    
    observe({
        if (nchar(input$artist_search) > 0) {
            output$select_artist_ui <- renderUI({
                artist_info <<- get_artists(input$artist_search)
                selectInput('select_artist', 'Choose an Artist', choices = artist_info$artist_name)
            })
        }
    })
    
    observeEvent(input$select_artist, {
        
        ### "searching for artists..."
        artist_name <<- input$select_artist
        
        if (nrow(artist_info) > 0) {
            
            output$album_go_ui <- renderUI({
                actionButton('album_go', 'Get album info')
            })
        }
    })
    
    observeEvent(input$album_go, {
        
        ### "searching for albums..."
        album_info <<- get_albums(artist_info$artist_uri[artist_info$artist_name == artist_name])
        
        print(head(album_info))
        
        if (nrow(album_info) > 0) {
            
            output$albums_ui <- renderUI({
                tagList(
                    selectizeInput('albums', 'Choose which albums to include', 
                                   choices = unique(album_info$album_name), 
                                   selected = unique(album_info$album_name), multiple = T),
                    actionButton('tracks_go', 'Get track info')
                )
            })
        }
    })
    
    observeEvent(input$tracks_go, {
        ### "loading all tracks from selected albums..."
        
        track_info <<- get_tracks(artist_info[artist_info$artist_name == artist_name, ], album_info[album_info$album_name %in% input$albums, ])
        
        output$quadrant_chart <- renderHighchart({
            quadrant_chart(track_info)
        })
        
        output$plot <- renderUI({
            
            if (input$GetScreenWidth >= 800) {
                highchartOutput('quadrant_chart', width = '775px', height='700px')
            } else {
                highchartOutput('quadrant_chart')
            }
        })
    })
    
})