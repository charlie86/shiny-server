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
                selectInput('select_artist', 'Choose an artist from these matches', choices = artist_info$artist_name)
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
        
        output$artist_quadrant_chart <- renderHighchart({
            artist_quadrant_chart(track_info)
        })
        
        output$artist_plot <- renderUI({
            
            if (input$GetScreenWidth >= 800) {
                highchartOutput('artist_quadrant_chart', width = '775px', height='700px')
            } else {
                highchartOutput('artist_quadrant_chart')
            }
        })
    })
    
    
    
    observe({
        output$select_user_ui <- renderUI({
            if (input$user_selector == 'Select user from list') {
                selectInput('user', 'Choose a User', famous_users)
            } else {
                textInput('user', 'User', placeholder = 'e.g. barackobama')
            }
        })
    })
    
    
    observeEvent(input$user_go, {
        user <<- str_replace(input$user, 'spotify:user:', '')
        user_info <<- GET(paste0(base_url, 'users/', user), query = list(access_token = access_token)) %>% content
        
        if (is.null(user_info$error)) {
            user_img <<- ifelse(length(user_info$images) > 0, user_info$images[[1]]$url, 'https://pbs.twimg.com/profile_images/509949472139669504/IQSh7By1_400x400.jpeg')
            
            output$user <- renderText({
                HTML(paste0('<img src=', user_img, ' height="200">', '<br/>', user_info$display_name))
            })
            
            playlists <<- get_user_playlists(user)
            output$select_playlist_ui <<- renderUI({
                tagList(
                    selectInput('playlist_selector', 'Choose playlists to include', choices = playlists$playlist_name, selected = playlists$playlist_name, multiple = T),
                    actionButton('playlist_go', 'Get tracks')
                )
            })
        }
    })
    
    observeEvent(input$playlist_go, {
        
        playlist_tracks <- get_playlist_tracks(playlists)
        
        playlist_track_audio_features <- get_track_audio_features(playlist_tracks)
        
        track_df <- playlist_tracks %>%
            filter(playlist_name %in% input$playlist_selector) %>% 
            left_join(playlist_track_audio_features, by = 'track_uri') %>%
            mutate_at(c('playlist_name', 'playlist_img', 'track_name', 'track_uri', 'artist_name', 'album_name', 'album_img'), funs(as.character)) %>%
            mutate_at(c('danceability', 'energy', 'key', 'loudness', 'mode', 'speechiness', 'acousticness',
                        'instrumentalness', 'liveness', 'valence', 'tempo', 'duration_ms', 'time_signature'), funs(as.numeric(gsub('[^0-9.-]+', '', as.character(.)))))
        
        output$playlist_quadrant_chart <- renderHighchart({
            playlist_quadrant_chart(track_df)
        })
        
        output$playlist_plot <- renderUI({
            
            if (input$GetScreenWidth >= 800) {
                highchartOutput('playlist_quadrant_chart', width = '775px', height='700px')
            } else {
                highchartOutput('playlist_quadrant_chart')
            }
        })
    })
    
})