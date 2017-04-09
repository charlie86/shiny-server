shinyServer(function(input, output, session) {
    
    observe({
        output$screenwidth <- renderText({
            input$GetScreenWidth
        })
    })
    
    observe({
        output$select_artist_ui <- renderUI({
            artist_info <<- get_artists(input$artist_search)
            selectInput('select_artist', 'Choose an artist from these matches on Spotify', choices = artist_info$artist_name)
        })
    })
    
    observeEvent(input$select_artist, {
        
        ### "searching for artists..."
        artist_name <<- input$select_artist
        output$album_go_ui <- renderUI({
            withBusyIndicatorUI(
                actionButton('album_go', 'Get albums', class = 'btn-primary')
            )
        })
    })
    
    observeEvent(input$album_go, {
        
        withBusyIndicatorServer('album_go', {
            
            if (nchar(input$select_artist) == 0) {
                stop("Enter an artist name")
            }
            
            ### "searching for albums..."
            album_info <<- get_albums(artist_info$artist_uri[artist_info$artist_name == artist_name])
            
            if (nrow(album_info) > 0) {
                
                output$albums_ui <- renderUI({
                    tagList(
                        selectizeInput('albums', 'Choose which albums to include', 
                                       choices = unique(album_info$album_name), 
                                       selected = unique(album_info$album_name), multiple = T),
                        withBusyIndicatorUI(
                            actionButton('tracks_go', 'Get tracks and generate plot', class = 'btn-primary')
                        )
                    )
                })
            } else {
                stop("Sorry, couldn't find any albums for that artist on Spotify.")
            }
        })
    })
    
    observeEvent(input$tracks_go, {
        
        withBusyIndicatorServer('tracks_go', {
            
            artist_tracks <<- get_album_tracks(album_info)
            
            artist_track_audio_features <<- get_track_audio_features(artist_tracks[artist_tracks$album_name %in% input$albums, ])
            
            track_info <<- artist_tracks %>%
                left_join(artist_track_audio_features, by = 'track_uri')
            
            if (nrow(track_info) == 0) {
                stop("Sorry, couldn't find any tracks for that artist's albums on Spotify.")
            }
            
            output$artist_quadrant_chart <- renderHighchart({
                artist_quadrant_chart(track_info)
            })
            
            # output$artist_profile_chart <- renderHighchart({
            #     sentiment_profile_chart(track_info, 'album_name')
            # })
            
            output$artist_plot <- renderUI({
                
                withProgress(message = 'Making plot', value = 0, {
                    
                    if (input$GetScreenWidth >= 800) {
                        incProgress(.9)
                        highchartOutput('artist_quadrant_chart', width = '820px', height = '700px')
                        # tabBox(
                        #     id = 'plots',
                        #     tabPanel('Sentiment Quadrants', highchartOutput('artist_quadrant_chart', width = '775px', height = '700px')),
                        #     tabPanel('Playlist Profile', highchartOutput('artist_profile_chart', width = '775px', height = '700px')),
                        #     width = 9
                        # )
                    } else {
                        incProgress(.9)
                        highchartOutput('artist_quadrant_chart')
                    }
                })
            })
        })
    })
    
    observeEvent(input$user_go, {
        withBusyIndicatorServer('user_go', {
            
            if (nchar(input$user) == 0) {
                stop('Type a user name or Spotify URI')
            }
            
            user <<- tolower(str_replace(input$user, 'spotify:user:', ''))
            user_info <<- GET(paste0(base_url, 'users/', user), query = list(access_token = access_token)) %>% content
            
            if (is.null(user_info$error)) {
                user_img <<- ifelse(length(user_info$images) > 0, user_info$images[[1]]$url, 'https://pbs.twimg.com/profile_images/509949472139669504/IQSh7By1_400x400.jpeg')
                
                if (user == 'barackobama') {
                    user_img <<- 'barry.jpg'
                }
                
                output$user <- renderText({
                    HTML(paste0('<img src=', user_img, ' height="200">', '<br/>', user_info$display_name))
                })
                
                playlists <<- get_user_playlists(user)
                
                if (nrow(playlists) == 0) {
                    stop("Sorry, that user doesn't have any playlists on Spotify.")
                }
                
                output$select_playlist_ui <<- renderUI({
                    tagList(
                        selectInput('playlist_selector', 'Choose playlists to include', choices = playlists$playlist_name, selected = playlists$playlist_name, multiple = T),
                        withBusyIndicatorUI(
                            actionButton('playlist_go', 'Get tracks and generate plot', class = 'btn-primary')
                        )
                    )
                })
            } else {
                stop("Sorry, couldn't find that user on Spotify.")
            }
        })
    })
    
    output$uri_gif <- renderText({
        HTML('<img src="user_uri.gif">')
    })
    
    observeEvent(input$playlist_go, {
        withBusyIndicatorServer('playlist_go', {
            
            if (length(input$playlist_selector) == 0) {
                stop("You must select at least one playlist.")
            }
            
            playlist_tracks <- get_playlist_tracks(playlists)
            
            playlist_track_audio_features <- get_track_audio_features(playlist_tracks)
            
            track_df <<- playlist_tracks %>%
                filter(playlist_name %in% input$playlist_selector) %>% 
                left_join(playlist_track_audio_features, by = 'track_uri')
            
            if (nrow(track_df) == 0) {
                stop("Sorry, couldn't find any tracks for that user's playlists on Spotify.")
            }
            
            output$playlist_quadrant_chart <- renderHighchart({
                playlist_quadrant_chart(track_df)
            })
            
            # output$playlist_profile_chart <- renderHighchart({
            #     sentiment_profile_chart(track_df, 'playlist_name')
            # })
            
            output$playlist_plot <- renderUI({
                
                withProgress(message = 'Making plot', value = 0, {
                
                if (input$GetScreenWidth >= 800) {
                    incProgress(.9)
                    highchartOutput('playlist_quadrant_chart', width = '820px', height = '700px')
                    # tabBox(
                    #     id = 'plots',
                    #     tabPanel('Sentiment Quadrants', highchartOutput('playlist_quadrant_chart', width = '775px', height = '700px')),
                    #     tabPanel('Playlist Profile', highchartOutput('playlist_profile_chart', width = '775px', height = '700px')),
                    #     width = 9
                    #     )
                } else {
                    incProgress(.9)
                    highchartOutput('playlist_quadrant_chart')
                }
                })
            })
            
        })
    })
    
})