shinyServer(function(input, output, session) {
    
    setBookmarkExclude(c('bookmark1', 'bookmark2'))
    
    observeEvent(input$bookmark1, {
        session$doBookmark()
    })
    
    observeEvent(input$bookmark2, {
        session$doBookmark()
    })
    
    spotify_access_token <- reactive({
        get_spotify_access_token()
    })
    
    observe({
        output$screenwidth <- renderText({
            input$GetScreenWidth
        })
    })
    
    artist_info <- reactive({
        req(input$artist_search)
        get_artists(input$artist_search, access_token = spotify_access_token())
    })
    
    album_info <- reactive({
        artist_info() %>% 
            filter(artist_name == input$select_artist) %>% 
            pull(artist_uri) %>% 
            get_artist_albums(artist_uri = ., use_artist_uri = TRUE, access_token = spotify_access_token())
    })
    
    artist_tracks <- reactive({
        get_album_tracks(album_info(), access_token = spotify_access_token())
    })
    
    artist_track_audio_features <- reactive({
        artist_tracks() %>% 
            filter(album_name %in% input$albums) %>% 
            get_track_audio_features(access_token = spotify_access_token())
    })
    
    artist_track_popularity <- reactive({
        artist_tracks() %>% 
            filter(album_name %in% input$albums) %>% 
            get_track_popularity(access_token = spotify_access_token())
    })
    
    track_info <- reactive({
        artist_tracks() %>%
            inner_join(artist_track_audio_features(), by = 'track_uri') %>% 
            left_join(artist_track_popularity(), by = 'track_uri') %>% 
            left_join(album_info(), by = 'album_name')
    })
    
    output$select_artist_ui <- renderUI({
        selectInput('select_artist', 'Choose an artist from these matches on Spotify', choices = artist_info()$artist_name)
    })
    
    
    observeEvent(input$select_artist, {
        
        req(nrow(artist_info()) > 0)
        
        artist_img <- ifelse(!is.na(artist_info()$artist_img[artist_info()$artist_name == input$select_artist]), artist_info()$artist_img[artist_info()$artist_name == input$select_artist], 'https://pbs.twimg.com/profile_images/509949472139669504/IQSh7By1_400x400.jpeg')
        
        output$artist_img <- renderText({
            HTML(paste0('<img src=', artist_img, ' height="200">'))
        })
        
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
            
            if (nrow(album_info()) > 0) {
                
                output$albums_ui <- renderUI({
                    tagList(
                        selectizeInput('albums', 'Choose which albums to include', 
                                       choices = unique(album_info()$album_name), 
                                       selected = unique(album_info()$album_name), multiple = T),
                        withBusyIndicatorUI(
                            actionButton('tracks_go', 'Get tracks and generate plot', class = 'btn-primary')
                        ),
                        checkboxInput('artist_autoplay', 'Play song preview on hover'),
                        uiOutput('artist_chart_song_ui')
                    )
                })
            } else {
                stop("Sorry, couldn't find any albums for that artist on Spotify.")
            }
        })
    })
    
    observeEvent(input$tracks_go, {
        
        withBusyIndicatorServer('tracks_go', {
            
            if (nrow(track_info()) == 0) {
                stop("Sorry, couldn't find any tracks for that artist's albums on Spotify.")
            }
            
            output$artist_quadrant_chart <- renderHighchart({
                track_info() %>% 
                    artist_quadrant_chart() %>% 
                    hc_add_event_point(event = 'mouseOver')
            })
            
            output$artist_chart_song_ui <- renderUI({
                
                if (input$tabs == 'Artists') {
                    req(input$artist_quadrant_chart_mouseOver)
                    
                    if (input$artist_autoplay == TRUE) {
                        track_preview_url <- track_info() %>% 
                            filter(album_name == input$artist_quadrant_chart_mouseOver$series,
                                   valence == input$artist_quadrant_chart_mouseOver$x,
                                   energy == input$artist_quadrant_chart_mouseOver$y) %>% 
                            pull(track_preview_url)
                        if (!is.na(track_preview_url)) {
                            tagList(
                                tags$audio(id = 'song_preview', src = track_preview_url, type = 'audio/mp3', autoplay = NA, controls = NA),
                                tags$script(JS("
                                           myAudio=document.getElementById('song_preview');
                                           myAudio.play();
                                           "
                                ))
                            )
                        } else {
                            h5('No preview for this track on Spotify')
                        }
                    }
                }
            })
            
            output$artist_plot <- renderUI({
                
                withProgress(message = 'Making plot', value = 0, {
                    
                    if (input$GetScreenWidth >= 800) {
                        incProgress(.9)
                        highchartOutput('artist_quadrant_chart', width = '820px', height = '700px')
                    } else {
                        incProgress(.9)
                        highchartOutput('artist_quadrant_chart')
                    }
                })
            })
        })
    })
    
    user <- reactive({
        tolower(str_replace(input$user, 'spotify:user:', ''))
    })
    
    user_info <- reactive({
        GET(paste0(base_url, 'users/', user()), query = list(access_token = spotify_access_token())) %>% content
    })
    
    playlists <- reactive({
        user() %>% get_user_playlists(access_token = spotify_access_token())
    })
    
    playlist_tracks <- reactive({
        playlists() %>% get_playlist_tracks(access_token = spotify_access_token())
    })
    
    playlist_track_audio_features <- reactive({
        playlist_tracks() %>% 
            filter(playlist_name %in% input$playlist_selector) %>% 
            get_track_audio_features(access_token = spotify_access_token())
    })
    
    playlist_track_popularity <- reactive({
        playlist_tracks() %>% 
            filter(playlist_name %in% input$playlist_selector) %>% 
            get_track_popularity(access_token = spotify_access_token())
    })
    
    track_df <- reactive({
        playlist_tracks() %>%
            inner_join(playlist_track_audio_features(), by = 'track_uri') %>% 
            left_join(playlist_track_popularity(), by = 'track_uri')
    })
    
    observeEvent(input$user_go, {
        withBusyIndicatorServer('user_go', {
            
            if (nchar(input$user) == 0) {
                stop('Type a user name or Spotify URI')
            }
            
            if (is.null(user_info()$error)) {
                user_img <- ifelse(length(user_info()$images) > 0, user_info()$images[[1]]$url, 'https://pbs.twimg.com/profile_images/509949472139669504/IQSh7By1_400x400.jpeg')
                
                if (user() == 'barackobama') {
                    user_img <- 'barry.jpg'
                }
                
                output$user <- renderText({
                    HTML(str_glue('<img src={user_img} height="200"><br/>{user_info()$display_name}'))
                })
                
                if (nrow(playlists()) == 0) {
                    stop("Sorry, that user doesn't have any playlists on Spotify.")
                }
                
                output$select_playlist_ui <- renderUI({
                    tagList(
                        selectInput('playlist_selector', 'Choose playlists to include', choices = playlists()$playlist_name, selected = playlists()$playlist_name, multiple = T),
                        withBusyIndicatorUI(
                            actionButton('playlist_go', 'Get tracks and generate plot', class = 'btn-primary')
                        ),
                        checkboxInput('playlist_autoplay', 'Play song preview on hover'),
                        uiOutput('playlist_chart_song_ui')
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
            
            if (nrow(track_df()) == 0) {
                stop("Sorry, couldn't find any tracks for that user's playlists on Spotify.")
            }
            
            output$playlist_quadrant_chart <- renderHighchart({
                track_df() %>% 
                    playlist_quadrant_chart() %>% 
                    hc_add_event_point(event = 'mouseOver')
            })
            
            output$playlist_chart_song_ui <- renderUI({
                
                if (input$tabs == 'User Playlists') {
                    
                    req(input$playlist_quadrant_chart_mouseOver)
                    
                    if (input$playlist_autoplay == TRUE) {
                        
                        playlist_track_hover <- input$playlist_quadrant_chart_mouseOver
                        
                        track_preview_url <- track_df() %>% 
                            filter(playlist_name == playlist_track_hover$series,
                                   valence == playlist_track_hover$x,
                                   energy == playlist_track_hover$y) %>% 
                            pull(track_preview_url)
                        
                        if (!is.na(track_preview_url)) {
                            tagList(
                                tags$audio(id = 'song_preview', src = track_preview_url, type = 'audio/mp3', autoplay = NA, controls = NA),
                                tags$script(JS("
                                           myAudio=document.getElementById('song_preview');
                                           myAudio.play();
                                           "
                                ))
                            )
                        } else {
                            h5('No preview for this track on Spotify')
                        }
                    }
                }
            })
            
            output$playlist_plot <- renderUI({
                withProgress(message = 'Making plot', value = 0, {
                    if (input$GetScreenWidth >= 800) {
                        incProgress(.9)
                        highchartOutput('playlist_quadrant_chart', width = '820px', height = '700px')
                    } else {
                        incProgress(.9)
                        highchartOutput('playlist_quadrant_chart')
                    }
                })
            })
        })
    })
})