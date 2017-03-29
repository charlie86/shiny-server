shinyServer(function(input, output, session) {
    
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
            left_join(playlist_track_audio_features, by = 'track_uri') %>%
            mutate_at(c('playlist_name', 'playlist_img', 'track_name', 'track_uri', 'artist_name', 'album_name', 'album_img'), funs(as.character)) %>%
            mutate_at(c('danceability', 'energy', 'key', 'loudness', 'mode', 'speechiness', 'acousticness',
                        'instrumentalness', 'liveness', 'valence', 'tempo', 'duration_ms', 'time_signature'), funs(as.numeric(gsub('[^0-9.-]+', '', as.character(.)))))
        
        output$quadrant_plot <- renderHighchart({
            track_df %>% 
                filter(playlist_name %in% input$playlist_selector) %>%
                rowwise %>%
                mutate(tooltip = paste0('<a style = \"margin-right:', max(max(nchar(track_name), nchar(playlist_name)) * 9, 110), 'px\">',
                                        '<img src=', album_img, ' height=\"50\" style=\"float:left;margin-right:5px\">',
                                        '<b>Artist:</b> ', artist_name,
                                        '<br><b>Track:</b> ', track_name,
                                        '<br><b>Valence:</b> ', valence,
                                        '<br><b>Energy:</b> ', energy)) %>% 
                ungroup %>% 
                hchart(hcaes(x = energy, y = valence, group = playlist_name), type = 'scatter') %>% 
                hc_tooltip(formatter = JS(paste0("function() {return this.point.tooltip;}")), useHTML = T) %>%
                hc_yAxis(max = 1, min = 0, title = list(text = 'Valence')) %>%
                hc_xAxis(max = 1, min = 0, title = list(text = 'Energy')) %>%
                hc_add_theme(hc_theme_smpl()) %>% 
                hc_yAxis(plotLines = list(list(
                    value = .5,
                    color = 'black',
                    width = 2,
                    zIndex = 2))) %>% 
                hc_xAxis(plotLines = list(list(
                    value = .5,
                    color = 'black',
                    width = 2,
                    zIndex = 2)))
        })
    })
    
    # output$track_plot <- renderHighchart({
    #     avg_line <- track_df %>%
    #         group_by(playlist_name) %>%
    #         summarise(avg = mean(valence, na.rm = T)) %>%
    #         ungroup %>%
    #         transmute(x = as.numeric(as.factor(playlist_name)), y = avg
    #                   # ,tooltip = paste0('<a style = "margin-right:', nchar(album_name) * 10, 'px">',
    #                   #                  '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
    #                   #                  '<b>Album:</b> ', album_name,
    #                   #                  '<br><b>Average Track ', feature,':</b> ', round(avg, 4),
    #                   #                  '</a>')
    #         )
    #     
    #     hc <- hchart(object = track_df, hcaes(x = as.numeric(as.factor(playlist_name)), y = valence, group = playlist_name), type = 'scatter') %>%
    #         hc_add_series(data = avg_line, type = 'line') %>%
    #         hc_colors(c(sample(brewer.pal(12, 'Paired'), n_distinct(track_df$playlist_name)), 'black')) %>%
    #         hc_yAxis(max = 1)
    #     hc$x$hc_opts$series[[n_distinct(track_df$playlist_name) + 1]]$name <- 'Playlist Averages'
    #     hc
    # })
    
})
#### track valence for a specific playlist
# tots %>%
#     rowwise %>% 
#     mutate(tooltip = paste0('<a style = "margin-right:', max(nchar(track_name), nchar(playlist_name)) * 9, 'px">',
#                             '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
#                             '<b>Artist:</b> ', artist_name,
#                             '<br><b>Track:</b> ', track_name,
#                             '<br><b>Valence:</b> ', valence)) %>% 
#     ungroup %>% 
#     filter(playlist_name == 'HappySad') %>%
#     arrange(-valence) %>% 
#     mutate(track_name = ifelse(nchar(track_name) > 20, paste0(substr(track_name, 1, 16), ' ...'), track_name)) %>% 
#     hchart(hcaes(x = track_name, y = valence), type = 'bar') %>% 
#     hc_tooltip(formatter = JS(paste0("function() {return this.point.tooltip;}")), useHTML = T) %>% 
#     hc_yAxis(max = 1, title = list(text = 'Valence')) %>%
#     hc_xAxis(title = list(text = '')) %>% 
#     hc_add_theme(hc_theme_economist())


