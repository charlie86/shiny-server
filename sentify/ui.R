shinyUI(fluidPage(
    tags$head(tags$link(rel = 'icon', type = 'image/png', href = 'smiley.png'),
              tags$title('Sentify')),
    
    useShinyjs(),
    tags$script(jscode),
    tags$style(appCSS),
    tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css')),
    
    navbarPage('Sentify',
               tabPanel('Artists',
                        
                        sidebarLayout(
                            sidebarPanel(
                                textInput('artist_search', "Type an artist name", value = ''),
                                uiOutput('select_artist_ui'),
                                uiOutput('album_go_ui'),
                                br(),
                                uiOutput('albums_ui'),
                                helpText(HTML(paste0('Click <a href="https://developer.spotify.com/web-api/" target="_blank">here</a> for more info on Spotify\'s API')))
                            ),
                            mainPanel(
                                uiOutput('artist_plot')
                            )
                        )), 
               tabPanel('User Playlists',
                        
                        sidebarLayout(
                            sidebarPanel(
                                radioButtons('user_selector', '', c('Select user from list', 'Enter specific Spotify User ID')),
                                conditionalPanel("input.user_selector=='Enter specific Spotify User ID'",
                                                 actionButton('tutorial_go', 'Where do I find my Spotify User ID?')
                                ),
                                bsModal('modalExample', 'Where to find your Spotify User ID (Desktop only)', 'tutorial_go', size = 'large', htmlOutput('uri_gif')),
                                br(),
                                uiOutput('select_user_ui'),
                                withBusyIndicatorUI(
                                    actionButton('user_go', 'Search for user', class = 'btn-primary')
                                ),
                                htmlOutput('user'),
                                uiOutput('select_playlist_ui'),
                                helpText(HTML(paste0('Click <a href="https://developer.spotify.com/web-api/" target="_blank">here</a> for more info on Spotify\'s API')))
                            ),
                            mainPanel(
                                uiOutput('playlist_plot')
                            )
                        )
               ),
               collapsible = T
    )
))