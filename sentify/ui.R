shinyUI(fluidPage(
    navbarPage('Sentify',
               tabPanel('Artists',
                        # titlePanel('Artists'),
                        
                        sidebarLayout(
                            sidebarPanel(
                                textInput('artist_search', "Search for an artist on Spotify", value = ''),
                                uiOutput('select_artist_ui'),
                                uiOutput('album_go_ui'),
                                uiOutput('albums_ui'),
                                helpText("Click here for more info on Spotify's API")
                            ),
                            mainPanel(
                                uiOutput('artist_plot')
                            )
                        )), 
               tabPanel('Playlists',
                        # titlePanel('Playlists'),
                        
                        sidebarLayout(
                            sidebarPanel(
                                radioButtons('user_selector', '', c('Select user from list', HTML(paste0('Enter specific Spotify user ID')))),
                                uiOutput('select_user_ui'),
                                actionButton('user_go', 'Search for user'),
                                htmlOutput('user'),
                                uiOutput('select_playlist_ui'),
                                helpText("Click here for more info on Spotify's API")
                            ),
                            mainPanel(
                                uiOutput('playlist_plot')
                            )
                        )
               )
    ),
    
    tags$script(jscode)
))