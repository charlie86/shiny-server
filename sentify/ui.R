shinyUI(fluidPage(
    navbarPage('Sentify',
               tabPanel('Artists',
                        titlePanel('Sentify'),
                        
                        sidebarLayout(
                            sidebarPanel(
                                textInput('artist_search', "Search for an artist on Spotify", value = ''),
                                uiOutput('select_artist_ui'),
                                uiOutput('album_go_ui'),
                                uiOutput('albums_ui'),
                                helpText("Click here for more info on Spotify's API")
                            ),
                            mainPanel(
                                uiOutput('plot')
                            )
                        )), 
               tabPanel('Playlists')
    ),
    
    tags$script(jscode)
))