function(request) {
    fluidPage(
    tags$head(tags$link(rel = 'icon', type = 'image/png', href = 'green_music_note.png'),
              tags$title('Sentify')),
    
    useShinyjs(),
    tags$script(jscode),
    tags$style(appCSS),
    tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css')),
    
    navbarPageWithText('Sentify', id = 'tabs',
               tabPanel('Artists',
                        
                        sidebarLayout(
                            sidebarPanel(
                                textInput('artist_search', "Type an artist name", value = ''),
                                uiOutput('select_artist_ui'),
                                htmlOutput('artist_img'),
                                br(),
                                uiOutput('album_go_ui'),
                                br(),
                                uiOutput('albums_ui'),
                                bookmarkButton(id = 'bookmark1'),
                                helpText(HTML(paste0('Click <a href="https://developer.spotify.com/web-api/" target="_blank">here</a> for more info on Spotify\'s API')))
                            ),
                            mainPanel(
                                uiOutput('artist_plot')
                            )
                        )), 
               tabPanel('User Playlists',
                        
                        sidebarLayout(
                            sidebarPanel(
                                textInput('user', 'Enter a User\'s Spotify URI', placeholder = 'e.g. barackobama'),
                                withBusyIndicatorUI(
                                    actionButton('user_go', 'Search for user', class = 'btn-primary')
                                ),
                                htmlOutput('user'),
                                uiOutput('select_playlist_ui'),
                                br(),
                                bsModal('modalExample', 'Where to find your Spotify URI (Desktop only)', 'tutorial_go', size = 'large', htmlOutput('uri_gif')),
                                actionButton('tutorial_go', 'Where do I find my Spotify URI?'),
                                bookmarkButton(id = 'bookmark2'),
                                helpText(HTML(paste0('Click <a href="https://developer.spotify.com/web-api/" target="_blank">here</a> for more info on Spotify\'s API')))
                            ),
                            mainPanel(
                                uiOutput('playlist_plot')
                            )
                        )
               ), text = HTML('<a href = "http://RCharlie.com" target = "_blank">RCharlie.com</a>')
    )
)
}