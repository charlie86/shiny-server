fluidPage(
    tags$head(tags$link(rel = 'icon', type = 'image/png', href = 'green_music_note.png'),
              tags$title('Music map')),
    tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css')),
    navbarPageWithText('Music map',
                       tabPanel('Map',
                                
                                sidebarLayout(
                                    sidebarPanel(
                                        selectInput('map_metric', 'Choose an audio feature:', feature_vars),
                                        helpText(HTML(paste0('Click <a href="https://developer.spotify.com/web-api/" target="_blank">here</a> for more info on Spotify\'s API'))),
                                        highchartOutput('feature_rank', height = '730')
                                    ),
                                    mainPanel(
                                        h2(textOutput('map_title'), style = "color:white;margin-top:0px"),
                                        leafletOutput('music_map', height = '800')
                                    )
                                )
                       ),
    text = HTML('<a href = "http://RCharlie.com" target = "_blank">RCharlie.com</a>')
))