shinyUI(fluidPage(
    titlePanel('Sentify'),
    
    sidebarLayout(
        sidebarPanel(        
            textInput('artist_search', 'Type an artist name', value = ''),
            uiOutput('select_artist_ui'),
            uiOutput('album_go_ui'),
            uiOutput('albums_ui'),
            helpText("Click here for more info on Spotify's API")
        ),
        mainPanel(
            highchartOutput('quadrant_chart')
            # highchartOutput('quadrant_chart', width = '775px', height='700px')
        )
    )
))