shinyUI(fluidPage(
    titlePanel('Sentify'),
    
    sidebarLayout(
        sidebarPanel(        
            textInput('artist_search', 'Type an artist name', value = ''),
            uiOutput('select_artist_ui'),
            uiOutput('album_go_ui'),
            uiOutput('albums_ui')),
        mainPanel(
            highchartOutput('quadrant_chart', width = '775px', height='700px')
        )
    )
))