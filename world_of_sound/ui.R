fluidPage(
    tags$head(tags$link(rel = 'icon', type = 'image/png', href = 'green_music_note.png'),
              tags$title('Sounds of the World')),
    tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css')),
    navbarPageWithText('Sounds of the World',
                       tabPanel('Country Explorer',
                                sidebarLayout(
                                    sidebarPanel(
                                        h4('Click on a country to explore its music'),
                                        leafletOutput('world_map'),
                                        selectInput('world_map_metric', 'Color countries by:', feature_vars, width = '150px'),
                                        uiOutput('music'),
                                        htmlOutput('flag')
                                        # highchartOutput('feature_rank', height = '730')
                                    ),
                                    mainPanel(
                                        # h2(textOutput('map_title'), style = "color:white;margin-top:0px"),
                                        
                                    )
                                )
                       ),
    text = HTML('<a href = "http://RCharlie.com" target = "_blank">RCharlie.com</a>')
))