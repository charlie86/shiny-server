dashboardPage(
    dashboardHeader(),
    dashboardSidebar(disable = T
    ),
    dashboardBody(
        column(9, leafletOutput('map', height = '500px')),
        column(3, 
               div(style = 'display: inline-block',
                   # div(style = 'display: inline-block', selectInput('line', 'Line', c('Red', 'Green'), width = '90px')),
                   div(style = 'display: inline-block', selectizeInput('station', 'Station', 'Metro Center'))
                   ),
               dataTableOutput('train_tbl'),
               br(), 
               actionButton('refresh', 'Refresh'),
               textOutput('update_text')),
        tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'map.css'))
    )
)
