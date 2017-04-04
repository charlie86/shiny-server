# dashboardPage(
#     dashboardHeader(),
#     dashboardSidebar(disable = T
#     ),
#     dashboardBody(
#         column(9, leafletOutput('map', height = '500px')),
#         column(3, 
#                div(style = 'display: inline-block',
#                    div(style = 'display: inline-block', selectizeInput('station', 'Station', 'Metro Center'))
#                    ),
#                dataTableOutput('train_tbl'),
#                br(), 
#                actionButton('refresh', 'Refresh'),
#                textOutput('update_text')),
#         tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'map.css'))
#     )
# )

shinyUI(fluidPage(
    tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'map.css')),
    sidebarLayout(
        sidebarPanel(
                   # div(style = 'display: inline-block',
                   #     div(style = 'display: inline-block', selectizeInput('station', 'Station', 'Metro Center', width = '100%'))
                   # ),
                    selectizeInput('station', 'Station', 'Metro Center'),
                   dataTableOutput('train_tbl'),
                   br(), 
                   actionButton('refresh', 'Refresh'),
                   textOutput('update_text')
        ),
        mainPanel(
            leafletOutput('map', height = '500px')
        )
    )
))