# shinyUI(fluidPage(
#     sidebarLayout(
#         sidebarPanel(
dashboardPage(
    dashboardHeader(),
    dashboardSidebar(disable = T),
    dashboardBody(
        fluidRow(
            column(3, 
                   helpText('Click on a station to see its stats'),
                   leafletOutput('bike_map'),
                   h2(textOutput('station_name_text'))   
            ),
            
            column(9,
                   uiOutput('boxes'),
                   highchartOutput('bike_plot')
            )
        )
    )
)
