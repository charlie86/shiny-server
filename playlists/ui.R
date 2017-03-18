dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
        radioButtons('selector', '', c('Select user from list', 'Enter specific User ID')),
        uiOutput('select_user_ui'),
        actionButton('gobutton', 'Search for user'),
        htmlOutput('user'),
        uiOutput('select_playlist_ui')
    ),
    dashboardBody(
        highchartOutput('quadrant_plot', width = '775px', height='700px')
        # highchartOutput('track_plot')
        # dataTableOutput('playlist_tbl')
    )
)