dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
        textInput('artist', 'Type an artist name', value = ''),
        uiOutput('select_artist_ui'),
        uiOutput('album_go_ui'),
        uiOutput('albums_ui'),
        selectInput('feature_var', 'Audio Feature', pca_vars, selected = 'valence')
    ),
    dashboardBody(
        highchartOutput('album_feature_chart'),
        dataTableOutput('track_tbl'),
        plotlyOutput('album_prog_chart')
    )
)