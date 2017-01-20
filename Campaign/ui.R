library(shinyjs)

dashboardPage(
    
    dashboardHeader(),
    dashboardSidebar(
        selectInput('twitt_timeframe', 'Tweet timeframe',
                    timeframes)
    ),
    dashboardBody(
        # box(plotOutput('agg_sent'), width = 12),
        # br(),
        # tabBox(title = 'Raw Tweets',
        #        tabPanel('Positive', htmlOutput('pos_twitts')),
        #        tabPanel('Negative', htmlOutput('neg_twitts')),
        #        width = 12)
        
        
        leafletOutput('my_map')
)
)