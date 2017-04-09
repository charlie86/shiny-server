shinyUI(fluidPage(
    navbarPage(
        'Coachella Groove Explorer',
        tabPanel('Lineup',
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(),
                         h4("Who's the most danceable artist this year?")
                     ),
                     mainPanel(
                         
                     )
                 ))
    )
))