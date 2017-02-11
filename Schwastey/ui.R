dashboardPage(
    title = tags$head(tags$link(rel = 'icon', type = 'image/png', href = 'biohazard-148696_640.png'),
                      tags$title('Schwastey')),
    dashboardHeader(
        title = div(img(src = "biohazard-148696_640.png", height = 35, width = 38), 
                    "Toxic Waste Dashboard"),
        titleWidth = 380),
        dashboardSidebar(
            htmlOutput('mytext')),
        dashboardBody(
            leafletOutput('myMap', width = '100%', height = 500),
            tags$head(
                tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css'))
        )
    )