shinyServer(function(input, output, session) {
    
    
    output$myMap <- renderLeaflet({
        leaflet() %>%
            setView(lat = 35, lng = -88, zoom = 4) %>% 
            addPolygons(data = states_og, color = 'black', weight = 1, fillColor = ~pal_states(count),
                        fillOpacity = 1, group = 'states', layerId = ~NAME) %>% 
            addCircles(data = mydf, lng = ~longitude, lat = ~latitude, popup = ~site.name, fillColor = ~pal_sites(site.rating), fillOpacity = 1,
                       color = 'black', weight= 1, radius = 40000, group = 'sites', layerId = ~site.id) %>% 
            addLegend(pal = pal_states, values = states_og$count, opacity = 1) %>% 
            addLegend(pal = pal_sites, values = mydf$site.rating, opacity = 1, position = 'bottomleft') %>% 
            addLayersControl(overlayGroups = 'sites', options = layersControlOptions(collapsed = F))
    })
    
    # Most recent mouseover region
    observeEvent(input$myMap_shape_mouseover, {
        
        # mouse_loc <<- input$myMap_shape_mouseover
        
        if (!is.null(input$myMap_shape_mouseover)) {
            if (input$myMap_shape_mouseover$group == 'sites') {
                output$mytext <- renderUI({
                    HTML(
                        paste0(
                            '<p align = "center"><b>',
                            mydf$site.name[mydf$site.id == input$myMap_shape_mouseover$id],
                            '</b></p>
                            <table border = "0", width = "100%">
                            <tr>
                            <th></th>
                            <th align = "center"><u>Materials</u></th>
                            </tr>
                            <td></td>
                            <td align = "left">', paste(unlist(mydf$materials[mydf$site.id == input$myMap_shape_mouseover$id]), collapse = '<br>'), '</td>'
                        ))
                })
            } else {
                output$mytext <- renderUI({
                    HTML(
                        paste0(
                            '<b>', input$myMap_shape_mouseover$id, '</b>'
                        )
                    )
                })
            }
        }
    }, ignoreNULL = F)
    
    # Light up region on mouseover
    observe({
        
        proxy <- leafletProxy('myMap') %>%
            clearGroup('hover')
        
        if (!is.null(input$myMap_shape_mouseover)) {
            
            # mouse_loc <<- input$myMap_shape_mouseover
            
            if (input$myMap_shape_mouseover$group == 'states') {
                proxy <- proxy %>%
                    addPolygons(data = states_og[states_og$NAME == input$myMap_shape_mouseover$id, ], group = 'hover',
                                 color = '#555555', opacity = 1, weight = 5, fill = F)
            } else if (input$myMap_shape_mouseover$group == 'sites') {
                proxy <- proxy %>% 
                    addCircles(data = mydf[mydf$site.id == input$myMap_shape_mouseover$id, ], group = 'hover', fill = F,
                               lng = ~longitude, lat = ~latitude, radius = 40000, color = '#555555',
                               opacity = 1, weight = 5)
            }
        }
        
        return(proxy)
    })
    
})