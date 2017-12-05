shinyServer(function(input, output, session) {
    
    output$music_map <- renderLeaflet({
        
        selected_map_metric <- 'valence'
        selected_map_metric <- input$map_metric
            
        pal <- colorNumeric('RdYlGn', countries[[selected_map_metric]], na.color = 'lightgrey')
        
        countries[['map_metric']] <- countries[[selected_map_metric]]
        
        leaflet(countries) %>%
            addTiles() %>%
            setView(0, 0, zoom = 2) %>% 
            addPolygons(layerId = ~id, opacity = 1, color = 'black', weight = 1, smoothFactor = 0.3, fillOpacity = 1,
                        fillColor = ~pal(map_metric),
                        label = ~paste0(name, ': ', formatC(map_metric, big.mark = ',')),
                        highlightOptions = highlightOptions(color = 'black', weight = 3,
                                                            bringToFront = TRUE)) %>%
            addLegend(pal = pal, values = ~map_metric, opacity = 1.0, title = selected_map_metric)
    })
    
    output$map_title <- renderText({
        glue("World countries' most distinctively listened to music on Spotify, ranked by average song {input$map_metric}")
    })
    
    output$music <- renderUI({
        
        mouseover_country <- input$music_map_shape_mouseover
        
        country_track <- geo_tracks %>% 
            filter(iso3c == mouseover_country, !is.na(track_preview_url)) %>% 
            select(track_preview_url) %>% 
            slice(1) %>% 
            .[[1]]
        
        tags$audio(src = country_track, type = "audio/mp3", autoplay = NA, controls = NA)
    })
    
    output$feature_rank <- renderHighchart({
        selected_map_metric <- 'valence'
        selected_map_metric <- input$map_metric
        
        country_features %>% 
            ungroup %>% 
            select(c('country', feature_vars)) %>% 
            gather(metric, value, -country) %>% 
            filter(metric == selected_map_metric) %>% 
            arrange(-value) %>% 
            mutate(value = round(value, 4)) %>% 
            hchart(hcaes(x = country, y = value, group = metric), type = 'bar') %>% 
            hc_xAxis(title = list(text = '')) %>% 
            hc_yAxis(title = list(text = selected_map_metric), min = min(country_features[[selected_map_metric]]))
        
    })
})