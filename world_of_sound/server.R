shinyServer(function(input, output, session) {
    
    output$world_map <- renderLeaflet({
        
        req(input$world_map_metric)
        
        # selected_map_metric <- 'valence'
        selected_world_map_metric <<- input$world_map_metric
        
        if (!is.null(selected_world_map_metric)) {
            
            pal <- colorNumeric('RdYlGn', as.data.frame(countries)[[selected_world_map_metric]], na.color = 'lightgrey')
            
            countries[['world_map_metric']] <- countries[[selected_world_map_metric]]
            
            leaflet(countries) %>%
                addTiles() %>%
                setView(0, 0, zoom = 1) %>%
                addPolygons(layerId = ~id, opacity = 1, color = 'black', weight = 1, smoothFactor = 0.3, fillOpacity = 1,
                            fillColor = ~pal(world_map_metric),
                            label = ~paste0(name, ': ', formatC(world_map_metric, big.mark = ',')),
                            highlightOptions = highlightOptions(color = 'black', weight = 3,
                                                                bringToFront = TRUE)) %>%
                addLegend(pal = pal, values = ~world_map_metric, opacity = 1.0, title = selected_world_map_metric)
        }
    })
    
    output$map_title <- renderText({
        
        req(input$world_map_metric)
        
        selected_world_map_metric <- 'valence'
        selected_world_map_metric <- input$world_map_metric
        
        glue("World countries' most distinctively listened to music on Spotify, ranked by average song {selected_world_map_metric}")
    })
    
    output$music <- renderUI({
        
        req(input$world_map_shape_mouseover)
        req(input$world_map_metric)
        
        # mouseover_country <- 'ARG'
        mouseover_country <<- input$world_map_shape_mouseover$id
        
        # selected_map_metric <- 'valence'
        selected_world_map_metric <<- input$world_map_metric

            country_track <- geo_tracks %>% 
                filter(iso3c == mouseover_country, !is.na(track_preview_url)) %>%
                mutate_('world_map_metric' = selected_world_map_metric) %>% 
                mutate(feature_mean = country_features[[selected_world_map_metric]][country_features$iso3c == mouseover_country],
                       dist_from_mean = abs(world_map_metric - feature_mean)) %>% 
                filter(dist_from_mean == min(dist_from_mean, na.rm = T)) %>% 
                select(track_preview_url) %>%
                slice(1) %>% 
                .[[1]]
                
            tagList(
                tags$audio(id = 'song_preview', src = country_track, type = 'audio/mp3', autoplay = NA, controls = NA),
                tags$script(JS("
                            myAudio=document.getElementById('song_preview');
                            myAudio.play();
                            //myAudio.currentTime = 12;
                               "
                ))
            )
    })
    
    output$flag <- renderText({
        req(input$world_map_shape_mouseover)
        mouseover_country <- input$world_map_shape_mouseover$id
        
        
        country_name <- unique(countrycode_data$country.name.en[countrycode_data$iso3c == mouseover_country])
        country_name <- country_name[!is.na(country_name)]
        
        iso2c <- unique(countrycode_data$iso2c[countrycode_data$iso3c == mouseover_country])
        iso2c <- iso2c[!is.na(iso2c)]
        
        HTML(glue('
<h3>{country_name}</h3>
<img src="country-flags/png250px/{iso2c}.png">
                  '))
    })
    
    # output$feature_rank <- renderHighchart({
    #     
    #     req(input$map_metric)
    #     
    #     selected_map_metric <- 'valence'
    #     selected_map_metric <- input$map_metric
    #     
    #     country_features %>% 
    #         ungroup %>% 
    #         select(c('country', feature_vars)) %>% 
    #         gather(metric, value, -country) %>% 
    #         filter(metric == selected_map_metric) %>% 
    #         arrange(-value) %>% 
    #         mutate(value = round(value, 4)) %>% 
    #         hchart(hcaes(x = country, y = value, group = metric), type = 'bar') %>% 
    #         hc_xAxis(title = list(text = '')) %>% 
    #         hc_yAxis(title = list(text = selected_map_metric), min = min(country_features[[selected_map_metric]]))
    #     
    # })
})