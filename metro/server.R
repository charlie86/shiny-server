shinyServer(function(input, output, session) {
    
    observeEvent(input$refresh, {
        
        lapply(metrics, function(x) {
            assign(names(metrics[metrics == x]), 
                   dbGetQuery(chuckDB, paste0('SELECT * FROM ', names(metrics[metrics == x]), 
                                           ' WHERE time = (SELECT MAX(time) FROM ', names(metrics[metrics == x]), ')')), 
                   envir = .GlobalEnv)
        }) %>% invisible
        
        line_order <- data.frame(line = 'SV',
                                 station = c('Wiehle-Reston East', 'Spring Hill', 'Greensboro',
                                             'Tysons Corner', 'McLean', 'East Falls Church',
                                             'Ballston-MU', 'Virginia Square-GMU', 'Clarendon',
                                             'Court House', 'Rosslyn', 'Foggy Bottom-GWU',
                                             'Farragut West', 'McPherson Square', 'Metro Center',
                                             'Federal Triangle', 'Smithsonian', 'L\'Enfant Plaza',
                                             'Federal Center SW', 'Capitol South', 'Eastern Market',
                                             'Potomac Ave', 'Stadium-Armory', 'Benning Road',
                                             'Capitol Heights', 'Addison Road-Seat Pleasant', 'Morgan Boulevard', 
                                             'Largo Town Center')) %>% 
            mutate(order = row_number())
        
        ##########
        # test <- line_order %>%
        #     left_join(predictions, by = c('line' = 'Line', 'station' = 'LocationName')) %>% 
        #     filter(DestinationName == 'Wiehle-Reston East' | is.na(DestinationName)) %>% 
        #     group_by(station, DestinationName) %>% 
        #     slice(1) %>% 
        #     ungroup %>% 
        #     arrange(order) %>% 
        #     mutate(Min = ifelse(Min %in% c('BRD', 'ARR'), 0, Min),
        #            lead = lead(Min),
        #            time = ifelse(Min > lead(Min), Min - lead(Min), NA))
        
        predictions <- predictions %>%
            group_by(LocationName) %>%
            mutate(rank = row_number()) %>%
            ungroup
        
        df <- stations %>%
            left_join(predictions, by = c('Code' = 'LocationCode'))
        
        df$line_img <- vapply(as.character(df$Line), function(x) {
            switch(x,
                   'SV' = 'silver',
                   'OR' = 'orange',
                   'RD' = 'red',
                   'BL' = 'blue',
                   'GR' = 'green',
                   'YL' = 'yellow',
                   'No' = 'No',
                   '--' = '--',
                   'NA'
            )
        }, character(1)) %>% paste0('<img src="http://www.wmata.com/images/', ., '-dot.png" height = "30"></img>')
        
        df$line_img <- ifelse(df$Line %in% c('--', 'No'), '<img src="http://emojipedia-us.s3.amazonaws.com/cache/63/66/6366ee08b752e4af3d8c4ed88f35f67c.png" height = "30"></img>', df$line_img)
        
        
        my_df <<- df %>%
            rowwise %>%
            mutate(line_img = HTML(line_img),
                   name_html = HTML(paste0('<b>', Name, '</b>'))) %>% 
            arrange(rank)
        
        select_lines <<- lapply(as.character(unique(my_df$Line[!my_df$Line %in% c('--', 'No')])), function(x) {
            as.character(unique(my_df$Name[my_df$Line == x & !is.na(my_df$Name)]))
        }) %>% as.list
        names(select_lines) <<- as.character(unique(my_df$Line[!my_df$Line %in% c('--', 'No')]))
        select_lines <<- select_lines[1:6]
        
        lapply(names(select_lines), function(x) {
            select_lines[[x]] <<- select_lines[[x]][!is.na(select_lines[[x]])]
        })
        
        updateSelectizeInput(session, 'station',
                             choices = select_lines,
                             selected = 'Metro Center'
                             # , options = list(render = I(
                             #     '{
                             #        options: function(item, escape) {
                             #        return "<div><strong>" + escape(item.value) + "</strong></div>"
                             #        }
                             #     }'
                             # ))
        )
        
        observeEvent(input$map_marker_click, {
            updateSelectizeInput(session, 'station',
                                 choices = select_lines,
                                 selected = input$map_marker_click$id
                                 # , options = list(render = I(
                                 #     '{
                                 #        options: function(item, escape) {
                                 #        return "<div><strong>" + escape(item.value) + "</strong></div>"
                                 #        }
                                 #     }'
                                 # ))
            )
            
            output$train_tbl <- renderDataTable({
                
                tmp_tbl <- my_df %>% 
                    ungroup %>% 
                    filter(as.character(LocationName) == input$station)
                
                tmp_tbl %>%
                    ungroup %>%
                    select(Destination = DestinationName, Line = line_img, Min) %>%
                    datatable(caption = , rownames = F, escape = F,
                              options = list(scrollX = T, searching = F, paging = F, info = F, ordering = F, processing = F,
                                             columnDefs = list(list(className = 'dt-center', targets = 0:length(.) - 1))
                              ))
            })
        }, ignoreNULL = F)
        
        output$update_text <- renderText({
            paste('Last updated at', (Sys.time() - hours(4)) %>% format.POSIXct('%I:%M %p') %>% gsub('^0', '', .))
        })
        
    }, ignoreNULL = F)
    
    output$map <- renderLeaflet({
        leaflet() %>% 
            setView(lat = 38.8983, lng = -77.0281, zoom = 13) %>%
            addProviderTiles('Stamen.TonerLite') %>% 
            addMarkers(lng = my_df$Lon, lat = my_df$Lat, layerId = my_df$LocationName, popup = my_df$name_html, icon = train_icon) %>%
            addPolylines(data = map_lines, color = ~NAME, opacity = 1)
    })
    
    
})