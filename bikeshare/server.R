shinyServer(function(input, output, session) {
        leaflet() %>%
            addTiles() %>% 
            setView(lat = 38.8983, lng = -77.0281, zoom = 14) %>%
            addPolygons(data = geojson_raw, fillOpacity = 1, fill = NULL, color = 'black', opacity = 1, weight = 1) %>% 
            addCircles(lng = stations$longitude, lat = stations$latitude, popup = stations$tooltip, layerId = stations$station_id)
    )
    
    observeEvent(input$bike_map_marker_click, {
        
        selected_station <<- input$bike_map_marker_click
        
        output$station_name_text <- renderText({
            stations$name[stations$station_id == selected_station$id]
        })
        
        output$boxes <- renderUI({
            tagList(
                valueBox(snapshot$nbBikes[snapshot$terminalName == selected_station$id], 'Bikes Right Now', icon = icon('bicycle')),
                valueBox(Sys.Date(), 'Estimated Time to Empty', icon = icon('clock-o'))
            )
        })
        
        res <- dbGetQuery(conn,paste0("
                                    SELECT
                                        bsl.station_id
                                        , bsl.name AS station_name
                                        , bd.bikes
                                        , bd.docks
                                        , bd.last_updated
                                    FROM bikeshare_station_lookup bsl
                                    INNER JOIN bikeshare_deduped bd
                                        ON bsl.station_id = bd.station_id
                                    WHERE bsl.station_id = '", selected_station$id, "';"))
                                    # WHERE bsl.station_id = '", 31285, "';"))
        
        days_of_week <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')
        
        dates_df <- tibble(mytime = seq.POSIXt(from = as.POSIXct('2017-03-21 00:00:00 EDT'), to = Sys.time(), by = 'min')) %>%
            mutate(date = as.Date(mytime - hours(4)),
                   mytime = round(hour(mytime) + (minute(mytime)/60), 2))
        
        graph_df <- res %>% 
            mutate(last_updated_og = as.POSIXct(last_updated, tz = '', format = '%Y-%m-%d %H:%M:%S') - hours(4),
                   last_updated = strftime(round(last_updated_og, 'mins'))) %>% 
            group_by(last_updated) %>%
            filter(last_updated_og == max(last_updated_og)) %>%
            filter(row_number() == max(row_number())) %>%
            ungroup %>% 
            select(-last_updated_og) %>% 
            mutate(date = as.Date(last_updated),
                   weekday = weekdays(date),
                   weekday_num = match(weekday, days_of_week),
                   hour = hour(last_updated),
                   minute = minute(last_updated),
                   second = second(last_updated),
                   mytime = round(hour + (minute/60), 2)) %>%
            arrange(mytime) %>%
            right_join(dates_df, by = c('mytime', 'date')) %>% 
            group_by(date) %>%
            mutate(num_entries_per_day = sum(!is.na(bikes))) %>% 
            ungroup %>% 
            filter(num_entries_per_day > 0) %>% 
            group_by(date) %>% 
            slice(min(which(!is.na(bikes))):n_distinct(mytime)) %>% 
            mutate_all(funs(na.locf(.))) %>% 
            group_by(mytime)
        
        gg_df <- graph_df %>%
            filter(mytime >= hour(Sys.time()) - 2, mytime <= hour(Sys.time()) + 2,
                   weekday == weekdays(Sys.time())) %>% 
            group_by(mytime) %>% 
            summarise(median = median(bikes),
                      mean = mean(bikes))
        
        output$bike_plot <- renderHighchart({            
            # ggplot() +
            #     geom_line(data = gg_df, aes(x = mytime, y = median)) +
            #     geom_point(data = snapshot %>% filter(terminalName == selected_station$id), aes(x = round(hour(lastCommWithServer) + (minute(lastCommWithServer) / 60), 2), y = nbBikes), color = 'blue', size = 3) +
            #     theme_tufte()
            
            highchart() %>% 
                hc_add_series(gg_df, hcaes(x = mytime, y = median), name = 'Historical Median', type = 'line') %>% 
                hc_add_series(snapshot %>% filter(terminalName == selected_station$id), name = 'Right Now', hcaes(x = round(lubridate::hour(lastCommWithServer) + (lubridate::minute(lastCommWithServer) / 60), 2), y = nbBikes), type = 'scatter',
                              marker = list(symbol = fa_icon_mark('bicycle')),
                              icon = fa_icon('bicycle')) %>% 
                hc_tooltip(
                    useHTML = TRUE,
                    pointFormat = paste0("<span style=\"color:{series.color};\">{series.options.icon}</span>",
                                         "{series.name}: <b>[{point.x}, {point.y}]</b><br/>")
                )
        })

        # graph_df <- res %>% 
        #     mutate(last_updated = as.POSIXct(last_updated) - hours(4),
        #            date = as.Date(last_updated),
        #            weekday = weekdays(date),
        #            weekday_num = match(weekday, days_of_week),
        #            time = strftime(round(last_updated, 'mins'), format="%H:%M:%S"),
        #            hour = as.numeric(substr(time,1,2)),
        #            minute = as.numeric(substr(time, 4, 5)),
        #            second = as.numeric(substr(time, 7, 8)),
        #            mytime = (hour*3600) + (minute*60) + second) %>%
        #     filter(hour >= 7, hour <= 10) %>%
        #     # arrange(mytime) %>% 
        #     group_by(date) %>% 
        #     mutate(empty_time = min(ifelse(bikes == 0, last_updated, NA), na.rm = T)) %>% 
        #     ungroup
        
        
        # dow <- graph_df %>%
        #     select(weekday, weekday_num, date, empty_time) %>% 
        #     unique %>% 
        #     arrange(weekday_num) %>% 
        #     mutate(empty_time = as.POSIXct(empty_time, origin = '1970-01-01'),
        #            mytime = hour(empty_time) + (minute(empty_time) / 60),
        #            tooltip = paste0(weekday, ', ', month(empty_time), '/', day(empty_time), ' - ', hour(empty_time), ':', str_pad(minute(empty_time), 2, pad = 0), 'am'))
        # 
        # output$bike_plot <- renderHighchart({
        #     highchart() %>%
        #         hc_boxplot(dow$mytime, dow$weekday, dow$weekday_num, name = 'Time of Day') %>%
        #         hc_add_theme(hc_theme_smpl()) %>% 
        #         hc_title(text = 'When should I wake up?') %>% 
        #         hc_subtitle(text = 'Time of day my bikeshare runs out of bikes')
        # })
        
    }, ignoreNULL = T)
})