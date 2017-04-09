artist_quadrant_chart <- function(artists, xvar_str, yvar_str) {
    
    df2 <- data.frame(x = c(0, 1, 0, 1),
                      y = c(1, 1, 0, 0),
                      text = c('Angry',
                               'Happy',
                               'Sad',
                               'Peaceful'))
    
    ds2 <- list_parse(df2)
    
    if (n_distinct(artists$artist_name) > 21) {
        my_colors <- tol21rainbow
    } else {
        my_colors <- sample(tol21rainbow, n_distinct(artists$artist_name))
    }
    
    artists %>% 
        rename_(xvar = xvar_str, yvar = yvar_str) %>% 
        rowwise %>%
        mutate(tooltip = paste0('<a style = \"margin-right:', max(nchar(artist_name) * 9, 110), 'px\">',
                                '<img src=', image_url, ' height=\"50\" style=\"float:left;margin-right:5px\">',
                                '<br><b>Artist:</b> ', artist_name,
                                '<br><b>', xvar_str, ':</b> ', xvar,
                                '<br><b>', yvar_str, ':</b> ', yvar)) %>% 
        ungroup %>% 
        hchart(hcaes(x = xvar, y = yvar), type = 'scatter') %>% 
        hc_tooltip(formatter = JS(paste0("function() {return this.point.tooltip;}")), useHTML = T) %>%
        hc_xAxis(max = 1, min = 0, title = list(text = xvar_str)) %>%
        hc_yAxis(max = 1, min = 0, title = list(text = yvar_str)) %>%
        hc_add_theme(hc_theme_smpl()) %>% 
        hc_colors(my_colors) %>% 
        hc_yAxis(plotLines = list(list(
            value = .5,
            color = 'black',
            width = 2,
            zIndex = 2))) %>% 
        hc_xAxis(plotLines = list(list(
            value = .5,
            color = 'black',
            width = 2,
            zIndex = 2))) %>% 
        hc_add_series(data = ds2,
                      name = "annotations",
                      type = "scatter",
                      color = "transparent",
                      showInLegend = FALSE,
                      enableMouseTracking = FALSE,
                      zIndex = 0,
                      dataLabels = list(enabled = TRUE, y = 10, format = "{point.text}",
                                        style = list(fontSize = "15px",
                                                     color =  'rgba(0,0,0,0.70)'))
        )
}
