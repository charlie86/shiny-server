navbarPageWithText <- function(..., text) {
    navbar <- navbarPage(...)
    textEl <- tags$p(class = "navbar-text", text)
    navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
        navbar[[3]][[1]]$children[[1]], textEl)
    navbar
}

hc_theme_rcharlie <- hc_theme_merge(
    hc_theme_monokai(),
    hc_theme(
        chart = list(
            backgroundColor = '#828282'
        ),
        title = list(
            style = list(
                color = '#ffffff'
            )
        ),
        subtitle = list(
            style = list(
                color = '#ffffff'
            )
        ),
        xAxis = list(
            labels = list(style = list(
                color = '#ffffff'
            )),
            title = list(style = list(
                color = '#ffffff'
            ))
            
        ),
        yAxis = list(
            labels = list(style = list(
                color = '#ffffff'
            )),
            title = list(style = list(
                color = '#ffffff'
            ))
        ),
        legend = list(
            itemStyle = list(
                color = '#ffffff'
            )
        )
    )
)

album_feature_chart <- function(df, feature) {
    
    df <- track_info_sub
    
    plot_df <- df %>% 
        mutate_(feature_var = interp(~ x, x = as.name(feature))) %>% 
        rowwise %>% 
        mutate(tooltip = paste0('<a style = "margin-right:', max(nchar(track_name), nchar(album_name)) * 9, 'px">',
                                '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
                                '<b>Album:</b> ', album_name,
                                '<br><b>Track:</b> ', track_name)) %>% 
        ungroup
    avg_line <- plot_df %>% 
        group_by(album_rank, album_name, album_img) %>% 
        summarise(avg = mean(feature_var)) %>% 
        ungroup %>% 
        transmute(x = album_rank, y = avg,
                  tooltip = paste0('<a style = "margin-right:', nchar(album_name) * 10, 'px">',
                                   '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
                                   '<b>Album:</b> ', album_name,
                                   '<br><b>Average Track ', feature,':</b> ', round(avg, 4),
                                   '</a>'))
    dep_df <- plot_df %>% 
        mutate(tooltip = paste0(tooltip, '<br><b>', feature, ':</b> ', feature_var, '</a>')) %>% 
        ungroup
    
    cat_str <- paste0('var categoryLinks = {',
                      paste0(map(unique(df$album_name), function(x) {
                          paste0("'", x, "': '", df$album_img[df$album_name == x][1], "'")
                      }), collapse = ','), '};'
    )
    
    album_chart <- hchart(dep_df, x = album_rank, y = feature_var, group = album_name, type = 'scatter') %>% 
        hc_add_series_df(data = avg_line, type = 'line') %>%
        hc_tooltip(formatter = JS(paste0("function() {return this.point.tooltip;}")), useHTML = T) %>% 
        hc_colors(c(rep('#d35400', n_distinct(df$album_name)), 'black')) %>% 
        hc_legend(enabled = F) %>% 
        hc_xAxis(title = list(enabled = F), 
                 categories = c('test', unique(dep_df$album_name)),
                 labels = list(
                     useHTML = T,
                     formatter = JS(paste('function() {', cat_str,
                                          'return \'<a style = "align:center;text-align:center"><img src="\' + categoryLinks[this.value] + \'" height = "50px"/><br><b>\' + this.value + \'</b>\';}'))
                 )) %>% 
        hc_yAxis(title = list(text = feature)) %>% 
        hc_title(text = paste(artist_name, feature, 'by album')) %>% 
        hc_add_theme(hc_theme_rcharlie)
    album_chart
}

############ playlists
get_user_playlists <- function(user_str) {
    user <- tolower(user_str)
    base_url <- 'https://api.spotify.com/v1/'
    user_search_query <- paste0(base_url, 'users/', user, '/playlists')    
    user_playlists <- GET(user_search_query,
                          query = list(access_token = access_token, limit = 50)) %>% content %>% .$items
    
    if (length(user_playlists) > 0) {
        
        playlist_df <- map_df(1:length(user_playlists), function(x) {
            list(
                playlist_name = user_playlists[[x]]$name,
                playlist_img = user_playlists[[x]]$images[[1]]$url,
                playlist_num_tracks = user_playlists[[x]]$tracks$total,
                playlist_tracks_url = user_playlists[[x]]$tracks$href
            )
        })
    } else {
        playlist_df <- data.frame()
    }
    
    return(playlist_df)
}

playlist_quadrant_chart <- function(track_df) {
    
    df2 <- data.frame(x = c(0, 1, 0, 1),
                      y = c(1, 1, 0, 0),
                      text = c('Angry',
                               'Happy',
                               'Sad',
                               'Peaceful'))
    
    ds2 <- list_parse(df2)
    
    track_df %>% 
        rowwise %>%
        mutate(tooltip = paste0('<a style = \"margin-right:', max(max(nchar(track_name), nchar(artist_name)) * 9, 110), 'px\">',
                                '<img src=', album_img, ' height=\"50\" style=\"float:left;margin-right:5px\">',
                                '<b>Track:</b> ', track_name,
                                '<br><b>Artist:</b> ', artist_name,
                                '<br><b>Valence:</b> ', valence,
                                '<br><b>Energy:</b> ', energy)) %>% 
        ungroup %>% 
        hchart(hcaes(x = valence, y = energy, group = playlist_name), type = 'scatter') %>% 
        hc_tooltip(formatter = JS(paste0("function() {return this.point.tooltip;}")), useHTML = T) %>%
        hc_xAxis(max = 1, min = 0, title = list(text = 'Valence')) %>%
        hc_yAxis(max = 1, min = 0, title = list(text = 'Energy')) %>%
        hc_add_theme(hc_theme_rcharlie) %>% 
        hc_colors(neon_colors) %>% 
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
                                        style = list(fontSize = "18px",
                                                     color =  '#fff',
                                                     textOutline = '0px'))
        )
}

profile_bar_chart <- function(track_df, group_var_str) {
    track_df %>% 
        rename_('group_var' = group_var_str) %>% 
        group_by(group_var) %>% 
        summarise(valence = mean(valence, na.rm = T),
                  energy = mean(energy, na.rm = T),
                  `(energy+valence)/2` = sum(valence, energy, na.rm = T) / 2) %>% 
        ungroup %>% 
        arrange(-`(energy+valence)/2`) %>% 
        gather(metric, value, valence:`(energy+valence)/2`) %>% 
        mutate(value = round(value, 4)) %>% 
        hchart(hcaes(x = group_var, y = value, group = metric), type = 'bar') %>% 
        hc_xAxis(title = list(text = '')) %>% 
        hc_yAxis(title = list(text = ''), max = 1) %>% 
        hc_add_theme(hc_theme_rcharlie)
}

############## both

artist_quadrant_chart <- function(track_df) {
    
    df2 <- data.frame(x = c(0, 1, 0, 1),
                      y = c(1, 1, 0, 0),
                      text = c('Angry',
                               'Happy',
                               'Sad',
                               'Peaceful'))
    
    ds2 <- list_parse(df2)
    
    track_df %>% 
        rowwise %>%
        mutate(tooltip = paste0('<a style = \"margin-right:', max(max(nchar(track_name), nchar(album_name)) * 9, 110), 'px\">',
                                '<img src=', album_img, ' height=\"50\" style=\"float:left;margin-right:5px\">',
                                '<b>Track:</b> ', track_name,
                                '<br><b>Album:</b> ', album_name,
                                '<br><b>Valence:</b> ', valence,
                                '<br><b>Energy:</b> ', energy)) %>% 
        ungroup %>% 
        hchart(hcaes(x = valence, y = energy, group = album_name), type = 'scatter') %>% 
        hc_tooltip(formatter = JS(paste0("function() {return this.point.tooltip;}")), useHTML = T) %>%
        hc_yAxis(max = 1, min = 0, title = list(text = 'Energy')) %>%
        hc_xAxis(max = 1, min = 0, title = list(text = 'Valence')) %>%
        hc_add_theme(hc_theme_rcharlie) %>% 
        hc_colors(neon_colors) %>% 
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
                      name = 'annotations',
                      type = 'scatter',
                      color = 'transparent',
                      showInLegend = FALSE,
                      enableMouseTracking = FALSE,
                      zIndex = 0,
                      dataLabels = list(enabled = TRUE, y = 10, format = "{point.text}",
                                        style = list(fontSize = "18px",
                                                     color =  '#fff',
                                                     textOutline = '0px'))
        )
}

classify_track_sentiment <- function(valence, energy) {
    if (is.na(valence) | is.na(energy)) {
        return(NA)
    }
    else if (valence >= .5) {
        if (energy >= .5) {
            return('Happy')
        } else {
            return('Peaceful')
        }
    } else {
        if (energy >= .5) {
            return('Angry')
        } else {
            return('Sad')
        }
    }
}

sentiment_profile_chart <- function(track_df, group_var_str) {
    sentiment_profiles <- track_df %>% 
        rowwise %>% 
        mutate(sentiment = classify_track_sentiment(valence, energy)) %>% 
        ungroup
    
    group_var_profiles <- sentiment_profiles %>% 
        rename_('group_var' = group_var_str) %>% 
        count(group_var, sentiment) %>% 
        mutate(pct = round(n / sum(n), 2)) %>% 
        ungroup 
    
    user_profile <- sentiment_profiles %>% 
        count(sentiment) %>% 
        mutate(pct = round(n / sum(n), 2)) %>% 
        ungroup %>% 
        mutate_(group_var = '"All"')
    
    all_profiles <- rbind(user_profile, group_var_profiles)
    
    all_profiles %>% 
        hchart(hcaes(x = group_var, y = pct, group = sentiment), type = 'bar') %>% 
        hc_add_theme(hc_theme_rcharlie) %>% 
        hc_xAxis(title = list(text = '')) %>% 
        hc_colors(c('red', 'green', 'lightblue', 'blue'))
}

########## loading button
withBusyIndicatorUI <- function(button) {
    id <- button[['attribs']][['id']]
    div(
        `data-for-btn` = id,
        button,
        span(
            class = 'btn-loading-container',
            hidden(
                img(src = 'ajax-loader-bar.gif', class = 'btn-loading-indicator'),
                icon('check', class = 'btn-done-indicator')
            )
        ),
        hidden(
            div(class = 'btn-err',
                div(icon('exclamation-circle'),
                    tags$b('Error: '),
                    span(class = 'btn-err-msg')
                )
            )
        )
    )
}

# Call this function from the server with the button id that is clicked and the
# expression to run when the button is clicked
withBusyIndicatorServer <- function(buttonId, expr) {
    # UX stuff: show the "busy" message, hide the other messages, disable the button
    loadingEl <- sprintf('[data-for-btn=%s] .btn-loading-indicator', buttonId)
    doneEl <- sprintf('[data-for-btn=%s] .btn-done-indicator', buttonId)
    errEl <- sprintf('[data-for-btn=%s] .btn-err', buttonId)
    shinyjs::disable(buttonId)
    shinyjs::show(selector = loadingEl)
    shinyjs::hide(selector = doneEl)
    shinyjs::hide(selector = errEl)
    on.exit({
        shinyjs::enable(buttonId)
        shinyjs::hide(selector = loadingEl)
    })
    
    # Try to run the code when the button is clicked and show an error message if
    # an error occurs or a success message if it completes
    tryCatch({
        value <- expr
        shinyjs::show(selector = doneEl)
        shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = 'fade',
                                           time = 0.5))
        value
    }, error = function(err) { errorFunc(err, buttonId) })
}

# When an error happens after a button click, show the error
errorFunc <- function(err, buttonId) {
    errEl <- sprintf('[data-for-btn=%s] .btn-err', buttonId)
    errElMsg <- sprintf('[data-for-btn=%s] .btn-err-msg', buttonId)
    errMessage <- gsub('^ddpcr: (.*)', '\\1', err$message)
    shinyjs::html(html = errMessage, selector = errElMsg)
    shinyjs::show(selector = errEl, anim = TRUE, animType = 'fade')
}

appCSS <- '
.btn-loading-container {
margin-left: 10px;
font-size: 1.2em;
}
.btn-done-indicator {
color: green;
}
.btn-err {
margin-top: 10px;
color: red;
}
'