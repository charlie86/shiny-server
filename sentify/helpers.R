get_artists <- function(artist_name) {
    
    # Search Spotify API for artist name
    res <- GET('https://api.spotify.com/v1/search', query = list(q = artist_name, type = 'artist')) %>%
        content %>% .$artists %>% .$items
    
    # Clean response and combine all returned artists into a dataframe
    artists <- map_df(seq_len(length(res)), function(x) {
        list(
            artist_name = res[[x]]$name,
            artist_uri = gsub('spotify:artist:', '', res[[x]]$uri), # remove meta info from the uri string
            artist_img = ifelse(length(res[[x]]$images) > 0, res[[x]]$images[[1]]$url, NA) # we'll grab this just for fun
        )
    }) %>% filter(!duplicated(tolower(artist_name)))
    
    return(artists)
}

get_albums <- function(artist) {
    
    albums <- GET(paste0('https://api.spotify.com/v1/artists/', artist,'/albums'), query = list(limit = 50)) %>% content
    
    df <- map_df(1:length(albums$items), function(x) {
        tmp <- albums$items[[x]]
        
        Sys.sleep(.1)
        
        # Make sure the album_type is not "single"
        if (tmp$album_type == 'album' & gsub('spotify:artist:', '', tmp$artists[[1]]$uri) == artist) {
            data.frame(album_uri = tmp$uri %>% gsub('spotify:album:', '', .),
                       album_name = gsub('\'', '', tmp$name),
                       album_img = ifelse(length(albums$items[[x]]$images) > 0, albums$items[[x]]$images[[1]]$url, NA),
                       stringsAsFactors = F) %>%
                mutate(album_release_date = GET(paste0('https://api.spotify.com/v1/albums/', tmp$uri %>% gsub('spotify:album:', '', .))) %>% content %>% .$release_date,
                       album_release_year = as.Date(ifelse(nchar(album_release_date) == 4, as.Date(paste0(year(as.Date(album_release_date, '%Y')), '-01-01')), as.Date(album_release_date, '%Y-%m-%d')), origin = '1970-01-01')
                )
        } else {
            NULL
        }
        
    })
    
    if (nrow(df) > 0) {
        df <- df %>% filter(!duplicated(tolower(album_name))) %>%
            mutate(base_album_name = str_replace_all(tolower(album_name), ' \\(.*(deluxe|international|anniversary|version|edition|remaster|live|mono|stereo).*\\)', ''),
                   base_album_name = str_replace_all(base_album_name, ' \\[.*(deluxe|international|anniversary|version|edition|remaster|live|mono|stereo).*\\]', ''),
                   base_album_name = str_replace_all(base_album_name, ':.*(deluxe|international|anniversary|version|edition|remaster|live|mono|stereo).*', ''),
                   base_album_name = str_replace_all(base_album_name, ' - .*(deluxe|international|anniversary|version|edition|remaster|live|mono|stereo).*', '')) %>% 
            group_by(base_album_name) %>% 
            filter(album_release_year == min(album_release_year)) %>% 
            mutate(base_album = tolower(album_name) == base_album_name,
                   num_albums = n(),
                   num_base_albums = sum(base_album)) %>% 
            filter((num_base_albums == 1 & base_album == 1) | ((num_base_albums == 0 | num_base_albums > 1) & row_number() == 1)) %>%
            ungroup %>% 
            # Sometimes there are multiple versions (just with different capitalizations) of the same album
            arrange(album_release_year) %>%
            mutate(album_rank = row_number())
    }
    
    return(df)
}

get_tracks <- function(artist_info, album_info) {
    
    # You'll have to set up a dev account with Spotify here:
    client_id <- 'c857dcec62a74825985e4749ef531abe'
    client_secret <- '54af922e8c7a44f28eb339adb0f23656'
    access_token <- POST('https://accounts.spotify.com/api/token',
                         accept_json(), authenticate(client_id, client_secret),
                         body = list(grant_type='client_credentials'),
                         encode = 'form', httr::config(http_version=2)) %>% content %>% .$access_token
    
    track_info <- map_df(album_info$album_uri, function(x) {
        tracks <- GET(paste0('https://api.spotify.com/v1/albums/', x, '/tracks')) %>% 
            content %>% 
            .$items 
        
        Sys.sleep(.1)
        
        uris <- map(1:length(tracks), function(z) {
            gsub('spotify:track:', '', tracks[z][[1]]$uri)
        }) %>% unlist %>% paste0(collapse=',')
        
        res <- GET(paste0('https://api.spotify.com/v1/audio-features/?ids=', uris),
                   query = list(access_token = access_token)) %>% content %>% .$audio_features
        
        audio_features <- c('danceability', 'energy', 'key', 'loudness', 'mode', 'speechiness', 'acousticness', 'instrumentalness', 'liveness', 'valence', 'tempo', 'id', 'duration_ms', 'time_signature')
        
        df <- map_df(1:length(res), function(i) {
            tmp <- map(audio_features, function(j) {
                ifelse(is.null(res[[i]][[j]]), NA, res[[i]][[j]])
            })
            names(tmp) <- audio_features
            tmp
        })
        
        # df <- unlist(res) %>% 
        #     matrix(nrow = length(res), byrow = T) %>% 
        #     as.data.frame(stringsAsFactors = F)
        # names(df) <- names(res[[1]])
        df <- df %>% 
            mutate(album_uri = x,
                   track_number = row_number()) %>% 
            rowwise %>% 
            mutate(track_name = tracks[[track_number]]$name) %>%
            ungroup %>% 
            left_join(album_info, by = 'album_uri') %>% 
            rename(track_uri = id) #%>% 
        # select(-c(type, track_href, analysis_url, uri))
        return(df)
    }) 
    
    if (nrow(track_info) > 0) {
        track_info <- track_info %>%
            mutate(artist_name = as.character(artist_info$artist_name),
                   artist_img = artist_info$artist_img) %>% 
            mutate_at(c('album_uri', 'track_uri', 'album_release_date', 'track_name', 'album_name', 'artist_img'), funs(as.character)) %>%
            mutate_at(c('danceability', 'energy', 'key', 'loudness', 'mode', 'speechiness', 'acousticness', 'album_release_year',
                        'instrumentalness', 'liveness', 'valence', 'tempo', 'duration_ms', 'time_signature', 'track_number'), funs(as.numeric(gsub('[^0-9.-]+', '', as.character(.))))) # for some reason parse_number() from readr doesn't work here
    }
    return(track_info)
}

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
        hc_add_theme(hc_theme_smpl())
    album_chart
}

quadrant_chart <- function(track_df) {
    
    df2 <- data.frame(x = c(0, 1, 0, 1),
                      y = c(1, 1, 0, 0),
                      text = c('Angry (high energy, low valence)',
                               'Joyful (high energy, high valence)',
                               'Sad (low energy, low valence)',
                               'Peaceful (low energy, high valence)'))
    
    ds2 <- list_parse(df2)
    
    if (n_distinct(track_df$album_name) > 21) {
        my_colors <- tol21rainbow
    } else {
        my_colors <- sample(tol21rainbow, n_distinct(track_df$album_name))
    }
    
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