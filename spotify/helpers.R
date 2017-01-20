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
    
    if (nchar(artist) == 22 & !grepl(' ', artist) & is.numeric(parse_number(substr(artist, 1, 1)))) {
        albums <- GET(paste0('https://api.spotify.com/v1/artists/', artist,'/albums')) %>% content
    } else {
        artists_res <- get_artists(artist)
        artist_input <- readline('Type the name of the artist you want: ')
        artist_uri <- artists_res$artist_uri[artists_res$artist_name == artist_input]
        if (length(artist_uri) == 0) {
            stop(paste0('"', artist_input, '" is not a valid artist. Please choose one of the following artists:\n', paste0(artists_res$artist_name, collapse = '\n')))
        }
        artist <- artist_uri
    }
    
    
    map_df(1:length(albums$items), function(x) {
        tmp <- albums$items[[x]]
        
        # Make sure the album_type is not "single"
        if (tmp$album_type == 'album') {
            data.frame(album_uri = tmp$uri %>% gsub('spotify:album:', '', .),
                       album_name = gsub('\'', '', tmp$name),
                       album_img = albums$items[[x]]$images[[1]]$url,
                       stringsAsFactors = F) %>%
                mutate(album_release_date = GET(paste0('https://api.spotify.com/v1/albums/', tmp$uri %>% gsub('spotify:album:', '', .))) %>% content %>% .$release_date, # yep, you need a separate call to on "albums" to get release date.
                       album_release_year = ifelse(nchar(album_release_date) == 4, year(as.Date(album_release_date, '%Y')), year(as.Date(album_release_date, '%Y-%m-%d'))) # not all album_release_dates have months, so I created album_release year for sorting
                )
        } else {
            NULL
        }
        
    }) %>% filter(!duplicated(tolower(album_name))) %>%  # Sometimes there are multiple versions (just with different capitalizations) of the same album
        arrange(album_release_date) %>%
        mutate(album_rank = match(album_name, unique(album_name)))
}

get_tracks <- function(artist_info, album_info) {
    
    # You'll have to set up a dev account with Spotify here:
    client_id <- 'c857dcec62a74825985e4749ef531abe'
    client_secret <- '46cb88674ec641a0ab124aa190060b70'
    access_token <- POST('https://accounts.spotify.com/api/token',
                         accept_json(), authenticate(client_id, client_secret),
                         body = list(grant_type='client_credentials'),
                         encode = 'form', httr::config(http_version=2)) %>% content %>% .$access_token
    
    track_info <- map_df(album_info$album_uri, function(x) {
        tracks <- GET(paste0('https://api.spotify.com/v1/albums/', x, '/tracks')) %>% 
            content %>% 
            .$items 
        
        uris <- map(1:length(tracks), function(z) {
            gsub('spotify:track:', '', tracks[z][[1]]$uri)
        }) %>% unlist %>% paste0(collapse=',')
        
        res <- GET(paste0('https://api.spotify.com/v1/audio-features/?ids=', uris),
                   query = list(access_token = access_token)) %>% content %>% .$audio_features
        df <- unlist(res) %>% 
            matrix(nrow = length(res), byrow = T) %>% 
            as.data.frame(stringsAsFactors = F)
        names(df) <- names(res[[1]])
        df <- df %>% 
            mutate(album_uri = x,
                   track_number = row_number()) %>% 
            rowwise %>% 
            mutate(track_name = tracks[[track_number]]$name) %>%
            ungroup %>% 
            left_join(album_info, by = 'album_uri') %>% 
            rename(track_uri = id) %>% 
            select(-c(type, track_href, analysis_url, uri))
        return(df)
    }) %>%
        mutate(artist_img = artist_info$artist_img) %>% 
        mutate_at(c('album_uri', 'track_uri', 'album_release_date', 'track_name', 'album_name', 'artist_img'), funs(as.character)) %>%
        mutate_at(c('danceability', 'energy', 'key', 'loudness', 'mode', 'speechiness', 'acousticness', 'album_release_year',
                    'instrumentalness', 'liveness', 'valence', 'tempo', 'duration_ms', 'time_signature', 'track_number'), funs(as.numeric(gsub('[^0-9.-]+', '', as.character(.))))) # for some reason parse_number() from readr doesn't work here
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