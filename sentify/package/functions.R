library(tidyverse)
library(httr)
library(lubridate)

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

get_artist_info <- function(artist, first_artist = T) {
    artist_info <- get_artists(artist)
    if (first_artist) {
        artist_info <- artist_info %>% slice(1)
    } else {
        print(artists_res$artist_name)
        artist_input <- tolower(readline('Type the name of the artist you want: '))
        artist_info <- artist_info %>% filter(tolower(artist_name) == tolower(artist_input))
        if (nrow(artist_info) == 0) {
            stop(paste0('"', artist_input, '" is not a valid artist. Please choose one of the following artists:\n', paste0(artists_res$artist_name, collapse = '\n')))
        }
    }
    return(artist_info)
}
get_artist_info('radiohead')

get_album_info <- function(artist_info) {
    albums <- GET(paste0('https://api.spotify.com/v1/artists/', artist_info$artist_uri,'/albums')) %>% content
    
    album_info <- map_df(1:length(albums$items), function(x) {
        tmp <- albums$items[[x]]
        
        # Make sure the album_type is not "single"
        if (tmp$album_type == 'album') {
            data.frame(album_uri = tmp$uri %>% gsub('spotify:album:', '', .),
                       album_name = gsub('\'', '', tmp$name),
                       album_img = albums$items[[x]]$images[[1]]$url,
                       stringsAsFactors = F) %>%
                mutate(album_release_date = GET(paste0('https://api.spotify.com/v1/albums/', tmp$uri %>% gsub('spotify:album:', '', .))) %>% content %>% .$release_date)
        } else {
            NULL
        }
        
    }) %>% filter(!duplicated(tolower(album_name))) %>%  # Sometimes there are multiple versions (just with different capitalizations) of the same album
        arrange(album_release_date) %>%
        mutate(album_rank = match(album_name, unique(album_name)))
    return(album_info)
}

get_discog_track_info <- function(artist_info, album_info) {
    
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
        
        uris <- map(1:length(tracks), function(y) {
            gsub('spotify:track:', '', tracks[y][[1]]$uri)
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
        mutate(artist_img = artist_info$artist_img,
               artist_name = artist_info$artist_name) %>% 
        mutate_at(c('album_uri', 'track_uri', 'album_release_date', 'track_name', 'album_name', 'artist_img'), funs(as.character)) %>%
        mutate_at(c('danceability', 'energy', 'key', 'loudness', 'mode', 'speechiness', 'acousticness',
                    'instrumentalness', 'liveness', 'valence', 'tempo', 'duration_ms', 'time_signature', 'track_number'), funs(as.numeric(gsub('[^0-9.-]+', '', as.character(.))))) %>% 
        select(artist_name, album_name, album_release_date, track_name, everything())
    return(track_info)
    
}

get_discog_info <- function(artist, first_artist = T) {
    artist_info <- get_artist_info(artist = artist, first_artist = first_artist)
    album_info <- get_album_info(artist_info)
    get_discog_track_info(artist_info, album_info)
}
get_discog_info('radiohead')




user_id <- '1238601781'

GET(paste0('https://api.spotify.com/v1/users/', user_id, '/playlists'),
    query = list(access_token = access_token)) %>% content

get_user_playlists <- function(user_id) {
    client_id <- 'c857dcec62a74825985e4749ef531abe'
    client_secret <- '46cb88674ec641a0ab124aa190060b70'
    access_token <- POST('https://accounts.spotify.com/api/token',
                         accept_json(), authenticate(client_id, client_secret),
                         body = list(grant_type='client_credentials'),
                         encode = 'form', httr::config(http_version=2)) %>% content %>% .$access_token
    GET(paste0('https://api.spotify.com/v1/users/', user_id, '/playlists'),
        query = list(access_token = access_token)) %>% content %>% .$items
}

playlists <- get_user_playlists(user_id)

get_playlist_tracks <- function(playlists) {
    
    client_id <- 'c857dcec62a74825985e4749ef531abe'
    client_secret <- '46cb88674ec641a0ab124aa190060b70'
    access_token <- POST('https://accounts.spotify.com/api/token',
                         accept_json(), authenticate(client_id, client_secret),
                         body = list(grant_type='client_credentials'),
                         encode = 'form', httr::config(http_version=2)) %>% content %>% .$access_token
    map_df(1:length(playlists), function(x) {
        playlist_uri <- playlists[[3]]$tracks$href
        tracks <- GET(playlist_uri, query = list(access_token = access_token)) %>% content %>% .$items
        track_uris <- map(1:length(tracks), function(y) {
            gsub('spotify:track:', '', tracks[[y]]$track$uri)
        }) %>% unlist %>% paste0(collapse=',')
        res <- GET(paste0('https://api.spotify.com/v1/audio-features/?ids=', track_uris),
            query = list(access_token = access_token)) %>% content %>% .$audio_features
        df <- unlist(res) %>% 
            matrix(nrow = length(res), byrow = T) %>% 
            as.data.frame(stringsAsFactors = F)
        names(df) <- names(res[[1]])
        df$playlist_img <- playlists[[x]]$images[[1]]$url
        df <- df %>% 
            mutate(track_number = row_number(),
                   playlist_name = playlists[[x]]$name) %>% 
            rowwise %>% 
            mutate(track_name = tracks[[track_number]]$track$name,
                   artist_name = tracks[[track_number]]$track$artists[[1]]$name) %>%
            ungroup
        return(df)
    }) %>% mutate_at(c('danceability', 'energy', 'key', 'loudness', 'mode', 'speechiness', 'acousticness',
                    'instrumentalness', 'liveness', 'valence', 'tempo', 'duration_ms', 'time_signature', 'track_number'), funs(as.numeric(gsub('[^0-9.-]+', '', as.character(.))))) %>% 
        select(playlist_name, track_name, artist_name, everything())
}
playlists <- get_user_playlists('1231546658')
df <- get_playlist_tracks(playlists)

df %>% group_by(playlist_name) %>% summarise(valence = mean(valence)) %>% arrange(valence)

GET(paste0('https://api.spotify.com/v1/users/pepperwater'),
    query = list(access_token = access_token)) %>% content %>% .$items

tracks <- GET(paste0('https://api.spotify.com/v1/users/1238601781/playlists/2KpF5IBEKcuppqYa6EHGEs/tracks'),
              query = list(access_token = access_token)) %>% content %>% .$items