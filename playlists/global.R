library(shiny)
library(stringr)
library(shinydashboard)
library(highcharter)
library(httr)
library(DT)
library(tidyverse)
library(RColorBrewer)

client_id <- 'c857dcec62a74825985e4749ef531abe'
client_secret <- '54af922e8c7a44f28eb339adb0f23656'
access_token <- POST('https://accounts.spotify.com/api/token',
                     accept_json(), authenticate(client_id, client_secret),
                     body = list(grant_type='client_credentials'),
                     encode = 'form', httr::config(http_version=2)) %>% content %>% .$access_token

base_url <- 'https://api.spotify.com/v1/'

famous_users <- list(
    'Barack Obama' = 'barackobama', 
    'Snoop Dogg' = 'snoopdogg',
    'Jonny Greenwood' = 'mrmarmite',
    'Jamie xx' = 'jamiexx',
    'Daft Punk' = 'daftpunkofficial',
    'Kendrick Lamar' = 'kendricklamar'
)

## helpers
get_user_playlists <- function(user) {
    base_url <- 'https://api.spotify.com/v1/'
    user_search_query <- paste0(base_url, 'users/', user, '/playlists')    
    user_playlists <- GET(user_search_query,
                          query = list(access_token = access_token, limit = 50)) %>% content %>% .$items
    
    playlist_df <- map_df(1:length(user_playlists), function(x) {
        list(
            playlist_name = user_playlists[[x]]$name,
            playlist_img = user_playlists[[x]]$images[[1]]$url,
            playlist_num_tracks = user_playlists[[x]]$tracks$total,
            playlist_tracks_url = user_playlists[[x]]$tracks$href
        )
    })
    
    return(playlist_df)
}

get_playlist_tracks <- function(playlists) {
    map_df(1:nrow(playlists), function(x) {
        
        loops <- ceiling(playlists$playlist_num_tracks[x] / 100)
        
        map_df(1:loops, function(y) {
            
            res <- GET(playlists$playlist_tracks_url[x], query = list(access_token = access_token, limit = 100, offset = (100*y)-100)) %>% content %>% .$items
            
            track_info <- map_df(1:length(res), function(z) {
                if (!is.null(res[[z]]$track$id)) {
                    list(
                        playlist_name = playlists$playlist_name[x],
                        playlist_img = playlists$playlist_img[x],
                        track_name = res[[z]]$track$name,
                        track_uri = res[[z]]$track$id,
                        artist_name = res[[z]]$track$artists[[1]]$name,
                        album_name = res[[z]]$track$album$name,
                        album_img = ifelse(length(res[[z]]$track$album$images) > 0, res[[z]]$track$album$images[[1]]$url, '')
                    )
                }
            })
        })
    })
}

get_track_audio_features <- function(tracks) {
    map_df(1:ceiling(nrow(tracks %>% filter(!duplicated(track_uri))) / 100), function(x) {
        uris <- tracks %>%
            filter(!duplicated(track_uri)) %>%
            slice((x * 100) - 99:x) %>%
            select(track_uri) %>%
            .[[1]] %>%
            paste0(collapse = ',')
        
        res <- GET(paste0('https://api.spotify.com/v1/audio-features/?ids=', uris),
                   query = list(access_token = access_token)) %>% content %>% .$audio_features
        
        df <- unlist(res) %>%
            matrix(nrow = length(res), byrow = T) %>%
            as.data.frame(stringsAsFactors = F)
        names(df) <- names(res[[1]])
        df
    }) %>% select(-c(type, uri, track_href, analysis_url)) %>%
        rename(track_uri = id)
}

search_artist <- function(artist_name) {
    
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