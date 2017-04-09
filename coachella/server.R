shinyServer(function(input, output, session) {
    
    
    
})


artist_means <- tracks %>% 
    mutate(mode = as.numeric(mode)) %>% 
    group_by(artist_name, image_url) %>% 
    summarise(danceability = mean(danceability),
              valence = mean(valence),
              energy = mean(energy),
              mode = mean(mode),
              key = n_distinct(key) / n(),
              tempo = mean(tempo),
              loudness = mean(loudness),
              speechiness = mean(speechiness),
              acousticness = mean(acousticness),
              duration_ms = mean(duration_ms),
              instrumentalness = mean(instrumentalness),
              liveness = mean(liveness)) %>% 
    ungroup 


mat <- artist_means %>% 
    select(match(feature_vars, names(.))) %>%
    mutate_all(funs(rescale(., to = c(0,1)))) %>% 
    as.matrix %>% 
    dist

plot(hclust(mat))

map_df(2:20, function(x) {
    cl <- kmeans(mat, x)
    list(
        k = x,
        wss = sum(cl$withinss)
    )
}) %>% 
    hchart(hcaes(x = k, y = wss), type = 'line')

hc <- mat %>% 
    # hclust
    kmeans(6)
# clust <- cutree(hc, k = 8)
clust <- hc$cluster
# hc$labels <- artist_means$artist_name

# hchart(artist_means %>% mutate(cluster = clust), hcaes(x = valence, y = energy, group = cluster), type = 'scatter')

pca <- artist_means %>% 
    select(match(feature_vars, names(.))) %>%
    mutate_all(funs(rescale(., to = c(0,1)))) %>% 
    as.matrix %>% 
    princomp


pca <- prcomp(artist_means[, feature_vars] %>% na.omit, scale = T)

lam <- pca$sdev[1:2]
lam <- lam * sqrt(nrow(pca$x))

hplot <- (pca$x[, 1:2] / lam) %>% 
    as.data.frame %>% 
    mutate(name = artist_means$artist_name,
           cluster = clust,
           tooltip = paste0('<b>Artist:</b> ', name))

dfobs <- (pca$x[, 1:2] / lam) %>% 
    as.data.frame

dfcomp <- pca$rotation[, 1:2] * lam
mx <- max(abs(dfobs[, 1:2]))
mc <- max(abs(dfcomp)) 
dfcomp <- dfcomp %>% 
{ . / mc * mx } %>% 
    as.data.frame() %>% 
    setNames(c("x", "y")) %>% 
    rownames_to_column("name") %>%  
    as_data_frame() %>% 
    group_by_("name") %>%
    do(data = list(c(0, 0), c(.$x, .$y))) %>%
    list_parse

pal <- brewer.pal(6, 'Dark2')

hchart(hplot, hcaes(x = PC1, y = PC2, group = cluster), type = 'scatter') %>%
    hc_add_series_list(dfcomp) %>%
    hc_colors(color = c(pal, rep('lightblue', nrow(pca$rotation)))) %>%
    hc_tooltip(shared = F, formatter = JS(paste0("function() {var text = ''; if ([", paste0(unique(clust), collapse = ', '),"].indexOf(this.series.name) >= 0) {text = this.point.tooltip;} else {text = this.series.name;}return text;}"))) %>%
    hc_title(text = 'The Three Types of Coachella 2017 Artists') %>%
    hc_subtitle(text = HTML('<em>Principle component analysis and clustering of their top tracks on Spotify</em>')) %>% 
    hc_add_theme(hc_theme_smpl())

get_artists('zimmer')

###### threejs
library(threejs)
library(RColorBrewer)
scatterplot3js(pca$x[, 1:3], color=pal[clust], labels=hplot$name, renderer = 'canvas')



pca$rotation
