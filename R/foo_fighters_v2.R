devtools::install_github('charlie86/spotifyr')
library(spotifyr)
library(tidyverse)
library(knitr)
library(beeswarm)
library(reshape2)
library(plotly)
devtools::install_github("josiahparry/geniusR")
library(geniusR)
library(dplyr) #data manipulation
library(ggplot2) #visualizations
library(gridExtra) #viewing multiple plots together
library(tidytext) #text mining
library(wordcloud2) #creative visualizations

Sys.setenv(SPOTIFY_CLIENT_ID = '58f53eb1c69d4bd3a4aa2abd2ea265a1')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '10b381cbeab548eb80d6f837be01e41e')

Sys.setenv(GENIUS_API_TOKEN = 'BOqFxFmHKNRwYEJuf0X5etaeaUhaZxNWiNGhYOqkvrLnh9STzd3hPgiYfvIv7w90')

accesstoken <- get_spotify_access_token()

# Spotify Artist : Foo Fighters
# spotify:artist:7jy3rLJdDQY21OgRLCZ9sD

#Extract data from spotify
ff_albums <- get_artist_albums(artist = 'Foo Fighters')
ff_meta <- get_albums(albums = ff_albums$album_uri)
ff_tracks <- get_album_tracks(ff_albums)
ff_audio_analysis <- data.frame(get_track_audio_analysis(ff_tracks$track_uri[1]))
ff_audio_features <- data.frame(get_track_audio_features(ff_tracks))

# using melt to reshape the columns in top_tracks into rows
# categories - danceability, energy, loudness, speechiness, acousticness, instrumentalness, 
# liveness, valence, tempo
ff_audio_melt <- melt(ff_audio_features, id=(c("track_uri","duration_ms","time_signature","key_mode")))
# remove rows where variable = key or mode, not interested in those
ff_audio_melt <- ff_audio_melt %>%
    filter(!(variable %in% c("key","mode")))

# set the value column to numeric
ff_audio_melt$value<-as.numeric(ff_audio_melt$value)
min_tempo <<- min(ff_audio_features$tempo)
min_dance <<- min(ff_audio_features$danceability)
min_energy <<- min(ff_audio_features$energy)
min_loudness <<- min(ff_audio_features$loudness)
min_speechiness <<- min(ff_audio_features$speechiness)
min_acoustic <<- min(ff_audio_features$acousticness)
min_instru <<- min(ff_audio_features$instrumentalness)
min_valence <<- min(ff_audio_features$valence)
min_live <<- min(ff_audio_features$liveness)
# max's
max_tempo <<- max(ff_audio_features$tempo)
max_dance <<- max(ff_audio_features$danceability)
max_energy <<- max(ff_audio_features$energy)
max_loudness <<- max(ff_audio_features$loudness)
max_speechiness <<- max(ff_audio_features$speechiness)
max_acoustic <<- max(ff_audio_features$acousticness)
max_instru <<- max(ff_audio_features$instrumentalness)
max_valence <<- max(ff_audio_features$valence)
max_live <<- max(ff_audio_features$liveness)

# function for normalising the data, formula => (value - min) / (max - min)
norm_data <- function(var1,x_val) {
    if (var1=="tempo") {
        norm_val <- (x_val-min_tempo)/(max_tempo-min_tempo)
    }
    if (var1=="liveness") {
        norm_val <- (x_val-min_live)/(max_live-min_live)
    }
    if (var1=="danceability") {
        norm_val <- (x_val-min_dance)/(max_dance-min_dance)
    }
    if (var1=="energy") {
        norm_val <- (x_val-min_energy)/(max_energy-min_energy)
    }
    if (var1=="loudness") {
        norm_val <- (x_val-min_loudness)/(max_loudness-min_loudness)
    }
    if (var1=="speechiness") {
        norm_val <- (x_val-min_speechiness)/(max_speechiness-min_speechiness)
    }
    if (var1=="acousticness") {
        norm_val <- (x_val-min_acoustic)/(max_acoustic-min_acoustic)
    }
    if (var1=="instrumentalness") {
        norm_val <- (x_val-min_instru)/(max_instru-min_instru)
    }
    if (var1=="valence") {
        norm_val <- (x_val-min_valence)/(max_valence-min_valence)
    }
    return(norm_val)
}

# normalise the audio values for the tt_audio_melt dataframe
for (row in 1:nrow(ff_audio_melt)) {
    # print(tt_audio_melt$value)
    ff_audio_melt$norm_value[row] <- norm_data(ff_audio_melt$variable[row],ff_audio_melt$value[row])
}

ff_sorted <- ff_audio_melt %>%
    group_by(variable) %>%
    summarise(m = median(norm_value)) %>%
    arrange(m) %>%
    .[["variable"]]

# Create Boxplot for audio features on top tracks, data is normalised
# -------------------------------------------------------------------
p <- plot_ly(data = ff_audio_melt, x = ~factor(variable,ff_sorted), y = ~norm_value, type = "box", 
             color = ~variable, boxpoints = "all", jitter = 0.3, pointpos = -1.8)
p

# Lyrics Analysis - utilising GeniusR library
song_ids <- c()
no_data<-"N"
for (row in 1:nrow(ff_tracks)) {
    no_data<-"N"
    tryCatch({
        lyrics <- genius_lyrics(artist = "Foo Fighters", song = ff_tracks$track_name[row])
    },error=function(e) {
        no_data<-"Y"},
    finally = {
        if (no_data=="N") {
            if (nrow(lyrics)>0) {
                if (row==1) {
                    lyrics$song_id <- row
                    lyrics_ds <- lyrics
                }
                if (row>1) {
                    lyrics$song_id <- row
                    lyrics_ds <- rbind(lyrics_ds,lyrics)
                }
                lyrics <- lyrics[0,]
                song_ids <- c(song_ids, row)
            }
        }
        no_data<-"N"})
}

# Sentiment Analysis
l_analyse <- lyrics_ds %>%
    select(lyric,song_id) %>%
    group_by(song_id) %>%
    unnest_tokens(word,lyric)

nrc_joy <- get_sentiments("nrc") %>%
    filter(sentiment == "joy")
nrc_anger <- get_sentiments("nrc") %>%
    filter(sentiment == "anger")
nrc_sent <- get_sentiments("nrc")
afinn_sent <- get_sentiments("afinn")
bing_sent <- get_sentiments("bing")

# look for joy words
words_joy <- l_analyse %>%
    inner_join(nrc_joy) %>%
    count(word, sort = TRUE)

# look for anger words
words_anger <- l_analyse %>%
    inner_join(nrc_anger) %>%
    count(word, sort = TRUE)  

total_words <- rbind(words_joy,words_anger)

# analysing the JOY words
s_analysis <- total_words %>%
    inner_join(nrc_sent) 
sentiments_full <- s_analysis %>%
    count(sentiment, sort = TRUE) %>%
    arrange(song_id,sentiment)

s_analysis_bing <- total_words %>%
    inner_join(bing_sent) 
sentiments_bing <- s_analysis_bing %>%
    count(sentiment, sort = TRUE) %>%
    arrange(song_id,sentiment)

s_analysis_afinn <- total_words %>%
    inner_join(afinn_sent) 
sentiments_afinn <- s_analysis_afinn %>%
    count(score, sort = TRUE) %>%
    arrange(song_id,score)

# Plots of Sentiment Analysis
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# To use for fills, add
scale_fill_manual(values=cbPalette)
ggplot(sentiments_full, aes(sentiment, nn, fill = sentiment)) +
    geom_col() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, face="bold")) +
    theme(
        plot.title = element_text(color="red", size=9, face="bold.italic"),
        axis.title.x = element_text(color="black", size=9, face="bold"),
        axis.title.y = element_text(color="black", size=9, face="bold")
    )

ggplot(sentiments_afinn, aes(score, nn, fill = score)) +
    geom_col() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, face="bold")) +
    theme(
        plot.title = element_text(color="red", size=9, face="bold.italic"),
        axis.title.x = element_text(color="black", size=9, face="bold"),
        axis.title.y = element_text(color="black", size=9, face="bold")
    )

ggplot(sentiments_bing, aes(sentiment, nn, fill = sentiment)) +
    geom_col() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, face="bold")) +
    theme(
        plot.title = element_text(color="red", size=9, face="bold.italic"),
        axis.title.x = element_text(color="black", size=9, face="bold"),
        axis.title.y = element_text(color="black", size=9, face="bold")
    )
# ----------------------------------

# ----------------------------------------------
# Most common positive and negative words
bing_word_counts <- l_analyse %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()

bing_word_counts %>%
    group_by(sentiment) %>%
    top_n(5) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Contribution to sentiment",
         x = NULL) +
    coord_flip()
# ----------------------------------------------
twords <- total_words[c('word','n')]
twords %>%
    anti_join(stop_words) %>%
    count(word) %>%
    wordcloud2(word, size=1.6, color='random-light', backgroundColor="black")

