# 2.1: Spotify Data Retrieval --------------------------------------------------

# Load packages required for this session into library

library(Rspotify)
library(spotifyr)
library(magrittr)
library(igraph)
library(dplyr)
library(knitr)
library(ggplot2)
library(ggridges)
library(httpuv)


# Configure application to store Spotify authentication data in cache

options(httr_oauth_cache = TRUE)


# Set up authentication variables

app_id <- "fb0cf199164b46638545119ee76c9409"
app_secret <- "14cc3f5cf88348c0a5069f8ef21a17f1"
token <- "1"


# Authentication for Rspotify package:

keys <- spotifyOAuth(token, app_id, app_secret) 


# Authentication for spotifyr package:

Sys.setenv(SPOTIFY_CLIENT_ID = app_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = app_secret)
access_token <- get_spotify_access_token()


# Get Spotify data on 'Drake'

find_my_artist <- searchArtist("Drake", token = keys)
View(find_my_artist)
drakeId <- find_my_artist$id[1]


# Retrieve information about the artist Drake

my_artist <- getArtist(drakeId, token = keys)
View(my_artist)


# Retrieve album data of the artist Drake

albums <- getAlbums(drakeId, token = keys)
View(albums)


# Creating Album and Song Lists

allAlbumsList <- data.frame()
allSongsList <- data.frame()

for (i in albums$id) {
  
  albumInfo <- getAlbumInfo(i, token = keys)
  allAlbumsList <- rbind(allAlbumsList, albumInfo)
  
  albumSongs <- get_album_tracks(i, limit = 50)
  allSongsList <- rbind(allSongsList, albumSongs)
  
}


# How many years has Drake been active?

firstAlbumReleaseDate <- as.numeric(allAlbumsList[allAlbumsList$release_date == min(allAlbumsList$release_date), ]$release_date)
current_year <- as.numeric(format(Sys.Date(), "%Y"))
years_active <- current_year - firstAlbumReleaseDate
print(paste("Drake has been active for", years_active, "years"))


# How many albums has Drake published?

allAlbumsList <- allAlbumsList[order(allAlbumsList[, 'name'], -allAlbumsList[, 'popularity']), ]
unique_albums <- allAlbumsList[!duplicated(allAlbumsList$name), ]
print(paste("Drake has published", nrow(unique_albums), "albums"))


# How many songs has Drake published?

unique_songs <- allSongsList[!duplicated(allSongsList$name), ]
print(paste("Drake has published", nrow(unique_songs), "songs"))


# Get Spotify catalog information for each song

allTracksList <- data.frame()

for (i in allSongsList$id) {
  allTracks <- getTrack(i, token = keys)
  allTracksList <- rbind(allTracksList, allTracks)
}

# Visualise the popularity distribution of the tracks

allTracksList <- allTracksList[!duplicated(allTracksList$name), ]
hist(allTracksList$popularity, main="Histogram of Drake's Track Popularity", xlab="Popularity Score", col=4)
print(mean(allTracksList$popularity))


# Find the Top Ten Most Popular Tracks for Australian Users

topTenTracks <- getTopTracks(drakeId, country = "AU", token = keys)
barplot(topTenTracks$popularity, names=topTenTracks$name, main = "Top Ten Most Popular Drake Songs In Australia", 
        xlab = "Name", ylab = "Popularity", col = 2, cex.names = 0.75)
print(mean(topTenTracks$popularity))


# Get audio features for 'Drake'

audio_features <- get_artist_audio_features("Drake")
View(audio_features)

audio_features <- audio_features[!duplicated(audio_features$track_name), ]


# With Whom has Drake Often Collaborated?

collaborations <- list()

for (i in 1:nrow(audio_features)) {
  track_artists <- audio_features$artists[[i]]$name
  collaborations[[length(collaborations) + 1]] <- track_artists[track_artists != "Drake"]
}

collaborations <- collaborations[lengths(collaborations) != 0]

collaborating_artists <- list()

for (i in 1:length(collaborations)) {
  if (length(collaborations[[i]]) == 1) {
    collaborating_artists <- append(collaborating_artists, collaborations[[i]])
  }
  if (length(collaborations[[i]]) > 1) {
    for (j in 1:length(collaborations[[i]])) {
      collaborating_artists <- append(collaborating_artists, collaborations[[i]][[j]])
    }
  }
}

collaborating_artists <- unlist(collaborating_artists)
collaboration_counts <- table(collaborating_artists)
collaboration_counts <- sort(collaboration_counts, decreasing = TRUE)
print(collaboration_counts)

barplot(collaboration_counts[1:9], main = "Artists with whom Drake has Often Collaborated With", 
        xlab = "Artists", ylab = "Frequency", col = "#BC13FE")


# What are the prevalent features of Drake's songs?

topTenTracksFeaturesList <- data.frame()

for (i in topTenTracks$artist_id) {
  topTenTracksFeatures <- getFeatures(i, token=keys)
  topTenTracksFeaturesList <- rbind(topTenTracksFeaturesList, topTenTracksFeatures)
}

barplot(topTenTracksFeaturesList$danceability, names=topTenTracks$name, main = "Danceability in Top 10 Drake Songs", 
        xlab = "Song Name", ylab = "Danceability Score", col = "#D4FF47", cex.names = 0.75)
print(mean(topTenTracksFeaturesList$danceability))

barplot(topTenTracksFeaturesList$energy, names=topTenTracks$name, main = "Energy in Top 10 Drake Songs", 
        xlab = "Song Name", ylab = "Energy Score", col = "#FFCC33", cex.names = 0.75)
print(mean(topTenTracksFeaturesList$energy))


# Plot danceability scores for each album

ggplot(audio_features, aes(x = danceability, y = album_name)) +
  geom_density_ridges(fill = "#D4FF47", alpha = 0.5) +
  theme_ridges() +
  ggtitle("Danceability in Drake's Albums",
          subtitle = "Based on Danceability from Spotify's Web API")


# Plot energy scores for each album

ggplot(audio_features, aes(x = energy, y = album_name)) +
  geom_density_ridges(fill = "#FFCC33", alpha = 0.5) +
  theme_ridges() +
  ggtitle("Energy in Drake's Albums",
          subtitle = "Based on Energy from Spotify's Web API")


# Retrieve information about related artists

related_bm <- getRelated("Drake", token = keys)
View(related_bm)


# ----------

# 2.2: Youtube Views/Likes -----------------------------------------------------

# Load packages required for this session into library

library(tuber)
library(vosonSML)
library(magrittr)
library(igraph)
library(httpuv)
library(dplyr)
library(ggplot2)


# Set up YouTube authentication variables 

api_key <- "AIzaSyAJVwmcbnLXqPdeCb-VoMPOi7-CCVsRpmE"
client_id <- "332504751343-sk433ia89jeghlefvpt82sidmiv870k9.apps.googleusercontent.com"
client_secret <- "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"


# Authenticate to YouTube using the tuber package

yt_oauth(app_id = client_id, app_secret = client_secret)


# Search YouTube

video_search <- yt_search("Drake")
View(video_search)


# Get statistics for each video from video_search

videoSearchList <- data.frame()

for (i in video_search$video_id) {
  videoStats <- get_stats(video_id = i)
  videoSearchList <- bind_rows(videoSearchList, videoStats)
}

j <- c(2, 3)
videoSearchList[ , j] <- apply(videoSearchList[ , j], 2,
                    function(x) as.numeric(as.character(x)))


# Which videos have the highest number of views and likes?

sortedVideoSearchList <- arrange(videoSearchList, desc(viewCount), desc(likeCount))

topFiveViewsLikesIds <- as.vector(unique(sortedVideoSearchList$id[1:5]))

topFiveViewsLikesTitlesList <- c()

for (i in topFiveViewsLikesIds) {
  topFiveViewsLikesTitles <- video_search[video_search$video_id %in% i,]$title
  topFiveViewsLikesTitlesList <- append(topFiveViewsLikesTitlesList, topFiveViewsLikesTitles)
}

print(topFiveViewsLikesTitlesList)


# Do you see a correlation between views and likes?

ggplot(sortedVideoSearchList, aes(x = viewCount, y = likeCount)) +
  geom_point() +
  stat_smooth() +
  ggtitle("Relationship Between Youtube Views and Likes") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Views") + ylab("Likes")

corrCoeff <- cor(sortedVideoSearchList$viewCount, sortedVideoSearchList$likeCount, use = "complete.obs")
print(paste("r =", corrCoeff))


# Choose 10 videos and store their video IDs,
# for which we want to collect comments
# and build an actor network

video_ids_list <- c()

for (i in 1:10) {
  video_ids <- video_search$video_id[i]
  video_ids_list <- append(video_ids_list, video_ids)
}

yt_data <- Authenticate("youtube", apiKey = api_key) %>%
  Collect(videoIDs = video_ids_list,
          writeToFile = TRUE,
          maxComments = 500,
          verbose = TRUE)

View(yt_data)

yt_actor_network_drake <- yt_data_drake %>% Create("actor")
yt_actor_graph_drake <- Graph(yt_actor_network_drake)


# ----------

# 2.3: Pre-processing & term-document matrix & top 10 terms --------------------

# Load packages required for this session into library

library(remotes)
library(igraph)
library(vosonSML)
library(magrittr)
library(tidyr)
library(tidytext)
library(stopwords)
library(textclean)
library(qdapRegex)
library(tm)
library(SnowballC)
library(ggplot2)


# Set up Twitter authentication variables

my_app_name <- "LiamBarryBigData"
my_api_key <- "vcyiO3xmm2R0SlQeUeYjWM5Zz"
my_api_secret <- "oUc42X1BgWbgjgOKmhtrsMiqVIwEhcIF3m5AsExDNxOemGq7Va"
my_access_token <- "1632609860960542722-AcRxp75HzcUlMMzYTqlIThlCioNSk8"
my_access_token_secret <- "EXjDeTOk0cvC361UA2wgbXPWdJztlJmdIx8GL9khx8znw"


# Authenticate to Twitter and collect data

twitter_data <- Authenticate("twitter",
                             appName = my_app_name,
                             apiKey = my_api_key,
                             apiSecret = my_api_secret,
                             accessToken = my_access_token,
                             accessTokenSecret = my_access_token_secret) %>%
  Collect(searchTerm = "Drake OR \"Honestly Nevermind\" OR #drake OR #drizzy OR #champagnepapi OR #6god
          OR #itsallablurtour OR #draketickets OR #teamdrizzy",
          searchType = "mixed",
          numTweets = 8000,
          lang = "en",
          includeRetweets = TRUE,
          writeToFile = TRUE,
          verbose = TRUE) # use 'verbose' to show download progress


# Merge Collected Data From Milestone 1 and Milestone 2

Milestone1_twitter <- readRDS("Milestone1_twitter.rds")
my8000tweets <- readRDS("my8000tweets.rds")

fullTwitterData <- Merge(Milestone1_twitter, my8000tweets, writeToFile = TRUE)


# Clean the tweet text

clean_text <- fullTwitterData$tweets$text %>% 
  rm_twitter_url() %>% 
  replace_url() %>% 
  replace_hash() %>% 
  replace_tag() %>% 
  replace_emoji() %>% 
  replace_emoticon()


# Convert clean_text vector into a document corpus (collection of documents)

text_corpus <- VCorpus(VectorSource(clean_text))

text_corpus[[100]]$content
text_corpus[[1000]]$content


# Perform further pre-processing 

text_corpus <- text_corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeWords, stopwords(kind = "SMART")) %>% 
  tm_map(stemDocument) %>% 
  tm_map(stripWhitespace)

text_corpus[[100]]$content
text_corpus[[1000]]$content


# Transform corpus into a Document Term Matrix

doc_term_matrix <- DocumentTermMatrix(text_corpus)


# Sort words by total frequency across all documents

dtm_df <- as.data.frame(as.matrix(doc_term_matrix))
View(dtm_df)

freq <- sort(colSums(dtm_df), decreasing = TRUE)

head(freq, n = 10)



# Run Page Rank algorithm to find important terms/hashtags (from 1.4)

# Create the network and graph: 
# - with 25% of the most frequent terms (before was the default of 5%)
# - with 50% of the most frequent hashtags
# - removing the actual search terms

tw_sem_nw_more_terms <- fullTwitterData %>%
  Create("semantic",
         termFreq = 25,
         hashtagFreq = 50,
         removeTermsOrHashtags = c("Drake", "Honestly Nevermind", "#drake", 
                                   "#drizzy", "#champagnepapi", "#6god", 
                                   "#itsallablurtour", "#draketickets", "#teamdrizzy"))

tw_sem_graph_more_terms <- tw_sem_nw_more_terms %>% Graph()

new_rank_twitter_semantic <- sort(page_rank(tw_sem_graph_more_terms)$vector, decreasing = TRUE)
topTenTerms <- head(new_rank_twitter_semantic, n = 10)

print(topTenTerms)


word_frequ_df <- data.frame(word = names(freq), freq)
View(word_frequ_df)

plot1 <- ggplot(subset(word_frequ_df, freq > 1690), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Top 10 Terms from 2.3") +
  theme(axis.title.x=element_blank()) +
  ylab("Frequency") +
  geom_col(fill = "#0099f9")


plot2 <- data.frame(names = factor(names(topTenTerms), levels = names(topTenTerms)), topTenTerms) %>%
  ggplot(aes(names, topTenTerms)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Top 10 Terms From 1.4") +
  theme(axis.title.x=element_blank()) +
  ylab("Page Rank Score") +
  geom_col(fill = "#FF3131")


library(gridExtra)
library(grid)

grid.arrange(plot1, plot2, nrow = 1, ncol = 2,
             top = textGrob("Comparison Plot",gp=gpar(fontsize=24, font=4)),
             bottom = textGrob("Terms", gp=gpar(fontsize=18)))


# ----------

# 2.4: Centrality Analysis -----------------------------------------------------

# Load packages required for this session into library

library(vosonSML)
library(magrittr)
library(tidytext)
library(igraph)


# Set up Twitter authentication variables

my_app_name <- "LiamBarryBigData"
my_api_key <- "vcyiO3xmm2R0SlQeUeYjWM5Zz"
my_api_secret <- "oUc42X1BgWbgjgOKmhtrsMiqVIwEhcIF3m5AsExDNxOemGq7Va"
my_access_token <- "1632609860960542722-AcRxp75HzcUlMMzYTqlIThlCioNSk8"
my_access_token_secret <- "EXjDeTOk0cvC361UA2wgbXPWdJztlJmdIx8GL9khx8znw"


# Centrality Analysis for Drake

twitter_data_drake <- readRDS("my8000tweets.rds")


# Create twomode (bimodal) network

twomode_network_drake <- twitter_data_drake %>% Create("twomode", 
                                                       removeTermsOrHashtags = c("Drake", "Honestly Nevermind", "#drake", 
                                                                                 "#drizzy", "#champagnepapi", "#6god", 
                                                                                 "#itsallablurtour", "#draketickets", "#teamdrizzy"))
twomode_graph_drake <- twomode_network_drake %>% Graph()


# Write graph to file

write.graph(twomode_graph_drake, file = "TwitterTwomodeDrake.graphml", format = "graphml")


# Inspect the graph object

length(V(twomode_graph_drake))
V(twomode_graph_drake)$name


# Find all maximum components that are weakly connected

twomode_comps_drake <- components(twomode_graph_drake, mode = c("weak"))

twomode_comps_drake$no
twomode_comps_drake$csize
head(twomode_comps_drake$membership, n = 30)


# Get sub-graph with most members

largest_comp_drake <- which.max(twomode_comps_drake$csize)

twomode_subgraph_drake <- twomode_graph_drake %>% 
  induced_subgraph(vids = which(twomode_comps_drake$membership == largest_comp_drake))


# Display top 20 nodes from the sub-graph ordered by degree centrality

degreeCentInDrake <- sort(degree(twomode_subgraph_drake, mode = "in"), decreasing = TRUE)[1:20]
print(degreeCentInDrake)
degreeCentOutDrake <- sort(degree(twomode_subgraph_drake, mode = "out"), decreasing = TRUE)[1:20]
print(degreeCentOutDrake)
degree(twomode_subgraph_drake, mode = "out")["@drake"]
degreeCentTotalDrake <- sort(degree(twomode_subgraph_drake, mode = "total"), decreasing = TRUE)[1:20]
print(degreeCentTotalDrake)


# Display top 20 nodes from the sub-graph ordered by closeness centrality

closeCentInDrake <- sort(closeness(twomode_subgraph_drake, mode = "in"), decreasing = TRUE)[1:20]
print(closeCentInDrake)
closeness(twomode_subgraph_drake, mode = "in")["@drake"]
closeCentOutDrake <- sort(closeness(twomode_subgraph_drake, mode = "out"), decreasing = TRUE)[1:20]
print(closeCentOutDrake)
closeness(twomode_subgraph_drake, mode = "out")["@drake"]
closeCentTotalDrake <- sort(closeness(twomode_subgraph_drake, mode = "total"), decreasing = TRUE)[1:20]
print(closeCentTotalDrake)


# Display top 20 nodes from the sub-graph ordered by betweenness centrality

betweenCentDrake <-sort(betweenness(twomode_subgraph_drake, directed = FALSE), decreasing = TRUE)[1:20]
print(betweenCentDrake)


# Centrality Analysis for related artist, The Weeknd

# Authenticate to Twitter and collect data for The Weeknd

twitter_data <- Authenticate("twitter",
                             appName = my_app_name,
                             apiKey = my_api_key,
                             apiSecret = my_api_secret,
                             accessToken = my_access_token,
                             accessTokenSecret = my_access_token_secret) %>%
  Collect(searchTerm = "\"The Weeknd\" OR #theweeknd",
          searchType = "recent",
          numTweets = 8000,
          lang = "en",
          includeRetweets = TRUE,
          writeToFile = TRUE,
          verbose = TRUE)


twitter_data_weeknd <- readRDS("twitter_data_weeknd.rds")


# Create twomode (bimodal) network

twomode_network_weeknd <- twitter_data_weeknd %>% Create("twomode", 
                                                         removeTermsOrHashtags = c("The Weeknd", "#theweeknd"))
twomode_graph_weeknd <- twomode_network_weeknd %>% Graph()


# Write graph to file

write.graph(twomode_graph_weeknd, file = "TwitterTwomodeWeeknd.graphml", format = "graphml")


# Inspect the graph object

length(V(twomode_graph_weeknd))
V(twomode_graph_weeknd)$name


# Find all maximum components that are weakly connected

twomode_comps_weeknd <- components(twomode_graph_weeknd, mode = c("weak"))

twomode_comps_weeknd$no
twomode_comps_weeknd$csize
head(twomode_comps_weeknd$membership, n = 30)


# Get sub-graph with most members

largest_comp_weeknd <- which.max(twomode_comps_weeknd$csize)

twomode_subgraph_weeknd <- twomode_graph_weeknd %>% 
  induced_subgraph(vids = which(twomode_comps_weeknd$membership == largest_comp_weeknd))


# Display top 10 nodes from the sub-graph ordered by degree centrality

degreeCentInWeeknd <- sort(degree(twomode_subgraph_weeknd, mode = "in"), decreasing = TRUE)[1:20]
print(degreeCentInWeeknd)
degreeCentOutWeeknd <- sort(degree(twomode_subgraph_weeknd, mode = "out"), decreasing = TRUE)[1:20]
print(degreeCentOutWeeknd)
degreeCentTotalWeeknd <- sort(degree(twomode_subgraph_weeknd, mode = "total"), decreasing = TRUE)[1:20]
print(degreeCentTotalWeeknd)


# Display top 10 nodes from the sub-graph ordered by closeness centrality

closeCentInWeeknd <- sort(closeness(twomode_subgraph_weeknd, mode = "in"), decreasing = TRUE)[1:20]
print(closeCentInWeeknd)
closeCentOutWeeknd <- sort(closeness(twomode_subgraph_weeknd, mode = "out"), decreasing = TRUE)[1:20]
print(closeCentOutWeeknd)
closeCentTotalWeeknd <- sort(closeness(twomode_subgraph_weeknd, mode = "total"), decreasing = TRUE)[1:20]
print(closeCentTotalWeeknd)


# Display top 10 nodes from the sub-graph ordered by betweenness centrality

betweenCentWeeknd <- sort(betweenness(twomode_subgraph_weeknd, directed = FALSE), decreasing = TRUE)[1:20]
print(betweenCentWeeknd)



# Centrality Analysis for related artist, J.Cole

# Authenticate to Twitter and collect data for J.Cole

twitter_data <- Authenticate("twitter",
                             appName = my_app_name,
                             apiKey = my_api_key,
                             apiSecret = my_api_secret,
                             accessToken = my_access_token,
                             accessTokenSecret = my_access_token_secret) %>%
  Collect(searchTerm = "\"J.Cole\" OR #jcole",
          searchType = "recent",
          numTweets = 8000,
          lang = "en",
          includeRetweets = TRUE,
          writeToFile = TRUE,
          verbose = TRUE)


twitter_data_jcole <- readRDS("twitter_data_jcole.rds")


# Create twomode (bimodal) network

twomode_network_jcole <- twitter_data_jcole %>% Create("twomode", 
                                                       removeTermsOrHashtags = c("J.Cole", "#jcole"))
twomode_graph_jcole <- twomode_network_jcole %>% Graph()


# Write graph to file

write.graph(twomode_graph_jcole, file = "TwitterTwomodeJcole.graphml", format = "graphml")


# Inspect the graph object

length(V(twomode_graph_jcole))
V(twomode_graph_jcole)$name


# Find all maximum components that are weakly connected

twomode_comps_jcole <- components(twomode_graph_jcole, mode = c("weak"))

twomode_comps_jcole$no
twomode_comps_jcole$csize
head(twomode_comps_jcole$membership, n = 30)


# Get sub-graph with most members

largest_comp_jcole <- which.max(twomode_comps_jcole$csize)

twomode_subgraph_jcole <- twomode_graph_jcole %>% 
  induced_subgraph(vids = which(twomode_comps_jcole$membership == largest_comp_jcole))


# Display top 10 nodes from the sub-graph ordered by degree centrality

degreeCentInJcole <- sort(degree(twomode_subgraph_jcole, mode = "in"), decreasing = TRUE)[1:20]
print(degreeCentInJcole)
degreeCentOutJcole <- sort(degree(twomode_subgraph_jcole, mode = "out"), decreasing = TRUE)[1:20]
print(degreeCentOutJcole)
degreeCentTotalJcole <- sort(degree(twomode_subgraph_jcole, mode = "total"), decreasing = TRUE)[1:20]
print(degreeCentTotalJcole)


# Display top 10 nodes from the sub-graph ordered by closeness centrality

closeCentInJcole <- sort(closeness(twomode_subgraph_jcole, mode = "in"), decreasing = TRUE)[1:20]
print(closeCentInJcole)
closeCentOutJcole <- sort(closeness(twomode_subgraph_jcole, mode = "out"), decreasing = TRUE)[1:20]
print(closeCentOutJcole)
closeCentTotalJcole <- sort(closeness(twomode_subgraph_jcole, mode = "total"), decreasing = TRUE)[1:20]
print(closeCentTotalJcole)


# Display top 10 nodes from the sub-graph ordered by betweenness centrality

betweenCentJcole <- sort(betweenness(twomode_subgraph_jcole, directed = FALSE), decreasing = TRUE)[1:20]
print(betweenCentJcole)



# Compare Centralities Between the Three Artists

Artist <- c("@drake", "@theweeknd", "@jcolenc")
Degree <- c(unname(degree(twomode_subgraph_drake, mode = "total")["@drake"]), unname(degree(twomode_subgraph_drake, mode = "total")["@theweeknd"]), unname(degree(twomode_subgraph_jcole, mode = "total")["@jcolenc"]))
Closeness <- c(unname(closeness(twomode_subgraph_drake, mode = "total")["@drake"]), unname(closeness(twomode_subgraph_drake, mode = "total")["@theweeknd"]), unname(closeness(twomode_subgraph_jcole, mode = "total")["@jcolenc"]))
Betweenness <- c(unname(betweenness(twomode_subgraph_drake, directed = FALSE)["@drake"]), unname(betweenness(twomode_subgraph_drake, directed = FALSE)["@theweeknd"]), unname(betweenness(twomode_subgraph_jcole, directed = FALSE)["@jcolenc"]))

centralitydf <- data.frame(Artist, Degree, Closeness, Betweenness)

barplot(height = centralitydf$Degree, names = centralitydf$Artist, main = "Degree Centrality Comparison Between Drake and His Related Artists", 
        xlab = "Artist", ylab = "Degree Centrality Score", col = c("#619CFF", "#00BA38", "#F8766D"))

barplot(height = centralitydf$Closeness, names = centralitydf$Artist, main = "Closeness Centrality Comparison Between Drake and His Related Artists", 
        xlab = "Artist", ylab = "Closeness Centrality Score", col = c("#619CFF", "#00BA38", "#F8766D"))

barplot(height = centralitydf$Betweenness, names = centralitydf$Artist, main = "Betweenness Centrality Comparison Between Drake and His Related Artists", 
        xlab = "Artist", ylab = "Betweenness Centrality Score", col = c("#619CFF", "#00BA38", "#F8766D"))


# ----------

# 2.5: Community Analysis ------------------------------------------------------

# Load packages required for this session into library

library(tuber)
library(vosonSML)
library(magrittr)
library(igraph)
library(httpuv)


# Centrality Analysis for Drake

yt_data_drake <- readRDS("youtube_comments_data.rds")


yt_actor_network_drake <- yt_data_drake %>% Create("actor")
yt_actor_graph_drake <- Graph(yt_actor_network_drake)


# Transform into an undirected graph

undir_yt_actor_graph_drake <- as.undirected(yt_actor_graph_drake, mode = "collapse")


# Run Louvain algorithm

louvain_yt_actor_drake <- cluster_louvain(undir_yt_actor_graph_drake)


# See sizes of communities

sizes(louvain_yt_actor_drake)


# Visualise the Louvain communities

plot(louvain_yt_actor_drake, 
     undir_yt_actor_graph_drake, 
     vertex.label = V(undir_yt_actor_graph_drake)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)


# Run Girvan-Newman (edge-betweenness) algorithm

eb_yt_actor_drake <- cluster_edge_betweenness(undir_yt_actor_graph_drake)


# See sizes of communities

sizes(eb_yt_actor_drake)


# Visualise the edge-betweenness communities

plot(eb_yt_actor_drake,
     undir_yt_actor_graph_drake, 
     vertex.label = V(undir_yt_actor_graph_drake)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)



# Centrality Analysis for related artist, The Weeknd

# Search YouTube

video_search_weeknd <- yt_search("The Weeknd")


# Choose 10 videos and store their video IDs,
# for which we want to collect comments
# and build an actor network

video_ids_list_weeknd <- c()

for (i in 1:10) {
  video_ids_weeknd <- video_search_weeknd$video_id[i]
  video_ids_list_weeknd <- append(video_ids_list_weeknd, video_ids_weeknd)
}

yt_data_weeknd <- Authenticate("youtube", apiKey = api_key) %>%
  Collect(videoIDs = video_ids_list_weeknd,
          writeToFile = TRUE,
          maxComments = 500,
          verbose = TRUE)

View(yt_data_weeknd)


yt_data_weeknd <- readRDS("youtube_data_weeknd.rds")


yt_actor_network_weeknd <- yt_data_weeknd %>% Create("actor")
yt_actor_graph_weeknd <- Graph(yt_actor_network_weeknd)


# Transform into an undirected graph

undir_yt_actor_graph_weeknd <- as.undirected(yt_actor_graph_weeknd, mode = "collapse")


# Run Louvain algorithm

louvain_yt_actor_weeknd <- cluster_louvain(undir_yt_actor_graph_weeknd)


# See sizes of communities

sizes(louvain_yt_actor_weeknd)


# Visualise the Louvain communities

plot(louvain_yt_actor_weeknd, 
     undir_yt_actor_graph_weeknd, 
     vertex.label = V(undir_yt_actor_graph_weeknd)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)


# Run Girvan-Newman (edge-betweenness) algorithm

eb_yt_actor_weeknd <- cluster_edge_betweenness(undir_yt_actor_graph_weeknd)


# See sizes of communities

sizes(eb_yt_actor_weeknd)


# Visualise the edge-betweenness communities

plot(eb_yt_actor_weeknd,
     undir_yt_actor_graph_weeknd, 
     vertex.label = V(undir_yt_actor_graph_weeknd)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)



# Centrality Analysis for related artist, J.Cole

# Search YouTube

video_search_jcole <- yt_search("J.Cole")


# Choose 10 videos and store their video IDs,
# for which we want to collect comments
# and build an actor network

video_ids_list_jcole <- c()

for (i in 1:10) {
  video_ids_jcole <- video_search_jcole$video_id[i]
  video_ids_list_jcole <- append(video_ids_list_jcole, video_ids_jcole)
}

yt_data_jcole <- Authenticate("youtube", apiKey = api_key) %>%
  Collect(videoIDs = video_ids_list_jcole,
          writeToFile = TRUE,
          maxComments = 500,
          verbose = TRUE)

View(yt_data_jcole)


yt_data_jcole <- readRDS("youtube_data_jcole.rds")


yt_actor_network_jcole <- yt_data_jcole %>% Create("actor")
yt_actor_graph_jcole <- Graph(yt_actor_network_jcole)


# Transform into an undirected graph

undir_yt_actor_graph_jcole <- as.undirected(yt_actor_graph_jcole, mode = "collapse")


# Run Louvain algorithm

louvain_yt_actor_jcole <- cluster_louvain(undir_yt_actor_graph_jcole)


# See sizes of communities

sizes(louvain_yt_actor_jcole)


# Visualise the Louvain communities

plot(louvain_yt_actor_jcole, 
     undir_yt_actor_graph_jcole, 
     vertex.label = V(undir_yt_actor_graph_jcole)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)


# Run Girvan-Newman (edge-betweenness) algorithm

eb_yt_actor_jcole <- cluster_edge_betweenness(undir_yt_actor_graph_jcole)


# See sizes of communities

sizes(eb_yt_actor_jcole)


# Visualise the edge-betweenness communities

plot(eb_yt_actor_jcole,
     undir_yt_actor_graph_jcole, 
     vertex.label = V(undir_yt_actor_graph_jcole)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)


# ----------

# 2.6: Sentiment Analysis ------------------------------------------------------

# Load packages required for this session into library

library(vosonSML)
library(magrittr)
library(tidytext)
library(textclean)
library(qdapRegex)
library(syuzhet)
library(ggplot2)


twitter_data <- readRDS("my8000tweets.rds")


# Clean the tweet text

clean_text <- twitter_data$Comment %>% 
  rm_twitter_url() %>% 
  replace_url() %>% 
  replace_hash() %>% 
  replace_tag() %>% 
  replace_internet_slang() %>% 
  replace_emoji() %>% 
  replace_emoticon() %>% 
  replace_non_ascii() %>% 
  replace_contraction() %>% 
  gsub("[[:punct:]]", " ", .) %>% 
  gsub("[[:digit:]]", " ", .) %>% 
  gsub("[[:cntrl:]]", " ", .) %>% 
  gsub("\\s+", " ", .) %>% 
  tolower()


# Assign sentiment scores to tweets

sentiment_scores <- get_sentiment(clean_text, method = "afinn") %>% sign()

sentiment_df <- data.frame(text = clean_text, sentiment = sentiment_scores)
View(sentiment_df)


# Convert sentiment scores to labels: positive, neutral, negative

sentiment_df$sentiment <- factor(sentiment_df$sentiment, levels = c(1, 0, -1),
                                 labels = c("Positive", "Neutral", "Negative")) 
View(sentiment_df)


# Plot sentiment classification

ggplot(sentiment_df, aes(x = sentiment)) +
  geom_bar(aes(fill = sentiment)) +
  scale_fill_brewer(palette = "RdGy") +
  labs(fill = "Sentiment") +
  labs(x = "Sentiment Categories", y = "Number of Tweets") +
  ggtitle("Sentiment Analysis of Tweets")


# Assign emotion scores to tweets

emo_scores <- get_nrc_sentiment(clean_text)[ , 1:8]

emo_scores_df <- data.frame(clean_text, emo_scores)
View(emo_scores_df)


# Calculate proportion of emotions across all tweets

emo_sums <- emo_scores_df[,2:9] %>% 
  sign() %>% 
  colSums() %>% 
  sort(decreasing = TRUE) %>% 
  data.frame() / nrow(emo_scores_df) 

names(emo_sums)[1] <- "Proportion" 
View(emo_sums)


# Plot emotion classification

ggplot(emo_sums, aes(x = reorder(rownames(emo_sums), Proportion),
                     y = Proportion,
                     fill = rownames(emo_sums))) +
  geom_col() +
  coord_flip()+
  guides(fill = "none") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Emotion Categories", y = "Proportion of Tweets") +
  ggtitle("Emotion Analysis of Tweets")



youtube_data <- readRDS("youtube_comments_data.rds")


# Clean the Youtube comment text

clean_text <- youtube_data$Comment %>% 
  rm_twitter_url() %>% 
  replace_url() %>% 
  replace_hash() %>% 
  replace_tag() %>% 
  replace_internet_slang() %>% 
  replace_emoji() %>% 
  replace_emoticon() %>% 
  replace_non_ascii() %>% 
  replace_contraction() %>% 
  gsub("[[:punct:]]", " ", .) %>% 
  gsub("[[:digit:]]", " ", .) %>% 
  gsub("[[:cntrl:]]", " ", .) %>% 
  gsub("\\s+", " ", .) %>% 
  tolower()


# Assign sentiment scores to tweets

sentiment_scores <- get_sentiment(clean_text, method = "afinn") %>% sign()

sentiment_df <- data.frame(text = clean_text, sentiment = sentiment_scores)
View(sentiment_df)


# Convert sentiment scores to labels: positive, neutral, negative

sentiment_df$sentiment <- factor(sentiment_df$sentiment, levels = c(1, 0, -1),
                                 labels = c("Positive", "Neutral", "Negative")) 
View(sentiment_df)


# Plot sentiment classification

ggplot(sentiment_df, aes(x = sentiment)) +
  geom_bar(aes(fill = sentiment)) +
  scale_fill_brewer(palette = "RdGy") +
  labs(fill = "Sentiment") +
  labs(x = "Sentiment Categories", y = "Number of Comments") +
  ggtitle("Sentiment Analysis of Youtube Comments")


# Assign emotion scores to tweets

emo_scores <- get_nrc_sentiment(clean_text)[ , 1:8]

emo_scores_df <- data.frame(clean_text, emo_scores)
View(emo_scores_df)


# Calculate proportion of emotions across all tweets

emo_sums <- emo_scores_df[,2:9] %>% 
  sign() %>% 
  colSums() %>% 
  sort(decreasing = TRUE) %>% 
  data.frame() / nrow(emo_scores_df) 

names(emo_sums)[1] <- "Proportion" 
View(emo_sums)


# Plot emotion classification

ggplot(emo_sums, aes(x = reorder(rownames(emo_sums), Proportion),
                     y = Proportion,
                     fill = rownames(emo_sums))) +
  geom_col() +
  coord_flip()+
  guides(fill = "none") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Emotion Categories", y = "Proportion of Comments") +
  ggtitle("Emotion Analysis of Youtube Comments")


# ----------

# 2.7: Decision Tree -----------------------------------------------------------

library(spotifyr)
library(C50)
library(caret)
library(e1071)
library(dplyr)


# Set up Spotify authentication variables

app_id <- "fb0cf199164b46638545119ee76c9409"
app_secret <- "14cc3f5cf88348c0a5069f8ef21a17f1"
token <- "1"


# Authenticate to Spotify using the spotifyr package:

Sys.setenv(SPOTIFY_CLIENT_ID = app_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = app_secret)
access_token <- get_spotify_access_token()


# Get songs from Drake and their audio features

drake_features <- get_artist_audio_features("Drake")
View(drake_features)

data.frame(colnames(drake_features))

drake_features_subset <- drake_features[ , 9:20]
View(drake_features_subset)


# Get top 100 songs and their audio features

top100_features <- get_playlist_audio_features("spotify", "4hOKQuZbraPDIfaGbM3lKI")
View(top100_features)

data.frame(colnames(top100_features))

top100_features_subset <- top100_features[ , 6:17]
View(top100_features_subset)

top100_features_subset <- top100_features_subset %>% rename(track_id = track.id)


# Add the 'isDrake' column (class variable) to each data frame
# to indicate which songs are by Drake and which are not

top100_features_subset["isDrake"] <- 0
drake_features_subset["isDrake"] <- 1


# Remove any songs by Drake that appear in the top 100
# and combine the two data frames into one dataset

top100_features_nodrake <- anti_join(top100_features_subset,
                                     drake_features_subset,
                                     by = "track_id")
comb_data <- rbind(top100_features_nodrake, drake_features_subset)


# Format the dataset so that we can give it as input to a model:
# change the 'isDrake' column into a factor
# and remove the 'track_id' column

comb_data$isDrake <- factor(comb_data$isDrake)
comb_data <- select(comb_data, -track_id)


# Randomise the dataset (shuffle the rows)

comb_data <- comb_data[sample(1:nrow(comb_data)), ]


# Split the dataset into training and testing sets (80% training, 20% testing)

split_point <- as.integer(nrow(comb_data)*0.8)
training_set <- comb_data[1:split_point, ]
testing_set <- comb_data[(split_point + 1):nrow(comb_data), ]


# Train the decision tree model

dt_model <- train(isDrake~ ., data = training_set, method = "C5.0")


# Sample a single prediction (can repeat)

prediction_row <- sample(nrow(testing_set), 1) # MUST be smaller than or equal to training set size

if (tibble(predict(dt_model, testing_set[prediction_row, ])) ==
    testing_set[prediction_row, 12]){
  print("Prediction is correct!")
} else {
  ("Prediction is wrong")
}


# Analyse the model accuracy with a confusion matrix

confusionMatrix(dt_model, reference = testing_set$isDrake)



# Attempt to improve Decision Tree Performance by using Larger Playlist with Less Bias Towards Drake

# Get top 1500 songs and their audio features

top1500_features <- get_playlist_audio_features("spotify", "3Z8FFgeOSwmQZI7BbWagsl")
View(top1500_features)

data.frame(colnames(top1500_features))

top1500_features_subset <- top1500_features[ , 6:17]
View(top1500_features_subset)

top1500_features_subset <- top1500_features_subset %>% rename(track_id = track.id)


# Add the 'isDrake' column (class variable) to each data frame
# to indicate which songs are by Drake and which are not

top1500_features_subset["isDrake"] <- 0


# Remove any songs by Drake that appear in the top 100
# and combine the two data frames into one dataset

top1500_features_nodrake <- anti_join(top1500_features_subset,
                                      drake_features_subset,
                                      by = "track_id")
comb_data_improved <- rbind(top1500_features_nodrake, drake_features_subset)


# Format the dataset so that we can give it as input to a model:
# change the 'isDrake' column into a factor
# and remove the 'track_id' column

comb_data_improved$isDrake <- factor(comb_data_improved$isDrake)
comb_data_improved <- select(comb_data_improved, -track_id)


# Randomise the dataset (shuffle the rows)

comb_data_improved <- comb_data_improved[sample(1:nrow(comb_data_improved)), ]


# Split the dataset into training and testing sets (80% training, 20% testing)

split_point_improved <- as.integer(nrow(comb_data_improved)*0.8)
training_set_improved <- comb_data_improved[1:split_point_improved, ]
testing_set_improved <- comb_data_improved[(split_point_improved + 1):nrow(comb_data_improved), ]


# Train the decision tree model

dt_model_improved <- train(isDrake~ ., data = training_set_improved, method = "C5.0")


# Sample a single prediction (can repeat)

prediction_row_improved <- sample(nrow(testing_set_improved), 1) # MUST be smaller than or equal to training set size

if (tibble(predict(dt_model_improved, testing_set_improved[prediction_row_improved, ])) ==
    testing_set_improved[prediction_row_improved, 12]){
  print("Prediction is correct!")
} else {
  ("Prediction is wrong")
}


# Analyse the model accuracy with a confusion matrix

confusionMatrix(dt_model_improved, reference = testing_set_improved$isDrake)


# ----------

# 2.8: Topic Modelling ---------------------------------------------------------

# Load packages required for this part

library(vosonSML)
library(magrittr)
library(tidytext)
library(textclean)
library(qdapRegex)
library(tm)
library(topicmodels)
library(slam)
library(Rmpfr)
library(dplyr)
library(ggplot2)
library(reshape2)


twitter_data <- readRDS("my8000tweets.rds")


# Clean the tweet text

clean_text <- twitter_data$tweets$text  %>% 
  rm_twitter_url() %>% 
  replace_url() %>% 
  replace_hash() %>% 
  replace_tag() %>% 
  replace_internet_slang() %>% 
  replace_emoji() %>% 
  replace_emoticon() %>% 
  replace_non_ascii() %>% 
  replace_contraction() %>% 
  gsub("[[:punct:]]", " ", .) %>% 
  gsub("[[:digit:]]", " ", .) %>% 
  gsub("[[:cntrl:]]", " ", .) %>% 
  gsub("\\s+", " ", .) %>% 
  tolower()


# Convert clean tweet vector into a document corpus (collection of documents)

text_corpus <- VCorpus(VectorSource(clean_text))

text_corpus[[1]]$content
text_corpus[[5]]$content


# Remove stop words

text_corpus <- text_corpus %>%
  tm_map(removeWords, stopwords(kind = "SMART")) 

text_corpus[[1]]$content
text_corpus[[5]]$content


# Transform corpus into a Document Term Matrix and remove 0 entries

doc_term_matrix <- DocumentTermMatrix(text_corpus)
non_zero_entries = unique(doc_term_matrix$i)
dtm = doc_term_matrix[non_zero_entries,]



# Create LDA model with k topics

lda_model <- LDA(dtm, k = 3)


# Generate topic probabilities for each word
# 'beta' shows the probability that this word was generated by that topic

tweet_topics <- tidy(lda_model, matrix = "beta")


# Visualise the top 10 terms per topic

top_terms <- tweet_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()



youtube_data <- readRDS("youtube_comments_data.rds")


# Clean the comment text

clean_text <- youtube_data$Comment  %>% 
  rm_twitter_url() %>% 
  replace_url() %>% 
  replace_hash() %>% 
  replace_tag() %>% 
  replace_internet_slang() %>% 
  replace_emoji() %>% 
  replace_emoticon() %>% 
  replace_non_ascii() %>% 
  replace_contraction() %>% 
  gsub("[[:punct:]]", " ", .) %>% 
  gsub("[[:digit:]]", " ", .) %>% 
  gsub("[[:cntrl:]]", " ", .) %>% 
  gsub("\\s+", " ", .) %>% 
  tolower()


# Convert clean comment vector into a document corpus (collection of documents)

text_corpus <- VCorpus(VectorSource(clean_text))

text_corpus[[1]]$content
text_corpus[[5]]$content


# Remove stop words

text_corpus <- text_corpus %>%
  tm_map(removeWords, stopwords(kind = "SMART")) 

text_corpus[[1]]$content
text_corpus[[5]]$content


# Transform corpus into a Document Term Matrix and remove 0 entries

doc_term_matrix <- DocumentTermMatrix(text_corpus)
non_zero_entries = unique(doc_term_matrix$i)
dtm = doc_term_matrix[non_zero_entries,]


# Create LDA model with k topics

lda_model <- LDA(dtm, k = 3)


# Generate topic probabilities for each word
# 'beta' shows the probability that this word was generated by that topic

comment_topics <- tidy(lda_model, matrix = "beta")


# Visualise the top 10 terms per topic

top_terms <- comment_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


# ----------

# 2.9: Gephi Vis ---------------------------------------------------------------

# Load packages required for this session into library

library(vosonSML)
library(magrittr)
library(igraph)
library(tidyr)
library(tidytext)

# Set up Twitter authentication variables

my_app_name <- "LiamBarryBigData"
my_api_key <- "vcyiO3xmm2R0SlQeUeYjWM5Zz"
my_api_secret <- "oUc42X1BgWbgjgOKmhtrsMiqVIwEhcIF3m5AsExDNxOemGq7Va"
my_access_token <- "1632609860960542722-AcRxp75HzcUlMMzYTqlIThlCioNSk8"
my_access_token_secret <- "EXjDeTOk0cvC361UA2wgbXPWdJztlJmdIx8GL9khx8znw"

twitterAuth <- Authenticate("twitter",
                            appName = my_app_name,
                            apiKey = my_api_key,
                            apiSecret = my_api_secret,
                            accessToken = my_access_token,
                            accessTokenSecret = my_access_token_secret)

twitter_data <- readRDS("my8000tweets.rds")

# Create actor network and graph with user metadata from the data

twitter_actor_network <- twitter_data %>% Create("actor")
twitter_actor_graph <- twitter_actor_network %>% AddUserData(twitter_data, 
                                                             lookupUsers = TRUE, 
                                                             twitterAuth = twitterAuth) %>% 
                                                             Graph()


# Write graph to file

write.graph(twitter_actor_graph, file = "TwitterActorGraphWithUserAttr.graphml", format = "graphml")


# ----------

# 2.10: Dashboard --------------------------------------------------------------

library(writexl)

write_xlsx(x = fullTwitterData, path = "fullTwitter_data_tableau.xlsx", col_names = TRUE)