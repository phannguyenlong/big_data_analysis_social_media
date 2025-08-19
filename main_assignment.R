# ============================================================================
# 7230ICT Big Data Analytics and Social Media - Assignment Code
# Artist/Band Social Media Analytics
# ============================================================================


# ============================================================================
# SETUP AND LIBRARIES
# ============================================================================
# set working directory
setwd("D://Long_Document//Griffith//Tri2_2025//Big_Data_and_Social_Media//assignment//big_data_analysis_social_media")

# Choose Artist 
ARTIST_NAME <- "Taylor Swift"  # Change this to your chosen artist
SPOTIFY_ARTIST_ID <- "06HL4z0CvFAxyc27GXpf02"  # Find this from Spotify

# Config API Key
YOUTUBE_API_KEY <- "AIzaSyDtcul75BKnxX5zZEEOeDAztBUIShRIYas"  # Get from Google Cloud Console
SPOTIFY_CLIENT_ID <- "499fba4675224785923328d609a7be23"  # Get from Spotify Developer
SPOTIFY_CLIENT_SECRET <- "f577e1ec9c5646b09163becce92c5df6"
# dont need authentication for Reddit

# Load required libraries
library(vosonSML)
library(dplyr)
library(tidyr)
library(tidytext)
library(textclean)
library(tm)
library(ggplot2)
library(igraph)
library(spotifyr)
library(RedditExtractoR)
library(httr)

# load utils
source("./utils.R")

# ============================================================================
# QUESTION 2: DATA COLLECTION FROM YOUTUBE AND REDDIT
# ============================================================================

source("./q2_data_collection.R")

# ============================================================================
# QUESTION 3: Create actor network
# ============================================================================
# Youtube Actor Graph ------------------------------------------------------
# Load YouTube data
yt_data <- readRDS("yt_data.rds")
# Create actor network 
yt_actor_network <- yt_data |> 
  Create("actor") |> 
  AddText(yt_data,
          repliesFromText = TRUE,
          verbose = TRUE) |> 
  AddVideoData(yt_auth,
               actorSubOnly = TRUE,
               verbose = TRUE)

# Create graph from the network and change IDs to screen names
yt_actor_graph <- yt_actor_network |> Graph()
V(yt_actor_graph)$name <- V(yt_actor_graph)$screen_name
plot(yt_actor_graph)

# Save and write graph to file
saveRDS(yt_actor_graph, file = "YouTubeActor.rds")
write_graph(yt_actor_graph, file = "YouTubeActor.graphml", format = "graphml")

# Reddit Actor Graph ------------------------------------------------------
# Load Reddit data
rd_data <- readRDS("rd_data.rds")
# Create actor network 
rd_actor_network <- rd_data |> 
  Create("actor") |> 
  AddText(rd_data,
          verbose = TRUE) 

# Create graph from the network
rd_actor_graph <- rd_actor_network |> Graph()
rd_actor_graph
V(rd_actor_graph)$name <- V(rd_actor_graph)$user
rd_actor_graph
plot(rd_actor_graph)

# Save and write graph to file
saveRDS(rd_actor_graph, file = "RedditActor.rds")
write_graph(rd_actor_graph, file = "RedditActor.graphml", format = "graphml")

