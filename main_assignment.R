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
# do we need to remove N/A for this question

# ============================================================================
# QUESTION 3: Create actor network
# ============================================================================

source("./q3_actor_network.R")
# do we need to create semetic graph for this question