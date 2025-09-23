# ============================================================================
# 7230ICT Big Data Analytics and Social Media - Assignment Code
# Artist/Band Social Media Analytics
# ============================================================================


# ============================================================================
# SETUP AND LIBRARIES
# ============================================================================
# set working directory
setwd("D://Long_Document//Griffith//Tri2_2025//Big_Data_and_Social_Media//assignment//big_data_analysis_social_media")

# other config
graph_dir <- ".//graphs//"
dataset_dir <- ".//data//"

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

#########################################################################
# Data Selection & Exploration
#########################################################################

# ============================================================================
# QUESTION 2: DATA COLLECTION FROM YOUTUBE AND REDDIT
# ============================================================================

source("./q2_data_collection.R")
# do we need to remove N/A for this question

# ============================================================================
# QUESTION 3: Create actor network
# ============================================================================

source("./q3_actor_network.R")
# do we need to create Semantic graph for this question

# ============================================================================
# QUESTION 4: Unique actor
# ============================================================================
source("./q4_unique_actor.R")

# ============================================================================
# QUESTION 5: Sportify
# ============================================================================
source("./q5_sportify.R")

#########################################################################
# Text Pre-Processing
#########################################################################

# ============================================================================
# QUESTION 6: Term Document Matrix
# ============================================================================
source("./q6_term_document_matrix.R")

# ============================================================================
# QUESTION 7: semantic (bigram) networks 
# ============================================================================
source("./q7_sementic_bigram.R")

#########################################################################
# Social Network Analysis
#########################################################################

# ============================================================================
# QUESTION 8: degree centrality, betweenness centrality, and closeness centrality
# ============================================================================
source("./q8_centrality.R")

# ============================================================================
# QUESTION 9: community analysis with the Girvan-Newman (edge betweenness) and Louvain methods
# ============================================================================
source("./q9_community_analysis.R")



