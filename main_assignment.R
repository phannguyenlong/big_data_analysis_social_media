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

# ============================================================================
# DATA COLLECTION FROM YOUTUBE AND REDDIT
# ============================================================================

#=======================YOUTUBE DATA COLLECTION=======================
source("./data_collection/youtube_data_collection.R")

#=======================DYNAMIC REDDIT DATA COLLECTION=======================
source("./data_collection/reddit_data_collection.R")

# ============================================================================
# DATA COLLECTION SUMMARY
# ============================================================================

print("\n========== DATA COLLECTION SUMMARY ==========")
print(paste("Artist:", ARTIST_NAME))
print(paste("YouTube Comments:", nrow(yt_data)))
print(paste("Reddit Comments:", nrow(rd_data)))
print(paste("Total Data Points:", nrow(yt_data) + nrow(rd_data)))
print(paste("Collection Date:", Sys.Date()))

# Create summary visualization
summary_data <- data.frame(
  Platform = c("YouTube", "Reddit"),
  Comments = c(nrow(yt_data), nrow(rd_data))
)

p_summary <- ggplot(summary_data, aes(x = Platform, y = Comments, fill = Platform)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Comments), vjust = -0.5) +
  labs(title = paste("Data Collection Summary for", ARTIST_NAME),
       subtitle = paste("Total:", sum(summary_data$Comments), "comments"),
       y = "Number of Comments") +
  theme_minimal() +
  scale_fill_manual(values = c("YouTube" = "#FF0000", "Reddit" = "#FF4500"))

ggsave("data_collection_summary.png", p_summary, width = 8, height = 6)
print("Summary plot saved as 'data_collection_summary.png'")

# Check if we have enough data (3000-8000 range)
total_comments <- nrow(yt_data) + nrow(rd_data)
if(total_comments < 3000) {
  warning(paste("Only collected", total_comments, "comments. Consider adding more threads or videos."))
} else if(total_comments > 8000) {
  print(paste("Collected", total_comments, "comments. Consider sampling down to 8000 if needed."))
} else {
  print(paste("✓ Successfully collected", total_comments, "comments (within 3000-8000 range)"))
}


# ============================================================================
# COMPREHENSIVE DATA SUMMARY AND VALIDATION
# ============================================================================

print("\n========== DATA COLLECTION SUMMARY ==========")

# Calculate totals
total_data_points <- nrow(yt_data) + nrow(rd_data)

# Create detailed summary
collection_summary <- data.frame(
  Source = c("YouTube", "Reddit", "Total"),
  Raw_Count = c(original_yt_count, nrow(rd_data) + 89, original_yt_count + nrow(rd_data) + 89),
  Clean_Count = c(nrow(yt_data), nrow(rd_data), total_data_points),
  Percentage = c(
    round(nrow(yt_data)/total_data_points * 100, 2),
    round(nrow(rd_data)/total_data_points * 100, 2),
    100
  ),
  Avg_Length = c(
    round(mean(nchar(yt_data$Comment_Clean), na.rm = TRUE)),
    round(mean(nchar(rd_data$comment), na.rm = TRUE)),
    NA
  )
)

print(collection_summary)