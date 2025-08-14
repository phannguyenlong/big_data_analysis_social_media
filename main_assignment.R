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

# ============================================================================
# VISUALIZATION FOR REPORT
# ============================================================================

# 1. Platform Distribution Chart
p1 <- ggplot(collection_summary[1:2,], aes(x = Source, y = Clean_Count, fill = Source)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(Clean_Count, "\n(", Percentage, "%)", sep = "")), 
            vjust = -0.5, size = 4) +
  labs(title = "Taylor Swift Social Media Data Collection Distribution",
       subtitle = paste("Total:", total_data_points, "cleaned data points"),
       x = "Platform", y = "Number of Comments") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("YouTube" = "#FF0000", "Reddit" = "#FF4500"))

ggsave("q2_data_distribution.png", p1, width = 10, height = 6, dpi = 300)

# 2. YouTube Video Performance
p2 <- ggplot(yt_metadata, aes(x = reorder(substr(reason, 1, 20), comments_collected), 
                              y = comments_collected)) +
  geom_bar(stat = "identity", fill = "#FF0000") +
  coord_flip() +
  labs(title = "YouTube Data Collection by Video",
       x = "Video", y = "Comments Collected") +
  theme_minimal()

ggsave("q2_youtube_breakdown.png", p2, width = 10, height = 6, dpi = 300)

# 3. Data Quality Comparison
quality_data <- data.frame(
  Platform = rep(c("YouTube", "Reddit"), each = 2),
  Type = rep(c("Raw", "Clean"), 2),
  Count = c(original_yt_count, nrow(yt_data), 
            nrow(rd_data) + 89, nrow(rd_data))
)

p3 <- ggplot(quality_data, aes(x = Platform, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Data Quality: Raw vs Cleaned Comments",
       y = "Number of Comments") +
  theme_minimal() +
  scale_fill_manual(values = c("Raw" = "#cccccc", "Clean" = "#4CAF50"))

ggsave("q2_data_quality.png", p3, width = 10, height = 6, dpi = 300)