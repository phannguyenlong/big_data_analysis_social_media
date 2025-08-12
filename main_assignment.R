# ============================================================================
# 7230ICT Big Data Analytics and Social Media - Assignment Code
# Artist/Band Social Media Analytics
# ============================================================================


# ============================================================================
# SETUP AND LIBRARIES
# ============================================================================
# set working directory
setwd("D://Long_Document//Griffith//Tri2_2025//Big_Data_and_Social_Media//assignment//assignment_code")

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
# # YOUTUBE DATA COLLECTION

yt_auth <- Authenticate("youtube", apiKey = YOUTUBE_API_KEY)

# Strategic video selection for Taylor Swift
# Mix of: Recent hits, classic songs, interviews, and fan content
youtube_videos <- c(
  # Recent Era (2023-2024) - High engagement expected
  "https://www.youtube.com/watch?v=b7QlX3yR2xs",  # Anti-Hero (Midnights)
  "https://www.youtube.com/watch?v=VuNIsY6JdUw",  # Shake It Off
  "https://www.youtube.com/watch?v=e-ORhEE9VVg",  # Blank Space
  "https://www.youtube.com/watch?v=tollGa3S0o8",  # Look What You Made Me Do
  "https://www.youtube.com/watch?v=FuXNumBwDOM",  # ME! ft. Brendon Urie
  "https://www.youtube.com/watch?v=RsEZmictANA",  # willow
  "https://www.youtube.com/watch?v=IC8qPpnD0uE",  # Lavender Haze
  "https://www.youtube.com/watch?v=Jb2stN7kH28",  # Cruel Summer
  "https://www.youtube.com/watch?v=wMpqCRF7TKg"   # Champagne Problems
)

# Collect YouTube comments with error handling
yt_data_list <- list()
total_yt_comments <- 0

for(i in seq_along(youtube_videos)) {
  tryCatch({
    print(paste("Collecting from video", i, "of", length(youtube_videos)))
    
    # Extract video ID from URL
    video_id <- gsub(".*v=([^&]+).*", "\\1", youtube_videos[i])
    
    # Collect with reasonable limits to avoid rate limiting
    temp_data <- yt_auth |> 
      Collect(videoIDs = video_id,
              maxComments = 500,  # Limit per video to avoid rate limits
              writeToFile = FALSE,
              verbose = TRUE)
    
    if(nrow(temp_data) > 0) {
      yt_data_list[[i]] <- temp_data
      total_yt_comments <- total_yt_comments + nrow(temp_data)
      print(paste("Collected", nrow(temp_data), "comments. Total so far:", total_yt_comments))
    }
    
    # Add delay to respect rate limits
    Sys.sleep(3)
    
  }, error = function(e) {
    print(paste("Error collecting video", i, ":", e$message))
  })
}

# Combine all YouTube data
yt_data <- bind_rows(yt_data_list)
print(paste("Total YouTube comments collected:", nrow(yt_data)))

# Save YouTube data
saveRDS(yt_data, file = "taylor_swift_youtube_data.rds")
write.csv(yt_data, file = "taylor_swift_youtube_data.csv", row.names = FALSE)

print("========== DYNAMIC REDDIT DATA COLLECTION ==========")

# Function to get popular threads from a subreddit
get_popular_threads <- function(subreddit_name, sort_by = "hot", time_filter = "week", limit = 10) {
  print(paste("Fetching popular threads from r/", subreddit_name, sep = ""))
  
  # Method 1: Using RedditExtractoR package
  tryCatch({
    # Find threads in the subreddit
    threads <- RedditExtractoR::find_thread_urls(
      subreddit = subreddit_name,
      sort_by = sort_by,        # Options: "hot", "new", "top", "rising"
      period = time_filter       # Options: "hour", "day", "week", "month", "year", "all"
    )
    
    if(nrow(threads) > 0) {
      # Sort by comments to get most discussed threads (base R to avoid NSE issues)
      threads <- threads[order(-threads$comments), , drop = FALSE]
      threads <- utils::head(threads, limit)
      
      print(paste("Found", nrow(threads), "popular threads"))
      return(threads$url)
    }
  }, error = function(e) {
    print(paste("RedditExtractoR error:", e$message))
  })
  
  # Method 2: Using Reddit JSON API (backup method)
  tryCatch({
    # Construct Reddit API URL
    reddit_url <- paste0(
      "https://www.reddit.com/r/", subreddit_name, 
      "/", sort_by, ".json?limit=", limit, 
      "&t=", time_filter
    )
    
    # Make request with proper user agent
    response <- httr::GET(
      reddit_url,
      httr::add_headers(`User-Agent` = "R:BigDataAnalytics:v1.0 (by /u/yourusername)")
    )
    
    if(httr::status_code(response) == 200) {
      # Parse JSON response
      content <- httr::content(response, "parsed")
      
      # Extract thread URLs
      thread_urls <- sapply(content$data$children, function(post) {
        paste0("https://www.reddit.com", post$data$permalink)
      })
      
      # Extract thread info for selection
      thread_info <- data.frame(
        url = thread_urls,
        title = sapply(content$data$children, function(x) x$data$title),
        score = sapply(content$data$children, function(x) x$data$score),
        num_comments = sapply(content$data$children, function(x) x$data$num_comments),
        stringsAsFactors = FALSE
      )
      
      # Sort by number of comments (most engaging threads)
      thread_info <- thread_info[order(-thread_info$num_comments), , drop = FALSE]
      thread_info <- utils::head(thread_info, limit)
      
      print("Thread selection based on engagement:")
      print(thread_info[, c("title", "num_comments", "score")])
      
      return(thread_info$url)
    }
  }, error = function(e) {
    print(paste("Reddit API error:", e$message))
  })
  
  # Return empty if both methods fail
  return(character(0))
}

# Function to get threads from multiple subreddits
get_multi_subreddit_threads <- function(artist_name) {
  all_threads <- character(0)
  
  # Primary subreddit (artist-specific)
  artist_subreddit <- gsub(" ", "", artist_name)  # Remove spaces
  
  # List of subreddits to search
  subreddits <- list(
    list(name = artist_subreddit, sort = "hot", time = "month", limit = 5),
    list(name = artist_subreddit, sort = "top", time = "year", limit = 3),
    list(name = "popheads", sort = "top", time = "month", limit = 2),
    list(name = "Music", sort = "top", time = "week", limit = 2)
  )
  
  for(sub in subreddits) {
    threads <- get_popular_threads(sub$name, sub$sort, sub$time, sub$limit)
    if(length(threads) > 0) {
      all_threads <- c(all_threads, threads)
    }
    Sys.sleep(2)  # Be respectful to Reddit's servers
  }
  
  # Remove duplicates
  all_threads <- unique(all_threads)
  
  return(all_threads)
}

# Authenticate Reddit (no credentials needed for vosonSML)
rd_auth <- Authenticate("reddit")

# Get popular threads dynamically
print("Fetching popular threads from Reddit...")

# Method 1: Get threads from Taylor Swift subreddit
popular_threads <- get_popular_threads("TaylorSwift", "hot", "month", 10)

# If not enough threads, expand search
if(length(popular_threads) < 10) {
  print("Expanding search to other subreddits...")
  additional_threads <- get_multi_subreddit_threads(ARTIST_NAME)
  popular_threads <- unique(c(popular_threads, additional_threads))
}

# Limit to 10 threads to manage collection time
reddit_threads <- head(popular_threads, 10)

print(paste("Selected", length(reddit_threads), "threads for data collection"))

# Collect Reddit data with error handling
rd_data_list <- list()
total_rd_comments <- 0

for(i in 1:length(reddit_threads)) {
  tryCatch({
    print(paste("\nCollecting Reddit thread", i, "of", length(reddit_threads)))
    print(paste("URL:", reddit_threads[i]))
    
    temp_data <- rd_auth |>
      Collect(threadUrls = reddit_threads[i],
              sort = "best",  # Get highest quality comments
              waitTime = c(6, 8),  # Random wait to avoid detection
              writeToFile = FALSE,
              verbose = TRUE)
    
    # Check if temp_data is NULL or empty
    if(!is.null(temp_data) && nrow(temp_data) > 0) {
      # Add thread URL to track source
      temp_data$thread_url <- reddit_threads[i]
      
      rd_data_list[[i]] <- temp_data
      total_rd_comments <- total_rd_comments + nrow(temp_data)
      print(paste("Collected", nrow(temp_data), "comments. Total so far:", total_rd_comments))
    } else {
      print(paste("No comments collected from thread", i))
    }
    
    # Respect rate limits
    Sys.sleep(5)
    
  }, error = function(e) {
    print(paste("Error collecting thread", i, ":", e$message))
  })
}

# Combine all Reddit data
rd_data <- bind_rows(rd_data_list)
print(paste("\nTotal Reddit comments collected:", nrow(rd_data)))

# Remove NA values
rd_data <- rd_data[complete.cases(rd_data$comment), ]
print(paste("Reddit comments after removing NAs:", nrow(rd_data)))

# Save Reddit data with metadata
saveRDS(rd_data, file = "taylor_swift_reddit_data.rds")
write.csv(rd_data, file = "taylor_swift_reddit_data.csv", row.names = FALSE)

# Save thread list for documentation
thread_metadata <- data.frame(
  thread_number = 1:length(reddit_threads),
  thread_url = reddit_threads,
  collection_date = Sys.Date()
)
write.csv(thread_metadata, file = "reddit_threads_collected.csv", row.names = FALSE)

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
