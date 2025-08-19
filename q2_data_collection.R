# ============================================================================
# QUESTION 2: DATA COLLECTION FROM YOUTUBE AND REDDIT
# Collect data about your artist/band from YouTube and/or Reddit. Make sure to choose
# keywords/videos/subreddits/threads for data retrieval that are most relevant to your
# artist/band. However, try not to be too narrow. You should retrieve 3000-8000 data points.
# List the keywords/videos/subreddits/threads and explain your search strategy, choice of
# data sources, and how much data you have collected.
# ============================================================================

#=======================YOUTUBE DATA COLLECTION=======================
yt_auth <- Authenticate("youtube", apiKey = YOUTUBE_API_KEY)
# Strategic video selection for Taylor Swift
# Mix of: Recent hits, classic songs, interviews, and fan content

# Strategic video selection - EXPLAIN WHY EACH VIDEO
youtube_videos <- list(
  list(url = "https://www.youtube.com/watch?v=b7QlX3yR2xs", 
       reason = "Anti-Hero - Lead single from Midnights, highest engagement"),
  list(url = "https://www.youtube.com/watch?v=VuNIsY6JdUw", 
       reason = "Shake It Off - Most viewed video, broad audience appeal"),
  list(url = "https://www.youtube.com/watch?v=e-ORhEE9VVg", 
       reason = "Blank Space - Second most popular, peak mainstream success"),
  list(url = "https://www.youtube.com/watch?v=tollGa3S0o8", 
       reason = "Look What You Made Me Do - Reputation era, controversy period"),
  list(url = "https://www.youtube.com/watch?v=FuXNumBwDOM", 
       reason = "ME! - Collaboration content, different fan base"),
  list(url = "https://www.youtube.com/watch?v=RsEZmictANA", 
       reason = "willow - Folklore/Evermore era, indie pivot"),
  list(url = "https://www.youtube.com/watch?v=IC8qPpnD0uE", 
       reason = "Lavender Haze - Recent release, current sentiment"),
  list(url = "https://www.youtube.com/watch?v=Jb2stN7kH28", 
       reason = "Cruel Summer - Fan favorite, organic hit"),
  list(url = "https://www.youtube.com/watch?v=wMpqCRF7TKg", 
       reason = "Champagne Problems - Storytelling peak, critical acclaim")
)

# Extract URLs for collection
video_urls <- sapply(youtube_videos, function(x) x$url)

# Collect YouTube comments with metadata
yt_data_list <- list()
yt_metadata <- data.frame()

for(i in seq_along(video_urls)) {
  tryCatch({
    print(paste("\nCollecting video", i, "of", length(video_urls)))
    print(paste("Reason:", youtube_videos[[i]]$reason))
    
    video_id <- gsub(".*v=([^&]+).*", "\\1", video_urls[i])
    
    temp_data <- yt_auth |> 
      Collect(videoIDs = video_id,
              maxComments = 500,
              writeToFile = FALSE,
              verbose = TRUE)
    
    if(nrow(temp_data) > 0) {
      # Add source metadata
      temp_data$video_source <- youtube_videos[[i]]$reason
      temp_data$video_url <- video_urls[i]
      
      yt_data_list[[i]] <- temp_data
      
      # Track metadata
      yt_metadata <- rbind(yt_metadata, data.frame(
        video_number = i,
        video_id = video_id,
        reason = youtube_videos[[i]]$reason,
        comments_collected = nrow(temp_data),
        avg_likes = mean(temp_data$LikeCount, na.rm = TRUE),
        avg_reply_count = mean(temp_data$ReplyCount, na.rm = TRUE)
      ))
      
      print(paste("✓ Collected", nrow(temp_data), "comments"))
    }
    
    Sys.sleep(3)  # Rate limiting
    
  }, error = function(e) {
    print(paste("✗ Error:", e$message))
  })
}

# Combine YouTube data (from 10 arrays - 10 video into 1 array)
yt_data <- bind_rows(yt_data_list)
print(paste("✓ Total YouTube comments collected:", nrow(yt_data)))

# remove N/A Row
# yt_data <- yt_data[complete.cases(yt_data), ] # does we need to remove N/A here

print(paste("✓ Total YouTube comments collected after N/A removed:", nrow(yt_data)))

# save it
saveRDS(yt_data, file = ".//yt_data.rds")
write.csv(yt_data, file = ".//yt_data.csv")

#=======================REDDIT DATA COLLECTION=======================
# Authenticate Reddit (no credentials needed for vosonSML)
rd_auth <- Authenticate("reddit")

# Get popular threads dynamically
print("Fetching popular threads from Reddit...")

# Method 1: Get threads from Taylor Swift subreddit
popular_threads <- get_popular_threads("TaylorSwift", "hot", "month", 10)

# Limit to 10 threads to manage collection time
reddit_threads <- head(popular_threads, 10)

print(paste("Selected", length(reddit_threads), "threads for data collection"))

# Collect Reddit data with error handling
rd_data_list <- list()
total_rd_comments <- 0

for(i in seq_along(reddit_threads)) {
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

# Combine all Reddit data (from 10 arrays - 10 video into 1 array)
rd_data <- bind_rows(rd_data_list)
print(paste("\nTotal Reddit comments collected:", nrow(rd_data)))

# remove N/A Row
rd_data <- rd_data[complete.cases(rd_data), ]

print(paste("\nTotal Reddit comments collected after N/A removed:", nrow(rd_data)))

# save it
saveRDS(rd_data, file = ".//rd_data.rds")
write.csv(rd_data, file = ".//rd_data.csv")

# ============================================================================
# COMPREHENSIVE DATA SUMMARY AND VALIDATION
# ============================================================================

print("\n========== DATA COLLECTION SUMMARY ==========")

# Calculate totals
total_data_points <- nrow(yt_data) + nrow(rd_data)

print(paste("✓ Total YouTube comments collected:", nrow(yt_data)))
print(paste("✓ Total Reddit comments collected:", nrow(rd_data)))
print(paste("✓ Total Data points: ", total_data_points))
