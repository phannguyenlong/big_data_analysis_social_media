# ======================================================
# # YOUTUBE DATA COLLECTION
# ======================================================

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

# Combine YouTube data
yt_data <- bind_rows(yt_data_list)
print(paste("✓ Total YouTube comments collected:", nrow(yt_data)))

# YOUTUBE DATA CLEANING

# Store original count
original_yt_count <- nrow(yt_data)

# 1. Remove duplicates
yt_data <- yt_data[!duplicated(yt_data$Comment), ]
duplicates_removed <- original_yt_count - nrow(yt_data)

# 2. Clean comment text (ensure 1:1 output with inputs)
clean_comment <- function(x) {
  if (length(x) == 0 || is.na(x)) return(NA_character_)
  x <- as.character(x)
  y <- x |>
    textclean::replace_url() |>
    textclean::replace_html() |>
    textclean::replace_non_ascii() |>
    textclean::replace_word_elongation() |>
    textclean::replace_internet_slang() |>
    textclean::replace_contraction() |>
    textclean::replace_emoji(replacement = "") |>
    textclean::replace_emoticon(replacement = "")
  if (length(y) != 1) y <- paste(y, collapse = " ")
  y
}

yt_data$Comment_Clean <- vapply(
  yt_data$Comment,
  clean_comment,
  FUN.VALUE = character(1),
  USE.NAMES = FALSE
)

# 3. Remove empty or very short comments (less than 3 characters)
yt_data <- yt_data[nchar(trimws(yt_data$Comment_Clean)) > 2, ]

# 4. Remove spam patterns (repeated characters, all caps spam)
spam_pattern <- grepl("^[A-Z\\s!]+$", yt_data$Comment_Clean) & 
  nchar(yt_data$Comment_Clean) > 20
yt_data <- yt_data[!spam_pattern, ]

print(paste("✓ Duplicates removed:", duplicates_removed))
print(paste("✓ Final YouTube comments after cleaning:", nrow(yt_data)))
print(paste("✓ Data quality improved by removing", 
            original_yt_count - nrow(yt_data), "low-quality entries"))

# Save YouTube data
saveRDS(yt_data, file = "taylor_swift_youtube_data.rds")
write.csv(yt_data, file = "taylor_swift_youtube_data.csv", row.names = FALSE)
