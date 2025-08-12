# ======================================================
# # YOUTUBE DATA COLLECTION
# ======================================================

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
