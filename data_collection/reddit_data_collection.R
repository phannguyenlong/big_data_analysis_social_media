
# ======================================================
# # REDDIT DATA COLLECTION
# ======================================================

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

# Combine all Reddit data
rd_data <- bind_rows(rd_data_list)
print(paste("\nTotal Reddit comments collected:", nrow(rd_data)))

# ============================================================================
# ENHANCED REDDIT DATA CLEANING (Matching YouTube Standards)
# ============================================================================

print("\n========== REDDIT DATA CLEANING ==========")

# Store original count
original_rd_count <- nrow(rd_data)

# 1. Remove NA values (already done but let's ensure)
rd_data <- rd_data[complete.cases(rd_data$comment), ]
na_removed <- original_rd_count - nrow(rd_data)

# 2. Remove duplicate comments
before_dedupe <- nrow(rd_data)
rd_data <- rd_data[!duplicated(rd_data$comment), ]
duplicates_removed <- before_dedupe - nrow(rd_data)

# 3. Comprehensive text cleaning (same as YouTube)
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

rd_data$comment_clean <- vapply(
  rd_data$comment,
  function(x) {
    x_vec <- unlist(x, use.names = FALSE)
    x_chr <- paste(as.character(x_vec), collapse = " ")
    trimws(clean_comment(x_chr))
  },
  FUN.VALUE = character(1),
  USE.NAMES = FALSE
)

# 4. Remove deleted/removed comments
rd_data <- rd_data[!grepl("^\\[deleted\\]$|^\\[removed\\]$", rd_data$comment_clean, ignore.case = TRUE), ]

# 5. Remove bot comments (common bot patterns)
bot_patterns <- c(
  "I am a bot",
  "^RemindMe!",
  "^!RemindMe",
  "This is an automated",
  "^Your submission has been removed"
)
bot_pattern <- paste(bot_patterns, collapse = "|")
rd_data <- rd_data[!grepl(bot_pattern, rd_data$comment_clean, ignore.case = TRUE), ]

# 6. Remove very short comments (less than 3 words or 10 characters)
rd_data <- rd_data[nchar(rd_data$comment_clean) >= 10, ]
rd_data <- rd_data[lengths(strsplit(rd_data$comment_clean, "\\s+")) >= 3, ]

# 7. Remove excessive quote replies (comments that are mostly quotes)
# If more than 70% of comment is quoted text (starting with >)
quote_ratio <- sapply(rd_data$comment_clean, function(x) {
  lines <- strsplit(x, "\n")[[1]]
  quoted_lines <- sum(grepl("^>", trimws(lines)))
  total_lines <- length(lines)
  if(total_lines > 0) quoted_lines/total_lines else 0
})
rd_data <- rd_data[quote_ratio < 0.7, ]

# 8. Reddit-specific: Remove AutoModerator comments
rd_data <- rd_data[rd_data$author != "AutoModerator", ]

# Calculate cleaning statistics
final_rd_count <- nrow(rd_data)
total_removed <- original_rd_count - final_rd_count

print(paste("Reddit Data Cleaning Summary:"))
print(paste("  Original comments:", original_rd_count))
print(paste("  NA values removed:", na_removed))
print(paste("  Duplicates removed:", duplicates_removed))
print(paste("  Deleted/Bot/Short removed:", total_removed - na_removed - duplicates_removed))
print(paste("  Final clean comments:", final_rd_count))
print(paste("  Data quality improvement:", round((total_removed/original_rd_count)*100, 2), "% removed"))

# Additional quality metrics
rd_data$comment_length <- nchar(rd_data$comment_clean)
rd_data$word_count <- lengths(strsplit(rd_data$comment_clean, "\\s+"))

print(paste("\nReddit Comment Quality Metrics:"))
print(paste("  Average comment length:", round(mean(rd_data$comment_length), 0), "characters"))
print(paste("  Median comment length:", median(rd_data$comment_length), "characters"))
print(paste("  Average word count:", round(mean(rd_data$word_count), 0), "words"))
print(paste("  Longest comment:", max(rd_data$comment_length), "characters"))
print(paste("  Shortest comment:", min(rd_data$comment_length), "characters"))

# Save cleaned Reddit data
saveRDS(rd_data, file = "reddit_data_clean.rds")
write.csv(rd_data, file = "reddit_data_clean.csv", row.names = FALSE)

