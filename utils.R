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
    Sys.sleep(2)  # Delay to prevent rate limit
  }
  
  # Remove duplicates
  all_threads <- unique(all_threads)
  
  return(all_threads)
}