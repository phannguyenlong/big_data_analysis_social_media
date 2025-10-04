# =========================================================================
# Q8 helper: Build second-artist datasets and actor graphs (lightweight)
# Reuses Q2 (collection) and Q3 (actor network) approach for a chosen artist
# Outputs:
#   - data/yt_data_<Slug>.rds, data/rd_data_<Slug>.rds (+ CSVs)
#   - graphs/YouTubeActor_<Slug>.rds, graphs/RedditActor_<Slug>.rds (+ GraphML)
# =========================================================================

if (!exists("dataset_dir")) dataset_dir <- ".//data//"
if (!exists("graph_dir")) graph_dir <- ".//graphs//"
if (!exists("SECOND_ARTISTS")) SECOND_ARTISTS <- c("Ed Sheeran", "Ariana Grande", "Justin Timberlake")

suppressWarnings({
  library(vosonSML)
  library(dplyr)
})

# appease R CMD check notes when run standalone (avoid get(..., ifnotfound=))
dataset_dir <- if (exists("dataset_dir", envir = .GlobalEnv)) get("dataset_dir", envir = .GlobalEnv) else ".//data//"
graph_dir <- if (exists("graph_dir", envir = .GlobalEnv)) get("graph_dir", envir = .GlobalEnv) else ".//graphs//"

# Provide default stubs; utils.R may overwrite real implementations
get_popular_threads <- function(subreddit_name, sort_by = "hot", time_filter = "week", limit = 10) {
  character(0)
}
get_multi_subreddit_threads <- function(artist_name) {
  character(0)
}

# Reuse helper utilities if available (provides real get_popular_threads)
if (file.exists("./utils.R")) {
  source("./utils.R")
}

slugify <- function(x) gsub("[^A-Za-z0-9]", "", x)
cat("\n[Q8 helper] Building datasets for: ", paste(SECOND_ARTISTS, collapse = ", "), "\n", sep = "")

# ======================
# YouTube (small sample)
# ======================

if (!exists("YOUTUBE_API_KEY")) stop("YOUTUBE_API_KEY not set; please define it before running this script.")
yt_auth <- Authenticate("youtube", apiKey = YOUTUBE_API_KEY)

collect_youtube_for_artist <- function(artist_name) {
  videos <- switch(artist_name,
    "Ed Sheeran" = c(
      "https://www.youtube.com/watch?v=JGwWNGJdvx8",
      "https://www.youtube.com/watch?v=2Vv-BfVoq4g",
      "https://www.youtube.com/watch?v=orJSJGHjBLI"
    ),
    "Ariana Grande" = c(
      "https://www.youtube.com/watch?v=QYh6mYIJG2Y",
      "https://www.youtube.com/watch?v=ffxKSjUwKdU",
      "https://www.youtube.com/watch?v=gl1aHhXnN1k"
    ),
    "Justin Timberlake" = c(
      "https://www.youtube.com/watch?v=ru0K8uYEZWw",
      "https://www.youtube.com/watch?v=FQ3slUz7Jo8",
      "https://www.youtube.com/watch?v=ISQm1FD47uM"
    ),
    c()
  )
  lst <- list()
  for (i in seq_along(videos)) {
    message(sprintf("[YouTube][%s] Collecting %d/%d", artist_name, i, length(videos)))
    video_id <- gsub(".*v=([^&]+).*", "\\1", videos[i])
    tmp <- tryCatch(yt_auth |>
                      Collect(videoIDs = video_id,
                              maxComments = 300,
                              writeToFile = FALSE,
                              verbose = TRUE),
                    error = function(e) NULL)
    if (!is.null(tmp) && is.data.frame(tmp) && nrow(tmp) > 0) {
      tmp$video_url <- videos[i]
      lst[[length(lst) + 1L]] <- tmp
    }
    Sys.sleep(2)
  }
  if (length(lst) > 0) dplyr::bind_rows(lst) else data.frame()
}

# ====================
# Reddit (small sample)
# ====================

rd_auth <- Authenticate("reddit")

# Prefer subreddit r/EdSheeran (fallback to top threads via utils if available)
get_threads_safe <- function(sub, n = 5) {
  urls <- character(0)
  if (exists("get_popular_threads")) {
    urls <- tryCatch(get_popular_threads(sub, "hot", "month", n), error = function(e) character(0))
  }
  urls <- unique(urls)
  head(urls, n)
}

collect_reddit_for_artist <- function(artist_name) {
  # Prefer Q2 multi-subreddit strategy if available
  threads <- character(0)
  if (exists("get_multi_subreddit_threads")) {
    # reference to satisfy linting without hard dependency
    get_multi_subreddit_threads_ref <- get_multi_subreddit_threads
    threads <- tryCatch(get_multi_subreddit_threads_ref(artist_name), error = function(e) character(0))
  }
  # Fallback: aggregate from artist subreddit + popheads + Music
  if (length(threads) == 0) {
    sub <- gsub(" ", "", artist_name)
    candidate_subs <- c(sub, "popheads", "Music")
    for (s in candidate_subs) {
      threads <- c(threads, get_threads_safe(s, n = 4))
      Sys.sleep(1)
    }
  }
  threads <- unique(threads)
  threads <- head(threads, 8)
  if (length(threads) == 0) return(data.frame())

  lst <- list()
  for (i in seq_along(threads)) {
    message(sprintf("[Reddit][%s] Collecting %d/%d", artist_name, i, length(threads)))
    tmp <- tryCatch(rd_auth |>
                      Collect(threadUrls = threads[i],
                              sort = "best",
                              waitTime = c(6, 8),
                              writeToFile = FALSE,
                              verbose = TRUE),
                    error = function(e) NULL)
    if (!is.null(tmp) && is.data.frame(tmp) && nrow(tmp) > 0) {
      tmp$thread_url <- threads[i]
      lst[[length(lst) + 1L]] <- tmp
    }
    Sys.sleep(5)
  }
  if (length(lst) > 0) dplyr::bind_rows(lst) else data.frame()
}

build_actor_graphs_for <- function(artist_name) {
  slug <- slugify(artist_name)

  yt_df <- collect_youtube_for_artist(artist_name)
  saveRDS(yt_df, file = paste(dataset_dir, paste0("yt_data_", slug, ".rds"), sep = ""))
  utils::write.csv(yt_df, file = paste(dataset_dir, paste0("yt_data_", slug, ".csv"), sep = ""), row.names = FALSE)
  cat("[YouTube][", artist_name, "] Rows: ", nrow(yt_df), "\n", sep = "")

  rd_df <- collect_reddit_for_artist(artist_name)
  saveRDS(rd_df, file = paste(dataset_dir, paste0("rd_data_", slug, ".rds"), sep = ""))
  utils::write.csv(rd_df, file = paste(dataset_dir, paste0("rd_data_", slug, ".csv"), sep = ""), row.names = FALSE)
  cat("[Reddit][", artist_name, "] Rows: ", nrow(rd_df), "\n", sep = "")

  if (is.data.frame(yt_df) && nrow(yt_df) > 0) {
    yt_net <- yt_df |> Create("actor") |> AddText(yt_df, repliesFromText = TRUE, verbose = TRUE) |> AddVideoData(yt_auth, actorSubOnly = TRUE, verbose = TRUE)
    yt_g <- yt_net |> Graph()
    if ("screen_name" %in% igraph::vertex_attr_names(yt_g)) igraph::V(yt_g)$name <- igraph::V(yt_g)$screen_name
    saveRDS(yt_g, file = paste(graph_dir, paste0("YouTubeActor_", slug, ".rds"), sep = ""))
    igraph::write_graph(yt_g, file = paste(graph_dir, paste0("YouTubeActor_", slug, ".graphml"), sep = ""), format = "graphml")
    cat("[Graph] YouTubeActor_", slug, " saved.\n", sep = "")
  } else {
    cat("[Graph] Skipped YouTube actor graph for ", artist_name, " (no data).\n", sep = "")
  }
  if (is.data.frame(rd_df) && nrow(rd_df) > 0) {
    rd_net <- rd_df |> Create("actor") |> AddText(rd_df, verbose = TRUE)
    rd_g <- rd_net |> Graph()
    if ("user" %in% igraph::vertex_attr_names(rd_g)) igraph::V(rd_g)$name <- igraph::V(rd_g)$user
    saveRDS(rd_g, file = paste(graph_dir, paste0("RedditActor_", slug, ".rds"), sep = ""))
    igraph::write_graph(rd_g, file = paste(graph_dir, paste0("RedditActor_", slug, ".graphml"), sep = ""), format = "graphml")
    cat("[Graph] RedditActor_", slug, " saved.\n", sep = "")
  } else {
    cat("[Graph] Skipped Reddit actor graph for ", artist_name, " (no data).\n", sep = "")
  }
}

invisible(lapply(SECOND_ARTISTS, build_actor_graphs_for))

cat("\n[Q8 helper] Done building datasets and graphs for: ", paste(SECOND_ARTISTS, collapse = ", "), "\n", sep = "")


