#!/usr/bin/env Rscript
# =========================================================================
# QUESTION 9: Community analysis (Girvan-Newman and Louvain)
# Simple implementation on saved actor graphs (Reddit, YouTube)
# =========================================================================

if (!exists("graph_dir")) graph_dir <- ".//graphs//"
if (!exists("dataset_dir")) dataset_dir <- ".//data//"
if (!exists("ARTIST_NAME")) ARTIST_NAME <- "Taylor Swift"
if (!exists("SPOTIFY_ARTIST_ID")) SPOTIFY_ARTIST_ID <- ""

safe_length <- function(x) if (is.null(x)) 0 else length(x)

analyze_communities <- function(g, label, out_dir) {
  if (is.null(g)) return(NULL)

  ug <- igraph::as.undirected(g, mode = "collapse")
  louv <- igraph::cluster_louvain(ug)
  gn <- igraph::cluster_edge_betweenness(ug)

  cat("\n========== ", label, " COMMUNITIES =========\n", sep = "")
  cat("Louvain: ", igraph::length(louv), " communities, modularity=", round(igraph::modularity(louv), 4), "\n", sep="")
  cat("Girvan-Newman: ", igraph::length(gn), " communities\n", sep="")

  louv_df <- data.frame(node = igraph::V(ug)$name, community = louv$membership, stringsAsFactors = FALSE)
  gn_df <- data.frame(node = igraph::V(ug)$name, community = gn$membership, stringsAsFactors = FALSE)
  utils::write.csv(louv_df, paste(out_dir, paste0(tolower(label), "_louvain_membership.csv"), sep=""), row.names = FALSE)
  utils::write.csv(gn_df, paste(out_dir, paste0(tolower(label), "_girvnewman_membership.csv"), sep=""), row.names = FALSE)

  list(graph = ug, louvain = louv, girvnewman = gn, louv_df = louv_df)
}

summarize_artist_communities <- function(result, main_artist, related_names = character(0), label = "", out_dir = "./") {
  if (is.null(result)) return(invisible(NULL))
  ug <- result$graph
  louv <- result$louvain
  vn <- igraph::V(ug)$name

  summarize_one <- function(name_pat) {
    hits <- which(grepl(tolower(name_pat), tolower(vn), fixed = TRUE))
    if (length(hits) == 0) return(data.frame(artist = name_pat, matches = 0, communities = "", sizes = "", stringsAsFactors = FALSE))
    comm_ids <- unique(louv$membership[hits])
    comm_sizes <- sapply(comm_ids, function(id) sum(louv$membership == id))
    data.frame(
      artist = name_pat,
      matches = length(hits),
      communities = paste(comm_ids, collapse = ";"),
      sizes = paste(comm_sizes, collapse = ";"),
      stringsAsFactors = FALSE
    )
  }

  rel <- unique(related_names)
  rel <- rel[rel != main_artist]
  rel <- rel[seq_len(min(length(rel), 10))]

  rows <- list(summarize_one(main_artist))
  if (length(rel) > 0) rows <- c(rows, lapply(rel, summarize_one))
  out <- do.call(rbind, rows)

  out_path <- paste(out_dir, paste0(tolower(label), "_artist_community_summary.csv"), sep = "")
  utils::write.csv(out, out_path, row.names = FALSE)

  cat("\n", label, " – artist community summary saved: ", out_path, "\n", sep = "")
}

get_related_artists_safe <- function(artist_id) {
  if (!requireNamespace("spotifyr", quietly = TRUE)) return(character(0))
  if (is.null(artist_id) || !nzchar(artist_id)) return(character(0))
  ra <- tryCatch(spotifyr::get_related_artists(artist_id), error = function(e) NULL)
  if (is.null(ra) || nrow(ra) == 0) return(character(0))
  unique(ra$name)
}

rd_file <- paste(graph_dir, "RedditActor.rds", sep="")
yt_file <- paste(graph_dir, "YouTubeActor.rds", sep="")
rd_res <- NULL
yt_res <- NULL

if (file.exists(rd_file)) rd_res <- analyze_communities(readRDS(rd_file), "Reddit", dataset_dir) else cat("Reddit graph not found at ", rd_file, "\n", sep="")
if (file.exists(yt_file)) yt_res <- analyze_communities(readRDS(yt_file), "YouTube", dataset_dir) else cat("YouTube graph not found at ", yt_file, "\n", sep="")

related_names <- get_related_artists_safe(SPOTIFY_ARTIST_ID)
if (!is.null(rd_res)) summarize_artist_communities(rd_res, ARTIST_NAME, related_names, label = "Reddit", out_dir = dataset_dir)
if (!is.null(yt_res)) summarize_artist_communities(yt_res, ARTIST_NAME, related_names, label = "YouTube", out_dir = dataset_dir)

cat("\nNotes:\n")
cat("- Louvain finds dense groups; Girvan-Newman splits via bridging edges.\n")
cat("- Relevance: communities around your artist or official channels indicate fan subgroups or topic clusters.\n")
cat("- Compare related artists: similar community counts/sizes suggest overlapping fandom structures; differences imply distinct sub-communities.\n")
