# =========================================================================
# QUESTION 8: degree centrality, betweenness centrality, and closeness centrality
# Perform centrality analysis by detecting degree centrality, betweenness centrality, and
# closeness centrality. Explain how relevant the results are to your artist/band. What are the
# actual degree, betweenness, and closeness centrality scores for your artist/band node in the
# network? Compare these scores to the scores for other artists that are related to your artist/band.
# =========================================================================

analyze_graph <- function(g, label, dataset_dir, artist_name) {
  if (is.null(g)) return(invisible(NULL))

  # Largest weakly connected component for stable centralities
  comps <- igraph::components(g, mode = "weak")
  gc_idx <- which.max(comps$csize)
  gc <- igraph::induced_subgraph(g, vids = which(comps$membership == gc_idx))

  # Centrality measures
  deg <- igraph::degree(gc, mode = "all")
  clo <- igraph::closeness(gc, mode = "all")
  bet <- igraph::betweenness(gc, directed = FALSE)

  centrality_df <- data.frame(
    node = igraph::V(gc)$name,
    degree = as.numeric(deg),
    closeness = as.numeric(clo),
    betweenness = as.numeric(bet),
    stringsAsFactors = FALSE
  )

  # Save full table
  out_path <- paste(dataset_dir, paste0(tolower(label), "_centrality.csv"), sep = "")
  utils::write.csv(centrality_df[order(-centrality_df$degree), ], out_path, row.names = FALSE)

  cat("\n========== ", label, " CENTRALITY (Largest Component) =========\n", sep = "")
  cat("Top 10 by degree:\n"); print(utils::head(centrality_df[order(-centrality_df$degree), c("node","degree")], 10))
  cat("Top 10 by closeness:\n"); print(utils::head(centrality_df[order(-centrality_df$closeness), c("node","closeness")], 10))
  cat("Top 10 by betweenness:\n"); print(utils::head(centrality_df[order(-centrality_df$betweenness), c("node","betweenness")], 10))

  # Artist node scores (best-effort match)
  name_vec <- igraph::V(gc)$name
  idx <- which(tolower(name_vec) == tolower(artist_name))
  if (length(idx) == 0) {
    idx <- which(grepl(tolower(artist_name), tolower(name_vec), fixed = TRUE))
  }

  if (length(idx) > 0) {
    i <- idx[1]
    cat("\nArtist node found in ", label, ": ", name_vec[i], "\n", sep = "")
    cat(sprintf("Degree: %d\n", centrality_df$degree[i]))
    cat(sprintf("Closeness: %.6f\n", centrality_df$closeness[i]))
    cat(sprintf("Betweenness: %.6f\n", centrality_df$betweenness[i]))
  } else {
    cat("\nArtist node not found in ", label, " actor network (likely not present as a user).\n", sep = "")
  }
}

# Analyze Reddit actor graph
rd_path <- paste(graph_dir, "RedditActor.rds", sep = "")
if (file.exists(rd_path)) {
  rd_graph <- readRDS(rd_path)
  analyze_graph(rd_graph, "Reddit", dataset_dir, ARTIST_NAME)
} else {
  cat("\nReddit actor graph not found at ", rd_path, "\n", sep = "")
}

# Analyze YouTube actor graph
yt_path <- paste(graph_dir, "YouTubeActor.rds", sep = "")
if (file.exists(yt_path)) {
  yt_graph <- readRDS(yt_path)
  analyze_graph(yt_graph, "YouTube", dataset_dir, ARTIST_NAME)
} else {
  cat("\nYouTube actor graph not found at ", yt_path, "\n", sep = "")
}