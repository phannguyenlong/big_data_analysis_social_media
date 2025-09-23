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



# ==================================================
# Visualization
# ==================================================

# CODE FOR FIGURE 1: Centrality comparison visualization
library(ggplot2)
library(tidyr)

# Create centrality comparison data
centrality_data <- data.frame(
  User = c("PassionateAsSin", "Lyd_Euh", "aran130711",
           "TaylorSwiftVEVO", "justintimberlakeVEVO", "@Christina-y5s"),
  Platform = c(rep("Reddit", 3), rep("YouTube", 3)),
  Degree = c(1651, 672, 425, 4014, 502, 66),
  Betweenness = c(2674047, 1369532, 277825, 8395247, 1990641, 76243) / 100000, # Scale for visualization
  Closeness = c(0.0001784, 0.0001439, 0.0001439, 0.0001815, 0.0001162, 0.0001041) * 10000 # Scale for visualization
)

# Reshape for plotting
centrality_long <- pivot_longer(centrality_data, 
                                cols = c(Degree, Betweenness, Closeness),
                                names_to = "Metric", 
                                values_to = "Score")

p1 <- ggplot(centrality_long, aes(x = User, y = Score, fill = Platform)) +
  geom_col() +
  facet_wrap(~Metric, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Centrality Measures: Top 3 Users per Platform",
       subtitle = "Degree (connections), Betweenness (bridge role), Closeness (reach)") +
  scale_fill_manual(values = c("Reddit" = "#FF4500", "YouTube" = "#FF0000"))

ggsave("q8_figure1_centrality.png", p1, width = 12, height = 6)

# CODE FOR FIGURE 2: Artist comparison
artist_comparison <- data.frame(
  Artist = c("TaylorSwiftVEVO", "justintimberlakeVEVO"),
  Degree = c(4014, 502),
  Betweenness_Millions = c(8.395, 1.991),
  Closeness_x10000 = c(1.815, 1.162)
)

# Create multi-metric comparison
p2 <- ggplot(artist_comparison) +
  geom_segment(aes(x = 0, xend = Degree/100, y = Artist, yend = Artist), 
               size = 10, color = "#1f77b4", alpha = 0.7) +
  geom_text(aes(x = Degree/100, y = Artist, label = paste("Degree:", Degree)), 
            hjust = -0.1, size = 3) +
  labs(title = "Artist Channel Centrality Comparison",
       subtitle = "TaylorSwiftVEVO dominates all centrality metrics",
       x = "Centrality Score (normalized)", y = "") +
  theme_minimal() +
  xlim(0, 50)

ggsave("q8_figure2_artist_comparison.png", p2, width = 10, height = 4)