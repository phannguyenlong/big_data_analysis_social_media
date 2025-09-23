# =========================================================================
# QUESTION 7: Reddit semantic (bigram) network
# Builds bigrams from cleaned Reddit text, removes stopwords, constructs
# a semantic network, runs PageRank, and saves all reusable artifacts.
# =========================================================================

# Ensure dirs if running standalone
if (!exists("dataset_dir")) dataset_dir <- ".//data//"
if (!exists("graph_dir")) graph_dir <- ".//graphs//"

if (!dir.exists(dataset_dir)) dir.create(dataset_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(graph_dir)) dir.create(graph_dir, recursive = TRUE, showWarnings = FALSE)

# Load pre-cleaned Reddit text from Q6
clean_text <- readRDS(paste(dataset_dir, "rd_clean_text.rds", sep = ""))

# Bigram tokenisation
clean_df <- data.frame(clean_text = clean_text, stringsAsFactors = FALSE)
rd_bigrams <- clean_df |> tidytext::unnest_tokens(
  output = bigram,
  input = clean_text,
  token = "ngrams",
  n = 2
)

# Count bigrams and split into columns
rd_bigrams_table <- rd_bigrams |>
  dplyr::count(bigram, sort = TRUE) |>
  tidyr::separate(bigram, c("left", "right"), sep = " ")

# Save raw bigram table
utils::write.csv(rd_bigrams_table, paste(dataset_dir, "rd_bigrams_all.csv", sep = ""), row.names = FALSE)

# Remove stopwords
rd_bigrams_nostops <- rd_bigrams_table |>
  dplyr::anti_join(tidytext::stop_words, by = c("left" = "word")) |>
  dplyr::anti_join(tidytext::stop_words, by = c("right" = "word"))

# Clean NA rows and keep n >= 2
rd_bigrams_nostops <- rd_bigrams_nostops[complete.cases(rd_bigrams_nostops), ] |>
  dplyr::filter(n >= 2)

# Save filtered bigram table and edge list
utils::write.csv(rd_bigrams_nostops, paste(dataset_dir, "rd_bigrams_nostops.csv", sep = ""), row.names = FALSE)
edge_list <- rd_bigrams_nostops[, c("left", "right", "n")]
utils::write.csv(edge_list, paste(dataset_dir, "rd_bigram_edges.csv", sep = ""), row.names = FALSE)

# Build semantic network
rd_bigram_graph <- igraph::graph_from_data_frame(edge_list, directed = FALSE)
rd_bigram_graph <- igraph::simplify(rd_bigram_graph)

# Save graph objects
saveRDS(rd_bigram_graph, paste(graph_dir, "RedditBigram.rds", sep = ""))
igraph::write_graph(rd_bigram_graph, file = paste(graph_dir, "RedditBigram.graphml", sep = ""), format = "graphml")

# PageRank on semantic network
rank_rd_bigram <- sort(igraph::page_rank(rd_bigram_graph)$vector, decreasing = TRUE)
top_n <- min(20, length(rank_rd_bigram))
idx <- if (top_n > 0) seq_len(top_n) else integer(0)
top_pagerank <- data.frame(term = names(rank_rd_bigram)[idx],
                           pagerank = as.numeric(rank_rd_bigram[idx]))
utils::write.csv(top_pagerank, paste(dataset_dir, "rd_bigram_pagerank.csv", sep = ""), row.names = FALSE)

# Also print to console
if (nrow(top_pagerank) > 0) {
  cat("\nTop PageRank terms (Reddit):\n")
  print(top_pagerank)
} else {
  cat("\nTop PageRank terms (Reddit): none (empty graph)\n")
}

# Console outputs
cat("\nReddit bigram network saved:\n")
cat(paste0("- Clean text: ", dataset_dir, "rd_clean_text.rds\n"))
cat(paste0("- All bigrams: ", dataset_dir, "rd_bigrams_all.csv\n"))
cat(paste0("- Filtered bigrams: ", dataset_dir, "rd_bigrams_nostops.csv\n"))
cat(paste0("- Edge list: ", dataset_dir, "rd_bigram_edges.csv\n"))
cat(paste0("- Graph RDS: ", graph_dir, "RedditBigram.rds\n"))
cat(paste0("- Graph GraphML: ", graph_dir, "RedditBigram.graphml\n"))
cat(paste0("- PageRank top terms: ", dataset_dir, "rd_bigram_pagerank.csv\n"))



# ========================================================================
# YouTube semantic (bigram) network (from Q6 cleaned text)
# ========================================================================

# Load pre-cleaned YouTube text from Q6
yt_clean_text <- readRDS(paste(dataset_dir, "yt_clean_text.rds", sep = ""))

# Bigram tokenisation
yt_df <- data.frame(clean_text = yt_clean_text, stringsAsFactors = FALSE)
yt_bigrams <- yt_df |> tidytext::unnest_tokens(
  output = bigram,
  input = clean_text,
  token = "ngrams",
  n = 2
)

# Count bigrams and split into columns
yt_bigrams_table <- yt_bigrams |>
  dplyr::count(bigram, sort = TRUE) |>
  tidyr::separate(bigram, c("left", "right"), sep = " ")

# Save raw bigram table
utils::write.csv(yt_bigrams_table, paste(dataset_dir, "yt_bigrams_all.csv", sep = ""), row.names = FALSE)

# Remove stopwords
yt_bigrams_nostops <- yt_bigrams_table |>
  dplyr::anti_join(tidytext::stop_words, by = c("left" = "word")) |>
  dplyr::anti_join(tidytext::stop_words, by = c("right" = "word"))

# Clean NA rows and keep n >= 2
yt_bigrams_nostops <- yt_bigrams_nostops[complete.cases(yt_bigrams_nostops), ] |>
  dplyr::filter(n >= 2)

# Save filtered bigram table and edge list
utils::write.csv(yt_bigrams_nostops, paste(dataset_dir, "yt_bigrams_nostops.csv", sep = ""), row.names = FALSE)
yt_edge_list <- yt_bigrams_nostops[, c("left", "right", "n")]
utils::write.csv(yt_edge_list, paste(dataset_dir, "yt_bigram_edges.csv", sep = ""), row.names = FALSE)

# Build semantic network
yt_bigram_graph <- igraph::graph_from_data_frame(yt_edge_list, directed = FALSE)
yt_bigram_graph <- igraph::simplify(yt_bigram_graph)

# Save graph objects
saveRDS(yt_bigram_graph, paste(graph_dir, "YouTubeBigram.rds", sep = ""))
igraph::write_graph(yt_bigram_graph, file = paste(graph_dir, "YouTubeBigram.graphml", sep = ""), format = "graphml")

# PageRank on semantic network
rank_yt_bigram <- sort(igraph::page_rank(yt_bigram_graph)$vector, decreasing = TRUE)
yt_top_n <- min(20, length(rank_yt_bigram))
yt_idx <- if (yt_top_n > 0) seq_len(yt_top_n) else integer(0)
yt_top_pagerank <- data.frame(term = names(rank_yt_bigram)[yt_idx],
                              pagerank = as.numeric(rank_yt_bigram[yt_idx]))
utils::write.csv(yt_top_pagerank, paste(dataset_dir, "yt_bigram_pagerank.csv", sep = ""), row.names = FALSE)

# Also print to console
if (nrow(yt_top_pagerank) > 0) {
  cat("\nTop PageRank terms (YouTube):\n")
  print(yt_top_pagerank)
} else {
  cat("\nTop PageRank terms (YouTube): none (empty graph)\n")
}

# Console outputs (YouTube)
cat("\nYouTube bigram network saved:\n")
cat(paste0("- All bigrams: ", dataset_dir, "yt_bigrams_all.csv\n"))
cat(paste0("- Filtered bigrams: ", dataset_dir, "yt_bigrams_nostops.csv\n"))
cat(paste0("- Edge list: ", dataset_dir, "yt_bigram_edges.csv\n"))
cat(paste0("- Graph RDS: ", graph_dir, "YouTubeBigram.rds\n"))
cat(paste0("- Graph GraphML: ", graph_dir, "YouTubeBigram.graphml\n"))
cat(paste0("- PageRank top terms: ", dataset_dir, "yt_bigram_pagerank.csv\n"))



# ========================================================================
# Comparative summary: Why Q7 (PageRank) differs from Q6 (frequency)
# ========================================================================

# Helper to compute and print overlap summary
print_overlap_summary <- function(freq_csv, pr_csv, label) {
  if (!file.exists(freq_csv) || !file.exists(pr_csv)) return(invisible(NULL))
  tf <- utils::read.csv(freq_csv, stringsAsFactors = FALSE)
  pr <- utils::read.csv(pr_csv, stringsAsFactors = FALSE)
  top_tf <- tolower(tf$term[seq_len(min(10, nrow(tf)))])
  top_pr <- tolower(pr$term[seq_len(min(10, nrow(pr)))])
  overlap <- intersect(top_tf, top_pr)
  only_tf <- setdiff(top_tf, top_pr)
  only_pr <- setdiff(top_pr, top_tf)
  
  cat("\n", label, " – Frequency (Q6) vs PageRank (Q7)\n", sep = "")
  cat("- Overlap count:", length(overlap), "\n")
  if (length(overlap) > 0) cat("  Overlap terms:", paste(overlap, collapse = ", "), "\n")
  if (length(only_tf) > 0) cat("- Frequency-only terms:", paste(only_tf, collapse = ", "), "\n")
  if (length(only_pr) > 0) cat("- PageRank-only terms:", paste(only_pr, collapse = ", "), "\n")
}

# Print short interpretation notes
cat("\nInterpretation notes:\n")
cat("- Q6 ranks individual words by how often they appear (bag-of-words).\n")
cat("- Q7 ranks words by how central they are in bigram networks (connectivity).\n")
cat("- PageRank elevates connector terms that link many phrases; pure high-frequency words can drop if they cluster narrowly.\n")

# Summaries for Reddit and YouTube
print_overlap_summary(
  paste(dataset_dir, "rd_top_terms.csv", sep = ""),
  paste(dataset_dir, "rd_bigram_pagerank.csv", sep = ""),
  "Reddit"
)
print_overlap_summary(
  paste(dataset_dir, "yt_top_terms.csv", sep = ""),
  paste(dataset_dir, "yt_bigram_pagerank.csv", sep = ""),
  "YouTube"
)

# ==================================================
# Visualization
# ==================================================

# FIGURE 1: Comparison of rankings
library(ggplot2)
library(dplyr)

# Reddit comparison data
reddit_comparison <- data.frame(
  Term = c("album", "taylor", "love", "people", "time", "orange", 
           "laughing", "cover", "podcast", "happy",  # Q6 terms
           "album", "taylor", "orange", "cover", "podcast", 
           "vinyl", "time", "release", "laughing", "love"),  # Q7 terms
  Method = c(rep("Frequency (Q6)", 10), rep("PageRank (Q7)", 10)),
  Rank = c(1:10, 1:10),
  Value = c(642, 554, 295, 281, 258, 248, 236, 217, 209, 183,  # Frequencies
            rep(100, 10))  # Normalized for visualization
)

p1 <- ggplot(reddit_comparison, aes(x = Rank, y = reorder(Term, -Rank), color = Method)) +
  geom_point(size = 4) +
  facet_wrap(~Method) +
  scale_color_manual(values = c("Frequency (Q6)" = "#FF4500", "PageRank (Q7)" = "#4169E1")) +
  labs(title = "Reddit: Term Ranking Methods Comparison",
       x = "Rank", y = "Term") +
  theme_minimal()

# Load the Reddit bigram graph
install.packages("ggraph")
library(igraph)
library(ggraph)
rd_bigram_graph <- readRDS("./graphs/RedditBigram.rds")

# Get top 50 nodes by PageRank for cleaner visualization
pr_scores <- page_rank(rd_bigram_graph)$vector
top_nodes <- names(sort(pr_scores, decreasing = TRUE)[1:50])
subgraph <- induced_subgraph(rd_bigram_graph, top_nodes)

# Create network plot
set.seed(123)
p2 <- ggraph(subgraph, layout = "fr") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(size = page_rank(subgraph)$vector), color = "#4169E1") +
  geom_node_text(aes(label = name, size = page_rank(subgraph)$vector), 
                 repel = TRUE, max.overlaps = 20) +
  scale_size_continuous(range = c(2, 8)) +
  labs(title = "Reddit Bigram Network: PageRank Centrality",
       subtitle = "Node size represents PageRank score") +
  theme_graph()


library(VennDiagram)

# Reddit overlap
venn.plot <- draw.pairwise.venn(
  area1 = 10,  # Q6 top 10
  area2 = 10,  # Q7 top 10
  cross.area = 8,  # Overlap
  category = c("Frequency\n(Q6)", "PageRank\n(Q7)"),
  fill = c("#FF4500", "#4169E1"),
  alpha = 0.5,
  cat.pos = c(350, 10),
  cat.dist = 0.05
)

# Save the plot
grid.draw(venn.plot)



