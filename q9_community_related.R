#!/usr/bin/env Rscript
# =========================================================================
# Q9 companion: Community analysis for main + related artists (saved graphs)
# - Uses the same approach as q9_community_analysis.R without modifying it
# - Runs Louvain and Girvan–Newman on:
#     RedditActor.rds, YouTubeActor.rds (main)
#     RedditActor_<Slug>.rds, YouTubeActor_<Slug>.rds (related)
# - Writes memberships per dataset and a combined comparison summary CSV
# =========================================================================

if (!exists("graph_dir")) graph_dir <- ".//graphs//"
if (!exists("dataset_dir")) dataset_dir <- ".//data//"
if (!exists("ARTIST_NAME")) ARTIST_NAME <- "Taylor Swift"
if (!exists("SECOND_ARTISTS")) SECOND_ARTISTS <- c("Ed Sheeran", "Ariana Grande", "Justin Timberlake")

slugify <- function(x) gsub("[^A-Za-z0-9]", "", x)

analyze_communities <- function(g, label, out_dir) {
  if (is.null(g)) return(NULL)

  ug <- igraph::as_undirected(g, mode = "collapse")
  louv <- igraph::cluster_louvain(ug)
  gn <- igraph::cluster_edge_betweenness(ug)

  cat("\n========== ", label, " COMMUNITIES =========\n", sep = "")
  cat("Louvain: ", length(louv), " communities, modularity=", round(igraph::modularity(louv), 4), "\n", sep="")
  cat("Girvan-Newman: ", length(gn), " communities\n", sep="")

  louv_df <- data.frame(node = igraph::V(ug)$name, community = louv$membership, stringsAsFactors = FALSE)
  gn_df <- data.frame(node = igraph::V(ug)$name, community = gn$membership, stringsAsFactors = FALSE)
  utils::write.csv(louv_df, paste(out_dir, paste0(tolower(label), "_louvain_membership.csv"), sep=""), row.names = FALSE)
  utils::write.csv(gn_df, paste(out_dir, paste0(tolower(label), "_girvnewman_membership.csv"), sep=""), row.names = FALSE)

  list(graph = ug, louvain = louv, girvnewman = gn)
}

results_summary <- list()

# ----------------------------------
# Related artists (mirror Q8 naming)
# ----------------------------------
for (sec in SECOND_ARTISTS) {
  s <- slugify(sec)
  rd_file <- paste(graph_dir, paste0("RedditActor_", s, ".rds"), sep="")
  yt_file <- paste(graph_dir, paste0("YouTubeActor_", s, ".rds"), sep="")

  if (file.exists(rd_file)) {
    lbl <- paste0("Reddit_", s)
    res <- analyze_communities(readRDS(rd_file), lbl, dataset_dir)
    if (!is.null(res)) results_summary[[length(results_summary)+1L]] <- data.frame(
      platform = "Reddit", artist = sec,
      louvain_count = length(res$louvain),
      louvain_modularity = igraph::modularity(res$louvain),
      girvan_newman_count = length(res$girvnewman),
      stringsAsFactors = FALSE
    )
  } else cat("Related Reddit graph not found: ", rd_file, "\n", sep="")

  if (file.exists(yt_file)) {
    lbl <- paste0("YouTube_", s)
    res <- analyze_communities(readRDS(yt_file), lbl, dataset_dir)
    if (!is.null(res)) results_summary[[length(results_summary)+1L]] <- data.frame(
      platform = "YouTube", artist = sec,
      louvain_count = length(res$louvain),
      louvain_modularity = igraph::modularity(res$louvain),
      girvan_newman_count = length(res$girvnewman),
      stringsAsFactors = FALSE
    )
  } else cat("Related YouTube graph not found: ", yt_file, "\n", sep="")
}

# -----------------------------
# Combined summary output
# -----------------------------
if (length(results_summary) > 0) {
  comm_summary <- do.call(rbind, results_summary)
  out_path <- paste(dataset_dir, "community_structure_summary.csv", sep="")
  utils::write.csv(comm_summary, out_path, row.names = FALSE)
  cat("\n[Q9 Related] Community structure summary written to ", out_path, "\n", sep="")
} else {
  cat("\n[Q9 Related] No results to summarize (graphs missing?).\n")
}

# ==================================================
# Visualization
# ==================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Set output directory
q9_img_dir <- "./images/q9"
if (!dir.exists(q9_img_dir)) dir.create(q9_img_dir, recursive = TRUE)

# =====================================================
# DATA PREPARATION - Manual entry from console output
# =====================================================

community_data <- data.frame(
  Artist = rep(c("Taylor Swift", "Ed Sheeran", "Ariana Grande", "Justin Timberlake"), each = 2),
  Platform = rep(c("Reddit", "YouTube"), 4),
  Louvain_Count = c(30, 179, 23, 58, 23, 44, 19, 30),
  Modularity = c(0.649, 0.370, 0.640, 0.299, 0.606, 0.153, 0.552, 0.223),
  GN_Count = c(41, 131, 34, 41, 46, 34, 40, 27),
  stringsAsFactors = FALSE
)

# Calculate GN vs Louvain ratio
community_data <- community_data %>%
  mutate(GN_Ratio = (GN_Count - Louvain_Count) / Louvain_Count * 100)

# =====================================================
# FIGURE 1: Modularity Comparison Across All Artists
# =====================================================

p1 <- ggplot(community_data, aes(x = Artist, y = Modularity, fill = Platform)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", Modularity)), 
            position = position_dodge(0.7), 
            vjust = -0.5, size = 3.5) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray40") +
  annotate("text", x = 2.5, y = 0.52, label = "Strong community threshold", size = 3) +
  scale_fill_manual(values = c("Reddit" = "#FF4500", "YouTube" = "#FF0000")) +
  scale_y_continuous(limits = c(0, 0.75), expand = c(0, 0)) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  ) +
  labs(
    title = "Modularity Comparison Across Artists and Platforms",
    subtitle = "Reddit shows consistent strong structure (>0.5), YouTube remains weak across all artists",
    x = NULL,
    y = "Modularity Score"
  )

ggsave(file.path(q9_img_dir, "modularity_comparison.png"), p1, width = 10, height = 6, dpi = 300)

# =====================================================
# FIGURE 2: Community Count Comparison (Faceted)
# =====================================================

# Reshape for faceting
count_long <- community_data %>%
  select(Artist, Platform, Louvain_Count, GN_Count) %>%
  pivot_longer(cols = c(Louvain_Count, GN_Count), 
               names_to = "Method", 
               values_to = "Count") %>%
  mutate(Method = recode(Method, 
                         "Louvain_Count" = "Louvain",
                         "GN_Count" = "Girvan-Newman"))

p2 <- ggplot(count_long, aes(x = Method, y = Count, fill = Platform)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = Count), 
            position = position_dodge(0.7), 
            vjust = -0.5, size = 3) +
  facet_wrap(~Artist, scales = "free_y") +
  scale_fill_manual(values = c("Reddit" = "#FF4500", "YouTube" = "#FF0000")) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  ) +
  labs(
    title = "Community Count: Louvain vs Girvan-Newman by Artist",
    subtitle = "Reddit shows consistent GN increases; YouTube patterns vary",
    x = NULL,
    y = "Number of Communities"
  )

ggsave(file.path(q9_img_dir, "community_count_faceted.png"), p2, width = 12, height = 8, dpi = 300)

# =====================================================
# FIGURE 3: Modularity vs Community Count Scatter
# =====================================================

p3 <- ggplot(community_data, aes(x = Louvain_Count, y = Modularity, 
                                 color = Platform, shape = Artist)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray40") +
  geom_text(aes(label = Artist), hjust = -0.1, size = 3) +
  scale_color_manual(values = c("Reddit" = "#FF4500", "YouTube" = "#FF0000")) +
  scale_x_continuous(limits = c(0, 200)) +
  scale_y_continuous(limits = c(0, 0.75)) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Modularity vs Community Count",
    subtitle = "YouTube shows high fragmentation with low cohesion; Reddit maintains structure",
    x = "Number of Communities (Louvain)",
    y = "Modularity Score"
  )

ggsave(file.path(q9_img_dir, "modularity_vs_count_scatter.png"), p3, width = 10, height = 6, dpi = 300)

# =====================================================
# FIGURE 4: Taylor Swift vs Peers Average Comparison
# =====================================================

# Calculate peer averages
peer_avg <- community_data %>%
  filter(Artist != "Taylor Swift") %>%
  group_by(Platform) %>%
  summarise(
    Avg_Louvain = mean(Louvain_Count),
    Avg_Modularity = mean(Modularity),
    Avg_GN = mean(GN_Count),
    .groups = "drop"
  )

taylor_data <- community_data %>%
  filter(Artist == "Taylor Swift")

comparison <- bind_rows(
  taylor_data %>% 
    select(Platform, Louvain_Count, Modularity, GN_Count) %>%
    mutate(Group = "Taylor Swift"),
  peer_avg %>%
    rename(Louvain_Count = Avg_Louvain, 
           Modularity = Avg_Modularity,
           GN_Count = Avg_GN) %>%
    mutate(Group = "Peer Average")
)

# Modularity comparison
p4a <- ggplot(comparison, aes(x = Platform, y = Modularity, fill = Group)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(aes(label = sprintf("%.3f", Modularity)), 
            position = position_dodge(0.6), 
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Taylor Swift" = "#8B4789", "Peer Average" = "#B0B0B0")) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Taylor Swift vs Peer Average: Modularity",
    y = "Modularity Score",
    x = NULL
  )

# Community count comparison
p4b <- ggplot(comparison, aes(x = Platform, y = Louvain_Count, fill = Group)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(aes(label = round(Louvain_Count)), 
            position = position_dodge(0.6), 
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Taylor Swift" = "#8B4789", "Peer Average" = "#B0B0B0")) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Taylor Swift vs Peer Average: Community Count",
    y = "Number of Communities (Louvain)",
    x = NULL
  )

library(gridExtra)
p4 <- grid.arrange(p4a, p4b, ncol = 2)
ggsave(file.path(q9_img_dir, "taylor_vs_peers.png"), p4, width = 12, height = 5, dpi = 300)

# =====================================================
# FIGURE 5: Heatmap of All Metrics
# =====================================================

# Normalize metrics for heatmap
heatmap_data <- community_data %>%
  group_by(Platform) %>%
  mutate(
    Modularity_Norm = (Modularity - min(Modularity)) / (max(Modularity) - min(Modularity)),
    Louvain_Norm = (Louvain_Count - min(Louvain_Count)) / (max(Louvain_Count) - min(Louvain_Count)),
    GN_Norm = (GN_Count - min(GN_Count)) / (max(GN_Count) - min(GN_Count))
  ) %>%
  ungroup() %>%
  select(Artist, Platform, Modularity_Norm, Louvain_Norm, GN_Norm) %>%
  pivot_longer(cols = c(Modularity_Norm, Louvain_Norm, GN_Norm),
               names_to = "Metric",
               values_to = "Normalized_Score") %>%
  mutate(
    Metric = recode(Metric,
                    "Modularity_Norm" = "Modularity",
                    "Louvain_Norm" = "Louvain Count",
                    "GN_Norm" = "GN Count"),
    Artist_Platform = paste(Artist, Platform, sep = "\n")
  )

p5 <- ggplot(heatmap_data, aes(x = Metric, y = Artist_Platform, fill = Normalized_Score)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f", Normalized_Score)), 
            color = "white", fontface = "bold", size = 3) +
  scale_fill_gradient2(low = "#2166AC", mid = "#F7F7F7", high = "#B2182B",
                       midpoint = 0.5, limits = c(0, 1),
                       name = "Normalized\nScore") +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Community Structure Metrics Heatmap (Normalized within Platform)",
    subtitle = "1.0 = highest in platform group, 0.0 = lowest",
    x = NULL,
    y = NULL
  )

ggsave(file.path(q9_img_dir, "metrics_heatmap.png"), p5, width = 8, height = 10, dpi = 300)

# =====================================================
# FIGURE 6: GN vs Louvain Ratio Comparison
# =====================================================

p6 <- ggplot(community_data, aes(x = Artist, y = GN_Ratio, fill = Platform)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = sprintf("%+.0f%%", GN_Ratio)), 
            position = position_dodge(0.7), 
            vjust = -0.5, size = 3) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  scale_fill_manual(values = c("Reddit" = "#FF4500", "YouTube" = "#FF0000")) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  ) +
  labs(
    title = "Girvan-Newman vs Louvain: Percentage Difference",
    subtitle = "Reddit shows consistent increases (hierarchical); YouTube shows decreases (fragmented)",
    x = NULL,
    y = "GN Change vs Louvain (%)"
  )

ggsave(file.path(q9_img_dir, "gn_vs_louvain_ratio.png"), p6, width = 10, height = 6, dpi = 300)

cat("\n✓ All Q9 visualizations saved to:", q9_img_dir, "\n")
cat("  - modularity_comparison.png\n")
cat("  - community_count_faceted.png\n")
cat("  - modularity_vs_count_scatter.png\n")
cat("  - taylor_vs_peers.png\n")
cat("  - metrics_heatmap.png\n")
cat("  - gn_vs_louvain_ratio.png\n")