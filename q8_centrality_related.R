# =========================================================================
# Q8 companion: Run the same centrality analysis for multiple related artists
# Uses the same logic as your current q8_centrality.R analyze_graph
# Looks for: graphs/RedditActor_<Slug>.rds, graphs/YouTubeActor_<Slug>.rds
# Writes: data/<label>_centrality.csv where label includes platform and artist slug
# =========================================================================

if (!exists("graph_dir")) graph_dir <- ".//graphs//"
if (!exists("dataset_dir")) dataset_dir <- ".//data//"
if (!exists("RELATED_ARTISTS")) RELATED_ARTISTS <- c("Ed Sheeran", "Ariana Grande", "Justin Timberlake")

# appease standalone runs
dataset_dir <- if (exists("dataset_dir", envir = .GlobalEnv)) get("dataset_dir", envir = .GlobalEnv) else ".//data//"

slugify <- function(x) gsub("[^A-Za-z0-9]", "", x)

analyze_graph <- function(g, label, dataset_dir, artist_name) {
  if (is.null(g)) return(invisible(NULL))

  # Largest weakly connected component for stable centralities (match user's code)
  comps <- igraph::components(g, mode = "weak")
  gc_idx <- which.max(comps$csize)
  gc <- igraph::induced_subgraph(g, vids = which(comps$membership == gc_idx))

  # Centrality measures (match user's code)
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

  # Save full table — label includes artist slug to avoid overwrites
  out_path <- paste(dataset_dir, paste0(tolower(label), "_centrality.csv"), sep = "")
  utils::write.csv(centrality_df[order(-centrality_df$degree), ], out_path, row.names = FALSE)

  cat("\n========== ", label, " CENTRALITY (Largest Component) =========\n", sep = "")
  cat("Top 10 by degree:\n"); print(utils::head(centrality_df[order(-centrality_df$degree), c("node","degree")], 10))
  cat("Top 10 by closeness:\n"); print(utils::head(centrality_df[order(-centrality_df$closeness), c("node","closeness")], 10))
  cat("Top 10 by betweenness:\n"); print(utils::head(centrality_df[order(-centrality_df$betweenness), c("node","betweenness")], 10))

  # Artist node scores (best-effort match) — same behavior
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

  invisible(centrality_df)
}

for (artist in RELATED_ARTISTS) {
  slug <- slugify(artist)
  rd_file <- paste(graph_dir, paste0("RedditActor_", slug, ".rds"), sep = "")
  yt_file <- paste(graph_dir, paste0("YouTubeActor_", slug, ".rds"), sep = "")

  if (file.exists(rd_file)) {
    rd_g <- readRDS(rd_file)
    analyze_graph(rd_g, paste0("Reddit_", slug), dataset_dir, artist)
  } else {
    cat("\nReddit graph not found for ", artist, ": ", rd_file, "\n", sep = "")
  }
  if (file.exists(yt_file)) {
    yt_g <- readRDS(yt_file)
    analyze_graph(yt_g, paste0("YouTube_", slug), dataset_dir, artist)
  } else {
    cat("\nYouTube graph not found for ", artist, ": ", yt_file, "\n", sep = "")
  }
}

cat("\n[Q8 Related] Done. CSVs written per dataset to ", dataset_dir, "\n", sep = "")

# ==================================================
# Visual comparison vs original artist (Taylor Swift)
# ==================================================
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Set output directory
q8_img_dir <- "./images/q8"
if (!dir.exists(q8_img_dir)) dir.create(q8_img_dir, recursive = TRUE)

# =====================================================
# DATA PREPARATION
# =====================================================

# Manual data entry from your console output
comparison_data <- data.frame(
  Artist = c("Taylor Swift", "Ed Sheeran", "Ariana Grande", "Justin Timberlake",
             "Taylor Swift", "Ed Sheeran", "Ariana Grande", "Justin Timberlake"),
  Platform = c(rep("YouTube", 4), rep("Reddit", 4)),
  TopNode = c("TaylorSwiftVEVO", "Ed Sheeran", "ArianaGrandeVevo", "justintimberlakeVEVO",
              "PassionateAsSin", "NA", "NA", "flopheadsbot"),
  Degree = c(4014, 904, 906, 604, 1651, 671, 670, 814),
  Betweenness = c(8395247, 388598, 348913, 183601, 2674047, 581739, 528882, 186578),
  Closeness = c(0.0001815, 0.0010081, 0.0011287, 0.0014993, 0.0001784, 0.0003611, 0.0004203, 0.0005583),
  MedianDegree = c(1, 1, 1, 1, 1, 2, 2, 2),
  MedianBetweenness = c(0, 0, 0, 0, 0, 0, 0, 0),
  MedianCloseness = c(0.0001041, 0.0005405, 0.0005824, 0.0007886, 0.0001440, 0.0002930, 0.0003495, 0.0004995)
)

# =====================================================
# FIGURE 1: Degree Centrality Comparison (Main Chart)
# =====================================================

p1 <- ggplot(comparison_data, aes(x = Artist, y = Degree, fill = Platform)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = comma(Degree)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("YouTube" = "#FF0000", "Reddit" = "#FF4500")) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = "Degree Centrality Comparison Across Artists",
    subtitle = "Taylor Swift shows 4-6x advantage over competitors on YouTube",
    x = NULL,
    y = "Degree (Number of Connections)",
    fill = "Platform"
  )

ggsave(file.path(q8_img_dir, "comparison_degree.png"), p1, width = 10, height = 6, dpi = 300)

# =====================================================
# FIGURE 2: Normalized Betweenness Comparison
# =====================================================

# Normalize betweenness for visualization (log scale)
comparison_data$BetweennessLog <- log10(comparison_data$Betweenness + 1)

p2 <- ggplot(comparison_data, aes(x = Artist, y = Betweenness, fill = Platform)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = comma(round(Betweenness))), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("YouTube" = "#FF0000", "Reddit" = "#FF4500")) +
  scale_y_log10(labels = comma, breaks = c(1e5, 1e6, 1e7)) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = "Betweenness Centrality Comparison (Log Scale)",
    subtitle = "Taylor Swift dominates bridging roles, especially on YouTube",
    x = NULL,
    y = "Betweenness (log scale)",
    fill = "Platform"
  )

ggsave(file.path(q8_img_dir, "comparison_betweenness.png"), p2, width = 10, height = 6, dpi = 300)

# =====================================================
# FIGURE 3: Closeness Centrality Comparison
# =====================================================

p3 <- ggplot(comparison_data, aes(x = Artist, y = Closeness, fill = Platform)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = sprintf("%.5f", Closeness)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("YouTube" = "#FF0000", "Reddit" = "#FF4500")) +
  scale_y_continuous(labels = function(x) sprintf("%.4f", x), 
                     expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = "Closeness Centrality Comparison",
    subtitle = "Lower YouTube closeness for Taylor indicates more distributed network",
    x = NULL,
    y = "Closeness (normalized)",
    fill = "Platform"
  )

ggsave(file.path(q8_img_dir, "comparison_closeness.png"), p3, width = 10, height = 6, dpi = 300)

# =====================================================
# FIGURE 4: YouTube-Only Detailed Comparison (Ratios)
# =====================================================

youtube_data <- comparison_data %>% filter(Platform == "YouTube")
youtube_data <- youtube_data %>%
  mutate(
    DegreeRatio = Degree / Degree[Artist == "Taylor Swift"],
    Artist = factor(Artist, levels = c("Taylor Swift", "Ariana Grande", 
                                       "Ed Sheeran", "Justin Timberlake"))
  )

p4 <- ggplot(youtube_data, aes(x = Artist, y = DegreeRatio, fill = Artist)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  geom_text(aes(label = sprintf("%.2fx", DegreeRatio)), 
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Taylor Swift" = "#8B4789", 
                               "Ariana Grande" = "#FFB6C1",
                               "Ed Sheeran" = "#FFA500",
                               "Justin Timberlake" = "#4169E1")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = "YouTube Degree Centrality: Relative to Taylor Swift",
    subtitle = "Taylor Swift's network is 4-6x larger than competitors",
    x = NULL,
    y = "Ratio to Taylor Swift (1.0 = baseline)"
  )

ggsave(file.path(q8_img_dir, "youtube_ratio_comparison.png"), p4, width = 8, height = 6, dpi = 300)

# =====================================================
# FIGURE 5: All Metrics Heatmap
# =====================================================

# Normalize all metrics to 0-1 scale for heatmap
heatmap_data <- comparison_data %>%
  group_by(Platform) %>%
  mutate(
    DegreeNorm = (Degree - min(Degree)) / (max(Degree) - min(Degree)),
    BetweennessNorm = (Betweenness - min(Betweenness)) / (max(Betweenness) - min(Betweenness)),
    ClosenessNorm = (Closeness - min(Closeness)) / (max(Closeness) - min(Closeness))
  ) %>%
  ungroup() %>%
  select(Artist, Platform, DegreeNorm, BetweennessNorm, ClosenessNorm) %>%
  pivot_longer(cols = c(DegreeNorm, BetweennessNorm, ClosenessNorm),
               names_to = "Metric", values_to = "NormalizedScore") %>%
  mutate(
    Metric = recode(Metric,
                    "DegreeNorm" = "Degree",
                    "BetweennessNorm" = "Betweenness",
                    "ClosenessNorm" = "Closeness"),
    ArtistPlatform = paste(Artist, Platform, sep = "\n")
  )

p5 <- ggplot(heatmap_data, aes(x = Metric, y = ArtistPlatform, fill = NormalizedScore)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f", NormalizedScore)), 
            color = "white", fontface = "bold", size = 3.5) +
  scale_fill_gradient2(low = "#2166AC", mid = "#F7F7F7", high = "#B2182B",
                       midpoint = 0.5, limits = c(0, 1),
                       name = "Normalized\nScore") +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Centrality Metrics Heatmap (Normalized within Platform)",
    subtitle = "1.0 = highest in platform group, 0.0 = lowest",
    x = NULL,
    y = NULL
  )

ggsave(file.path(q8_img_dir, "heatmap_all_metrics.png"), p5, width = 8, height = 10, dpi = 300)

# =====================================================
# FIGURE 6: Summary Table Visualization
# =====================================================

library(gridExtra)
library(grid)

# Create summary table
summary_table <- comparison_data %>%
  select(Artist, Platform, Degree, Betweenness, Closeness) %>%
  mutate(
    Degree = comma(Degree),
    Betweenness = comma(round(Betweenness)),
    Closeness = sprintf("%.6f", Closeness)
  )

# Convert to grob
table_grob <- tableGrob(summary_table, rows = NULL, theme = ttheme_minimal(
  core = list(fg_params = list(cex = 0.8)),
  colhead = list(fg_params = list(cex = 0.9, fontface = "bold"))
))

ggsave(file.path(q8_img_dir, "summary_table.png"), 
       table_grob, width = 12, height = 6, dpi = 300)

cat("\n✓ All comparison visualizations saved to:", q8_img_dir, "\n")
cat("  - comparison_degree.png\n")
cat("  - comparison_betweenness.png\n")
cat("  - comparison_closeness.png\n")
cat("  - youtube_ratio_comparison.png\n")
cat("  - heatmap_all_metrics.png\n")
cat("  - summary_table.png\n")
