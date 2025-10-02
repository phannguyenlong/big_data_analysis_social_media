# Create Power BI export directory
powerbi_dir <- "./data/powerbi_export/"
if (!dir.exists(powerbi_dir)) dir.create(powerbi_dir, recursive = TRUE)

# Dataset 1: Platform Overview Metrics
platform_metrics <- data.frame(
  Platform = c("Reddit", "YouTube"),
  Total_Comments = c(4968, 5076),
  Unique_Actors = c(2865, 4129),
  Avg_Comments_Per_Actor = c(1.73, 1.23),
  Positive_Pct = c(39.3, 22.7),
  Neutral_Pct = c(42.3, 68.6),
  Negative_Pct = c(18.4, 8.7)
)
write.csv(platform_metrics, paste0(powerbi_dir, "platform_metrics.csv"), row.names = FALSE)

# Dataset 2: Emotion Analysis (8 emotions x 2 platforms)
emotion_data <- read.csv("./data/emotion_summary.csv")
write.csv(emotion_data, paste0(powerbi_dir, "emotion_analysis.csv"), row.names = FALSE)

# Dataset 3: Top 15 Terms Per Platform (for frequency comparison)
rd_terms <- read.csv("./data/rd_top_terms.csv")
yt_terms <- read.csv("./data/yt_top_terms.csv")
term_comparison <- data.frame(
  Term = c("album", "taylor", "love", "people", "time", "orange", 
           "laughing", "cover", "podcast", "happy", "song", "excuse", "loud", "songs", "version"),
  Reddit_Freq = c(642, 554, 295, 281, 258, 248, 236, 217, 209, 183, 0, 0, 0, 0, 0),
  YouTube_Freq = c(10, 21, 11, 13, 0, 13, 12, 0, 0, 0, 28, 14, 10, 10, 0)
)
term_comparison$Platform_Ratio <- term_comparison$Reddit_Freq / pmax(term_comparison$YouTube_Freq, 1)
write.csv(term_comparison, paste0(powerbi_dir, "term_frequency_gap.csv"), row.names = FALSE)

# Dataset 4: Community Analysis
community_data <- data.frame(
  Platform = c("Reddit", "YouTube"),
  Louvain_Communities = c(30, 179),
  GirvanNewman_Communities = c(41, 131),
  Modularity = c(0.649, 0.370),
  Top_Influencer = c("PassionateAsSin", "TaylorSwiftVEVO"),
  Top_Degree = c(1651, 4014)
)
write.csv(community_data, paste0(powerbi_dir, "community_structure.csv"), row.names = FALSE)

# Dataset 5: Bigram PageRank vs Frequency (NEW INSIGHT DATA)
rd_freq_map <- setNames(rd_terms$frequency, tolower(rd_terms$term))
rd_bigram <- read.csv("./data/rd_bigram_pagerank.csv")
bigram_insight <- head(rd_bigram, 12)
bigram_insight$frequency <- sapply(tolower(bigram_insight$term), function(t) {
  if (t %in% names(rd_freq_map)) rd_freq_map[t] else 0
})
bigram_insight$rank_type <- ifelse(bigram_insight$frequency == 0, "PageRank Only", "Both Methods")
write.csv(bigram_insight, paste0(powerbi_dir, "bigram_vs_frequency.csv"), row.names = FALSE)

# Dataset 6: Sentiment Score Distribution (NEW INSIGHT DATA)
rd_raw <- readRDS("./data/rd_data.rds")
yt_raw <- readRDS("./data/yt_data.rds")
rd_sentiment <- syuzhet::get_sentiment(rd_raw$comment, method = "afinn")
yt_sentiment <- syuzhet::get_sentiment(yt_raw$Comment, method = "afinn")
sentiment_distribution <- data.frame(
  Platform = c(rep("Reddit", length(rd_sentiment)), rep("YouTube", length(yt_sentiment))),
  AFINN_Score = c(rd_sentiment, yt_sentiment)
)
write.csv(sentiment_distribution, paste0(powerbi_dir, "sentiment_score_distribution.csv"), row.names = FALSE)

cat("\n✓ Power BI datasets ready in:", powerbi_dir, "\n")