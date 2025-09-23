# ============================================================================
# QUESTION 6: Term Document Matrix
# After performing text pre-processing, create Term-Document Matrices for your data. What
# are the 10 terms occurring with the highest frequency? Explain the results.
# ============================================================================

preprocess_to_corpus <- function(text_vector) {
  cleaned <- text_vector |>
    textclean::replace_url() |>
    textclean::replace_html() |>
    textclean::replace_non_ascii() |>
    textclean::replace_word_elongation() |>
    textclean::replace_internet_slang() |>
    textclean::replace_contraction() |>
    tm::removeNumbers() |>
    tm::removePunctuation() |>
    textclean::replace_emoji() |>
    textclean::replace_emoticon()

  tm::VCorpus(tm::VectorSource(cleaned)) |>
    tm::tm_map(tm::content_transformer(tolower)) |>
    tm::tm_map(tm::removeWords, tm::stopwords(kind = "SMART")) |>
    tm::tm_map(tm::stripWhitespace)
}

build_term_frequency <- function(corpus) {
  dtm <- tm::DocumentTermMatrix(corpus)
  dtm_df <- as.data.frame(as.matrix(dtm))
  sort(colSums(dtm_df), decreasing = TRUE)
}

write_top_terms_csv <- function(freq, out_path, top_n = 10) {
  n <- min(top_n, length(freq))
  df <- data.frame(term = names(freq)[1:n], frequency = as.integer(freq[1:n]))
  utils::write.csv(df, out_path, row.names = FALSE)
}

corpus_from_clean <- function(cleaned_text) {
  tm::VCorpus(tm::VectorSource(cleaned_text)) |>
    tm::tm_map(tm::content_transformer(tolower)) |>
    tm::tm_map(tm::removeWords, tm::stopwords(kind = "SMART")) |>
    tm::tm_map(tm::stripWhitespace)
}

# ============================================================================
# REDDIT
# ============================================================================

# Load redit data ------------
rd_data <- readRDS(paste(dataset_dir,"rd_data.rds",sep=""))

# Data Cleaning ------------
# remove N/A Row
rd_data <- rd_data[complete.cases(rd_data), ]

# Build corpus and term frequency
rd_clean_text <- rd_data$comment |>
  textclean::replace_url() |>
  textclean::replace_html() |>
  textclean::replace_non_ascii() |>
  textclean::replace_word_elongation() |>
  textclean::replace_internet_slang() |>
  textclean::replace_contraction() |>
  tm::removeNumbers() |>
  tm::removePunctuation() |>
  textclean::replace_emoji() |>
  textclean::replace_emoticon()

# Save cleaned Reddit text for reuse (Q7)
saveRDS(rd_clean_text, paste(dataset_dir, "rd_clean_text.rds", sep = ""))
utils::write.csv(data.frame(text = rd_clean_text), paste(dataset_dir, "rd_clean_text.csv", sep = ""), row.names = FALSE)

rd_corpus <- corpus_from_clean(rd_clean_text)
rd_freq <- build_term_frequency(rd_corpus)

# Show and save top 10
cat("\nTop 10 Reddit terms:\n")
print(utils::head(rd_freq, 10))
write_top_terms_csv(rd_freq, paste(dataset_dir, "rd_top_terms.csv", sep = ""), 10)

# Simple plot of top 10 terms
rd_top10 <- utils::head(rd_freq, 10)
rd_plot_df <- data.frame(word = names(rd_top10), freq = as.integer(rd_top10))
ggplot(rd_plot_df, aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("Reddit: Top 10 Terms") +
  xlab("") +
  ylab("Frequency")


# ============================================================================
# YOUTUBE
# ============================================================================
# Load youtube data ------------
yt_data <- readRDS(paste(dataset_dir,"yt_data.rds",sep=""))

# Data Cleaning ------------
# remove N/A Row
yt_data <- yt_data[complete.cases(yt_data), ]

# Build corpus and term frequency
yt_clean_text <- yt_data$Comment |>
  textclean::replace_url() |>
  textclean::replace_html() |>
  textclean::replace_non_ascii() |>
  textclean::replace_word_elongation() |>
  textclean::replace_internet_slang() |>
  textclean::replace_contraction() |>
  tm::removeNumbers() |>
  tm::removePunctuation() |>
  textclean::replace_emoji() |>
  textclean::replace_emoticon()

# Save cleaned YouTube text for reuse
saveRDS(yt_clean_text, paste(dataset_dir, "yt_clean_text.rds", sep = ""))
utils::write.csv(data.frame(text = yt_clean_text), paste(dataset_dir, "yt_clean_text.csv", sep = ""), row.names = FALSE)

yt_corpus <- corpus_from_clean(yt_clean_text)
yt_freq <- build_term_frequency(yt_corpus)

# Show and save top 10
cat("\nTop 10 YouTube terms:\n")
print(utils::head(yt_freq, 10))
write_top_terms_csv(yt_freq, paste(dataset_dir, "yt_top_terms.csv", sep = ""), 10)

# Simple plot of top 10 terms
yt_top10 <- utils::head(yt_freq, 10)
yt_plot_df <- data.frame(word = names(yt_top10), freq = as.integer(yt_top10))
ggplot(yt_plot_df, aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("YouTube: Top 10 Terms") +
  xlab("") +
  ylab("Frequency")

# ==================================================
# Visualization
# ==================================================

library(ggplot2)
library(tidyr)
library(dplyr)

# Create combined dataset
term_data <- data.frame(
  Term = c("album", "taylor", "love", "people", "time", "orange", 
           "laughing", "cover", "podcast", "happy",
           "song", "taylor", "excuse", "orange", "people", 
           "laughing", "love", "album", "loud", "songs"),
  Frequency = c(642, 554, 295, 281, 258, 248, 236, 217, 209, 183,
                28, 21, 14, 13, 13, 12, 11, 10, 10, 10),
  Platform = c(rep("Reddit", 10), rep("YouTube", 10)),
  Rank = c(1:10, 1:10)
)

# Figure 1: Side-by-side comparison
p1 <- ggplot(term_data, aes(x = reorder(Term, -Frequency), y = Frequency, fill = Platform)) +
  geom_col(position = "dodge") +
  facet_wrap(~Platform, scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top 10 Terms by Platform",
       x = "Term", y = "Frequency") +
  scale_fill_manual(values = c("Reddit" = "#FF4500", "YouTube" = "#FF0000"))

ggsave("q6_figure1_terms_comparison.png", p1, width = 10, height = 6)


# CODE FOR FIGURE 2: Scatter plot comparison
common_terms <- c("taylor", "album", "love", "people", "orange", "laughing")
comparison_data <- term_data %>%
  filter(Term %in% common_terms) %>%
  pivot_wider(names_from = Platform, values_from = Frequency, values_fill = 0)

p2 <- ggplot(comparison_data, aes(x = Reddit, y = YouTube, label = Term)) +
  geom_point(size = 4, color = "#1DB954") +
  geom_text(vjust = -1, hjust = 0.5, size = 3.5) +
  labs(title = "Term Frequency: Reddit vs YouTube",
       subtitle = "Common terms across platforms (diagonal line = equal frequency)",
       x = "Reddit Frequency", y = "YouTube Frequency") +
  theme_minimal() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.3) +
  scale_x_continuous(limits = c(0, 650)) +
  scale_y_continuous(limits = c(0, 30))

ggsave("q6_figure2_correlation.png", p2, width = 8, height = 8)

# CODE FOR FIGURE 3: Word clouds
library(wordcloud)
library(RColorBrewer)

# Reproducible layout for word clouds
set.seed(42)

# Reddit word cloud (all terms)
png("q6_figure3a_reddit_wordcloud.png", width = 800, height = 600)
wordcloud(words = names(rd_freq),
          freq = as.integer(rd_freq),
          min.freq = 1,
          max.words = length(rd_freq),
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"),
          scale = c(5, 0.5))
title("Reddit: All Terms")
dev.off()

# YouTube word cloud (all terms)
png("q6_figure3b_youtube_wordcloud.png", width = 800, height = 600)
wordcloud(words = names(yt_freq),
          freq = as.integer(yt_freq),
          min.freq = 1,
          max.words = length(yt_freq),
          random.order = FALSE,
          colors = brewer.pal(8, "Set2"),
          scale = c(5, 0.5))
title("YouTube: All Terms")
dev.off()