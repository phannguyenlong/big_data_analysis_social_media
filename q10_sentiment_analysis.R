# =========================================================================
# QUESTION 10: Use sentiment analysis to identify how the public reacts to events and/or topics related to
# your artist/band. Provide a summary of public opinions (emotions, reactions).
# =========================================================================

if (!exists("dataset_dir")) dataset_dir <- ".//data//"
if (!exists("graph_dir")) graph_dir <- ".//graphs//"
if (!dir.exists(graph_dir)) dir.create(graph_dir, recursive = TRUE, showWarnings = FALSE)
images_base_dir <- ".//images//"
q10_img_dir <- file.path(images_base_dir, "q10")
if (!dir.exists(images_base_dir)) dir.create(images_base_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(q10_img_dir)) dir.create(q10_img_dir, recursive = TRUE, showWarnings = FALSE)

library(dplyr)
library(tidyr)
library(tidytext)
library(syuzhet)
library(ggplot2)

# Load pre-cleaned text from Q6 (fallback to raw if needed)
safe_load_rds <- function(path) { if (file.exists(path)) readRDS(path) else NULL }

rd_clean <- safe_load_rds(paste(dataset_dir, "rd_clean_text.rds", sep = ""))
yt_clean <- safe_load_rds(paste(dataset_dir, "yt_clean_text.rds", sep = ""))

if (is.null(rd_clean) && file.exists(paste(dataset_dir, "rd_data.rds", sep = ""))) {
  rd_raw <- readRDS(paste(dataset_dir, "rd_data.rds", sep = ""))
  rd_raw <- rd_raw[complete.cases(rd_raw), ]
  rd_clean <- rd_raw$comment
}
if (is.null(yt_clean) && file.exists(paste(dataset_dir, "yt_data.rds", sep = ""))) {
  yt_raw <- readRDS(paste(dataset_dir, "yt_data.rds", sep = ""))
  yt_raw <- yt_raw[complete.cases(yt_raw), ]
  yt_clean <- yt_raw$Comment
}

# Helper: compute sentiment and emotions (AFINN + NRC)
analyze_text <- function(text_vec, platform_label) {
  if (length(text_vec) == 0) return(data.frame())
  afinn <- syuzhet::get_sentiment(text_vec, method = "afinn")
  sent_label <- ifelse(afinn > 0, "Positive", ifelse(afinn < 0, "Negative", "Neutral"))
  nrc <- syuzhet::get_nrc_sentiment(text_vec)[, 1:8]
  out <- cbind(data.frame(platform = platform_label, text = text_vec, afinn = afinn, sentiment = sent_label, stringsAsFactors = FALSE), nrc)
  out
}

rd_df <- analyze_text(rd_clean, "Reddit")
yt_df <- analyze_text(yt_clean, "YouTube")
all_df <- dplyr::bind_rows(rd_df, yt_df)

# Summary: sentiment counts per platform
sent_summary <- all_df |>
  dplyr::count(platform, sentiment, name = "count") |>
  dplyr::group_by(platform) |>
  dplyr::mutate(share = count / sum(count)) |>
  dplyr::ungroup()
utils::write.csv(sent_summary, paste(dataset_dir, "sentiment_summary.csv", sep = ""), row.names = FALSE)

# Summary: emotions share per platform
emo_cols <- c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust")
emo_summary <- all_df |>
  dplyr::group_by(platform) |>
  dplyr::summarise(dplyr::across(all_of(emo_cols), ~ mean(. > 0))) |>
  tidyr::pivot_longer(cols = all_of(emo_cols), names_to = "emotion", values_to = "proportion")
utils::write.csv(emo_summary, paste(dataset_dir, "emotion_summary.csv", sep = ""), row.names = FALSE)

# Top contributing words (bing lexicon) per platform and polarity
compute_word_contrib <- function(text_vec, platform_label) {
  if (length(text_vec) == 0) return(data.frame())
  tibble::tibble(platform = platform_label, text = text_vec) |>
    tidytext::unnest_tokens(word, text) |>
    dplyr::anti_join(tidytext::stop_words, by = c("word" = "word")) |>
    dplyr::inner_join(tidytext::get_sentiments("bing"), by = "word") |>
    dplyr::count(platform, sentiment, word, sort = TRUE)
}
rd_contrib <- compute_word_contrib(rd_clean, "Reddit")
yt_contrib <- compute_word_contrib(yt_clean, "YouTube")
all_contrib <- dplyr::bind_rows(rd_contrib, yt_contrib)
utils::write.csv(all_contrib, paste(dataset_dir, "word_contributions.csv", sep = ""), row.names = FALSE)

# --------------------
# Visualisations
# --------------------

# 1) Sentiment distribution per platform
p1 <- ggplot(sent_summary, aes(x = platform, y = share, fill = sentiment)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("Positive" = "#2ca02c", "Neutral" = "#999999", "Negative" = "#d62728")) +
  labs(title = "Sentiment Distribution by Platform", x = "", y = "Share") +
  theme_minimal()
ggsave(paste(graph_dir, "q10_sentiment_distribution.png", sep = ""), p1, width = 8, height = 5)
ggsave(file.path(q10_img_dir, "q10_sentiment_distribution.png"), p1, width = 8, height = 5)

# 2) Emotion proportions per platform
p2 <- ggplot(emo_summary, aes(x = reorder(emotion, -proportion), y = proportion, fill = platform)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("Reddit" = "#FF4500", "YouTube" = "#FF0000")) +
  labs(title = "Emotion Proportions by Platform (NRC)", x = "Emotion", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(paste(graph_dir, "q10_emotions_platform.png", sep = ""), p2, width = 9, height = 5)
ggsave(file.path(q10_img_dir, "q10_emotions_platform.png"), p2, width = 9, height = 5)

# 3) AFINN score distribution per platform
p3 <- ggplot(all_df, aes(x = afinn, fill = platform)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("Reddit" = "#FF4500", "YouTube" = "#FF0000")) +
  labs(title = "AFINN Sentiment Score Distribution", x = "AFINN score", y = "Density") +
  theme_minimal()
ggsave(paste(graph_dir, "q10_afinn_density.png", sep = ""), p3, width = 8, height = 5)
ggsave(file.path(q10_img_dir, "q10_afinn_density.png"), p3, width = 8, height = 5)

# 4) Top contributing words per platform (positive vs negative)
top_words <- all_contrib |>
  dplyr::group_by(platform, sentiment) |>
  dplyr::slice_max(n, n = 10, with_ties = FALSE) |>
  dplyr::ungroup()
p4 <- ggplot(top_words, aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ platform + sentiment, scales = "free_y") +
  scale_fill_manual(values = c("positive" = "#2ca02c", "negative" = "#d62728")) +
  labs(title = "Top Sentiment-Contributing Words by Platform", x = "Word", y = "Count") +
  theme_minimal()
ggsave(paste(graph_dir, "q10_top_words.png", sep = ""), p4, width = 10, height = 7)
ggsave(file.path(q10_img_dir, "q10_top_words.png"), p4, width = 10, height = 7)

# --------------------
# Console summary
# --------------------

cat("\n========== SENTIMENT SUMMARY =========\n")
print(sent_summary)
cat("\n========== EMOTION SUMMARY (NRC) =========\n")
print(emo_summary)
cat("\nNotes for report:\n")
cat("- We used AFINN for polarity (positive/negative/neutral) and NRC for 8 basic emotions.\n")
cat("- Results are reported for both Reddit and YouTube to capture cross-platform reactions.\n")
cat("- Visualisations saved to ", graph_dir, " (distribution, emotions, score density, top contributing words).\n", sep = "")
