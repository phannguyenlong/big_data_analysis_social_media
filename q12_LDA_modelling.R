# =========================================================================
# QUESTION 12: Use LDA topic modelling to identify some terms that are closely related to your artist/band.
# Find at least 3 significant groups of words that can be meaningful to your analysis. Explain
# your findings
# =========================================================================

if (!exists("dataset_dir")) dataset_dir <- ".//data//"
if (!exists("graph_dir")) graph_dir <- ".//graphs//"
if (!dir.exists(dataset_dir)) dir.create(dataset_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(graph_dir)) dir.create(graph_dir, recursive = TRUE, showWarnings = FALSE)

library(dplyr)
library(tidyr)
library(tidytext)
library(textclean)
library(tm)
library(topicmodels)
library(ggplot2)

# --------------------------------------------------------------------------
# Helpers
# --------------------------------------------------------------------------

safe_load_rds <- function(path) { if (file.exists(path)) readRDS(path) else NULL }

clean_text_vector <- function(x) {
  x <- as.character(x)
  x <- replace_url(x) |>
    replace_html() |>
    replace_non_ascii() |>
    replace_word_elongation() |>
    replace_internet_slang() |>
    replace_contraction() |>
    tm::removeNumbers() |>
    tm::removePunctuation()
  x <- trimws(x)
  x[nzchar(x)]
}

build_corpus <- function(clean_vec) {
  tm::VCorpus(tm::VectorSource(clean_vec)) |>
    tm::tm_map(tm::content_transformer(tolower)) |>
    tm::tm_map(tm::removeWords, tm::stopwords(kind = "SMART")) |>
    tm::tm_map(tm::stripWhitespace)
}

build_dtm <- function(corpus) {
  dtm <- tm::DocumentTermMatrix(corpus)
  # Remove zero rows (empty docs)
  nz <- unique(dtm$i)
  if (length(nz) > 0) dtm <- dtm[nz, ]
  dtm
}

print_top_terms_to_console <- function(beta_df, label, top_n = 10) {
  cat("\n[Q12] Top", top_n, "terms per topic for", label, "\n")
  topics <- sort(unique(beta_df$topic))
  for (t in topics) {
    sub <- beta_df[beta_df$topic == t, , drop = FALSE]
    sub <- sub[order(-sub$beta), , drop = FALSE]
    terms <- utils::head(sub$term, top_n)
    cat(sprintf("  Topic %s: %s\n", as.character(t), paste(terms, collapse = ", ")))
  }
}

print_three_significant_topics <- function(beta_df, label, artist_name = NULL, top_n = 10) {
  # Choose topics by total beta mass and print a brief, human-readable summary
  sums <- beta_df |>
    dplyr::group_by(topic) |>
    dplyr::summarise(total_beta = sum(beta), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(total_beta))
  chosen <- utils::head(sums$topic, 3)
  cat("\n[Q12] Three significant topics for", label, "(by total beta mass):\n")
  for (t in chosen) {
    sub <- beta_df[beta_df$topic == t, , drop = FALSE]
    sub <- sub[order(-sub$beta), , drop = FALSE]
    terms <- utils::head(sub$term, top_n)
    cat(sprintf("  Topic %s → %s\n", as.character(t), paste(terms, collapse = ", ")))
  }
  # Lightweight generic interpretation guidance
  who <- if (is.null(artist_name)) "the artist" else artist_name
  cat("[Q12] Interpretation guide:")
  cat("\n  - Topic may capture eras/releases (album_year cues, title terms) for", who, ".")
  cat("\n  - Collaboration/performance signals (feat., live, acoustic) can define distinct themes.")
  cat("\n  - Emotion/story words likely map to songwriting and fan discussion angles.\n")
}

top_terms_plot <- function(beta_df, title_text, out_path) {
  top_terms <- beta_df |>
    group_by(topic) |>
    slice_max(beta, n = 10) |>
    ungroup() |>
    arrange(topic, -beta)
  p <- top_terms |>
    mutate(term = tidytext::reorder_within(term, beta, topic)) |>
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    tidytext::scale_y_reordered() +
    labs(title = title_text, x = "Beta (word probability)", y = "Term") +
    theme_minimal()
  ggsave(out_path, p, width = 10, height = 6)
  invisible(top_terms)
}

# --------------------------------------------------------------------------
# Load and prepare data (multiple datasets: Reddit and YouTube)
# --------------------------------------------------------------------------

rd_clean <- safe_load_rds(paste(dataset_dir, "rd_clean_text.rds", sep = ""))
yt_clean <- safe_load_rds(paste(dataset_dir, "yt_clean_text.rds", sep = ""))

if (is.null(rd_clean) && file.exists(paste(dataset_dir, "rd_data.rds", sep = ""))) {
  rd_raw <- readRDS(paste(dataset_dir, "rd_data.rds", sep = ""))
  rd_raw <- rd_raw[complete.cases(rd_raw), ]
  rd_clean <- clean_text_vector(rd_raw$comment)
}
if (is.null(yt_clean) && file.exists(paste(dataset_dir, "yt_data.rds", sep = ""))) {
  yt_raw <- readRDS(paste(dataset_dir, "yt_data.rds", sep = ""))
  yt_raw <- yt_raw[complete.cases(yt_raw), ]
  yt_clean <- clean_text_vector(yt_raw$Comment)
}

cat("\n[Q12] Reddit docs:", ifelse(is.null(rd_clean), 0L, length(rd_clean)),
    " YouTube docs:", ifelse(is.null(yt_clean), 0L, length(yt_clean)), "\n")

# --------------------------------------------------------------------------
# LDA on Reddit
# --------------------------------------------------------------------------

if (!is.null(rd_clean) && length(rd_clean) > 0) {
  rd_corpus <- build_corpus(rd_clean)
  rd_dtm <- build_dtm(rd_corpus)
  cat("[Q12] Reddit DTM dims:", nrow(rd_dtm), "docs x", ncol(rd_dtm), "terms\n")
  k_rd <- 6  # choose at least 3 topics; 6 provides granularity
  lda_rd <- topicmodels::LDA(rd_dtm, k = k_rd, control = list(seed = 1337))
  beta_rd <- tidytext::tidy(lda_rd, matrix = "beta")
  out_csv_rd <- paste(dataset_dir, "q12_reddit_top_terms.csv", sep = "")
  top_rd <- top_terms_plot(beta_rd, "Q12 Reddit: Top terms per topic", paste(graph_dir, "q12_reddit_topics.png", sep = ""))
  utils::write.csv(top_rd, out_csv_rd, row.names = FALSE)
  cat("[Q12] Reddit top terms saved:", out_csv_rd, "\n")
  print_top_terms_to_console(beta_rd, "Reddit", top_n = 10)
  artist_label <- if (exists("ARTIST_NAME")) ARTIST_NAME else NULL
  print_three_significant_topics(beta_rd, "Reddit", artist_label, top_n = 10)
} else {
  cat("[Q12] Reddit text unavailable – skipping Reddit LDA.\n")
}

# --------------------------------------------------------------------------
# LDA on YouTube
# --------------------------------------------------------------------------

if (!is.null(yt_clean) && length(yt_clean) > 0) {
  yt_corpus <- build_corpus(yt_clean)
  yt_dtm <- build_dtm(yt_corpus)
  cat("[Q12] YouTube DTM dims:", nrow(yt_dtm), "docs x", ncol(yt_dtm), "terms\n")
  k_yt <- 6
  lda_yt <- topicmodels::LDA(yt_dtm, k = k_yt, control = list(seed = 1337))
  beta_yt <- tidytext::tidy(lda_yt, matrix = "beta")
  out_csv_yt <- paste(dataset_dir, "q12_youtube_top_terms.csv", sep = "")
  top_yt <- top_terms_plot(beta_yt, "Q12 YouTube: Top terms per topic", paste(graph_dir, "q12_youtube_topics.png", sep = ""))
  utils::write.csv(top_yt, out_csv_yt, row.names = FALSE)
  cat("[Q12] YouTube top terms saved:", out_csv_yt, "\n")
  print_top_terms_to_console(beta_yt, "YouTube", top_n = 10)
  artist_label <- if (exists("ARTIST_NAME")) ARTIST_NAME else NULL
  print_three_significant_topics(beta_yt, "YouTube", artist_label, top_n = 10)
} else {
  cat("[Q12] YouTube text unavailable – skipping YouTube LDA.\n")
}

# --------------------------------------------------------------------------
# Brief interpretation notes (console)
# --------------------------------------------------------------------------

cat("\n========== Q12 TOPIC MODELLING SUMMARY =========\n")
cat("- We trained separate LDA models for Reddit and YouTube (multiple datasets).\n")
cat("- Each model used k = 6 topics and standard text pre-processing taught in class.\n")
cat("- See saved figures q12_reddit_topics.png and q12_youtube_topics.png for top terms per topic.\n")
cat("- Typical themes for this artist often include album/era terms, collaboration cues (feat.), and emotion/story words.\n")
cat("- Select at least 3 coherent topics from the figures and explain how they relate to the artist’s eras, releases, or fan discussions.\n")
