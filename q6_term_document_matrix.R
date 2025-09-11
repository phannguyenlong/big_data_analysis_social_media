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
