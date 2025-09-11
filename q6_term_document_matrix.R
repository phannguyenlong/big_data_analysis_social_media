# ============================================================================
# QUESTION 6: Term Document Matrix
# After performing text pre-processing, create Term-Document Matrices for your data. What
# are the 10 terms occurring with the highest frequency? Explain the results.
# ============================================================================

preprocess_to_corpus <- function(text_vector) {
  cleaned <- text_vector |>
    replace_url() |>
    replace_html() |>
    replace_non_ascii() |>
    replace_word_elongation() |>
    replace_internet_slang() |>
    replace_contraction() |>
    removeNumbers() |>
    removePunctuation() |>
    replace_emoji() |>
    replace_emoticon()

  VCorpus(VectorSource(cleaned)) |>
    tm_map(content_transformer(tolower)) |>
    tm_map(removeWords, stopwords(kind = "SMART")) |>
    tm_map(stripWhitespace)
}

build_term_frequency <- function(corpus) {
  dtm <- DocumentTermMatrix(corpus)
  dtm_df <- as.data.frame(as.matrix(dtm))
  sort(colSums(dtm_df), decreasing = TRUE)
}

write_top_terms_csv <- function(freq, out_path, top_n = 10) {
  n <- min(top_n, length(freq))
  df <- data.frame(term = names(freq)[1:n], frequency = as.integer(freq[1:n]))
  utils::write.csv(df, out_path, row.names = FALSE)
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
rd_corpus <- preprocess_to_corpus(rd_data$comment)
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
yt_corpus <- preprocess_to_corpus(yt_data$Comment)
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
