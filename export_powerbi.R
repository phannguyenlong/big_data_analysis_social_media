# =========================================================================
# Export datasets for Power BI (Q10, Q11, Q12, networks)
# Outputs written to ./powerBI/
# =========================================================================

if (!exists("dataset_dir")) dataset_dir <- ".//data//"
if (!exists("graph_dir")) graph_dir <- ".//graphs//"
powerbi_dir <- ".//powerBI//"
if (!dir.exists(powerbi_dir)) dir.create(powerbi_dir, recursive = TRUE, showWarnings = FALSE)

suppressWarnings({
  library(dplyr)
  library(tidyr)
  library(tidytext)
  library(textclean)
  library(tm)
  library(topicmodels)
  library(syuzhet)
  library(lubridate)
})

safe_load_rds <- function(path) { if (file.exists(path)) readRDS(path) else NULL }
safe_read_csv <- function(path) { if (file.exists(path)) tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL) else NULL }

# ------------------------------------------------------------
# Text cleaning + sentiment/emotions (per-comment)
# ------------------------------------------------------------
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
  trimws(x)
}

annotate_sentiment_emotions <- function(text_vec) {
  out <- data.frame(text = as.character(text_vec), stringsAsFactors = FALSE)
  out$afinn <- tryCatch(syuzhet::get_sentiment(out$text, method = "afinn"), error = function(e) NA_real_)
  out$sentiment <- ifelse(out$afinn > 0, "Positive", ifelse(out$afinn < 0, "Negative", "Neutral"))
  emo <- tryCatch(syuzhet::get_nrc_sentiment(out$text)[ , 1:8], error = function(e) NULL)
  if (!is.null(emo)) out <- cbind(out, emo)
  out
}

# ------------------------------------------------------------
# LDA topic assignment (per-document top topic + label)
# ------------------------------------------------------------
build_corpus <- function(clean_vec) {
  tm::VCorpus(tm::VectorSource(clean_vec)) |>
    tm::tm_map(tm::content_transformer(tolower)) |>
    tm::tm_map(tm::removeWords, tm::stopwords(kind = "SMART")) |>
    tm::tm_map(tm::stripWhitespace)
}

build_dtm <- function(corpus) {
  dtm <- tm::DocumentTermMatrix(corpus)
  nz <- unique(dtm$i)
  if (length(nz) > 0) dtm <- dtm[nz, ]
  dtm
}

assign_topics <- function(text_vec, k = 6) {
  if (length(text_vec) == 0) return(data.frame())
  corp <- build_corpus(text_vec)
  dtm <- build_dtm(corp)
  if (nrow(dtm) == 0 || ncol(dtm) == 0) return(data.frame())
  lda_m <- topicmodels::LDA(dtm, k = k, control = list(seed = 1337))
  gamma <- tidytext::tidy(lda_m, matrix = "gamma")
  beta <- tidytext::tidy(lda_m, matrix = "beta")
  top_terms <- beta |>
    dplyr::group_by(topic) |>
    dplyr::slice_max(beta, n = 5) |>
    dplyr::summarise(label = paste(term, collapse = ", "), .groups = "drop")
  top_topic <- gamma |>
    dplyr::group_by(document) |>
    dplyr::slice_max(gamma, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::left_join(top_terms, by = "topic") |>
    dplyr::rename(top_topic = topic, top_topic_prob = gamma, top_topic_label = label)
  top_topic$document <- as.integer(top_topic$document)
  top_topic
}

# ------------------------------------------------------------
# Reddit: per-comment annotations (sentiment, emotions, topics)
# ------------------------------------------------------------
rd <- safe_load_rds(paste(dataset_dir, "rd_data.rds", sep = ""))
if (!is.null(rd) && nrow(rd) > 0) {
  rd <- rd[complete.cases(rd), ]
  txt <- if ("comment" %in% names(rd)) rd$comment else rd[[1]]
  clean <- clean_text_vector(txt)
  sent <- annotate_sentiment_emotions(clean)
  # timestamps
  if ("comm_date" %in% names(rd)) {
    if (is.numeric(rd$comm_date)) {
      rd$timestamp_parsed <- as.POSIXct(rd$comm_date, origin = "1970-01-01", tz = "UTC")
    } else {
      rd$timestamp_parsed <- suppressWarnings(lubridate::ymd_hms(rd$comm_date, quiet = TRUE))
    }
  }
  # engagement proxy
  if ("comment_score" %in% names(rd)) rd$engagement <- rd$comment_score
  # topics
  gamma_rd <- assign_topics(clean, k = 6)
  rd_out <- cbind(rd, sent)
  if (nrow(gamma_rd) > 0) {
    # align by row index (document == row position in dtm)
    rd_out$doc_id <- seq_len(nrow(rd_out))
    rd_out <- dplyr::left_join(rd_out, gamma_rd, by = c("doc_id" = "document"))
    rd_out$doc_id <- NULL
  }
  utils::write.csv(rd_out, file = file.path(powerbi_dir, "reddit_comments_annotated.csv"), row.names = FALSE)
}

# ------------------------------------------------------------
# YouTube: per-comment annotations (sentiment, emotions, topics)
# ------------------------------------------------------------
yt <- safe_load_rds(paste(dataset_dir, "yt_data.rds", sep = ""))
if (!is.null(yt) && nrow(yt) > 0) {
  yt <- yt[complete.cases(yt), ]
  txt <- if ("Comment" %in% names(yt)) yt$Comment else yt[[1]]
  clean <- clean_text_vector(txt)
  sent <- annotate_sentiment_emotions(clean)
  # timestamps
  date_cols <- intersect(c("Published", "PublishedAt", "publishedAt"), names(yt))
  if (length(date_cols) > 0) yt$timestamp_parsed <- suppressWarnings(lubridate::ymd_hms(yt[[date_cols[1]]], quiet = TRUE))
  # engagement proxy
  likes <- if ("LikeCount" %in% names(yt)) yt$LikeCount else 0
  replies <- if ("ReplyCount" %in% names(yt)) yt$ReplyCount else 0
  yt$engagement <- suppressWarnings(as.numeric(likes) + as.numeric(replies))
  # topics
  gamma_yt <- assign_topics(clean, k = 6)
  yt_out <- cbind(yt, sent)
  if (nrow(gamma_yt) > 0) {
    yt_out$doc_id <- seq_len(nrow(yt_out))
    yt_out <- dplyr::left_join(yt_out, gamma_yt, by = c("doc_id" = "document"))
    yt_out$doc_id <- NULL
  }
  utils::write.csv(yt_out, file = file.path(powerbi_dir, "youtube_comments_annotated.csv"), row.names = FALSE)
}

# ------------------------------------------------------------
# Network metrics (centrality, communities)
# ------------------------------------------------------------
copy_if_exists <- function(src, dest_name) {
  if (file.exists(src)) file.copy(src, file.path(powerbi_dir, dest_name), overwrite = TRUE)
}
copy_if_exists(paste(dataset_dir, "reddit_centrality.csv", sep = ""), "reddit_centrality.csv")
copy_if_exists(paste(dataset_dir, "youtube_centrality.csv", sep = ""), "youtube_centrality.csv")
copy_if_exists(paste(dataset_dir, "reddit_louvain_membership.csv", sep = ""), "reddit_louvain_membership.csv")
copy_if_exists(paste(dataset_dir, "reddit_girvnewman_membership.csv", sep = ""), "reddit_girvnewman_membership.csv")
copy_if_exists(paste(dataset_dir, "youtube_louvain_membership.csv", sep = ""), "youtube_louvain_membership.csv")
copy_if_exists(paste(dataset_dir, "youtube_girvnewman_membership.csv", sep = ""), "youtube_girvnewman_membership.csv")

# ------------------------------------------------------------
# Spotify tracks with features and predictions (Q11)
# ------------------------------------------------------------
tracks <- safe_read_csv(paste(dataset_dir, "q11_tracks_features.csv", sep = ""))
pred_b <- safe_read_csv(file.path(dataset_dir, "train_data", "predictions_baseline.csv"))
pred_bo <- safe_read_csv(file.path(dataset_dir, "train_data", "predictions_boosted.csv"))
pred_glm <- safe_read_csv(file.path(dataset_dir, "train_data", "predictions_caret_glm.csv"))

if (!is.null(tracks)) {
  # Era categorization from album_year
  if ("album_year" %in% names(tracks)) {
    yr <- suppressWarnings(as.integer(tracks$album_year))
    tracks$era_decade <- ifelse(is.na(yr), NA_character_, paste0((yr %/% 10) * 10, "s"))
  }
  out <- tracks
  # join each prediction table by track_id (test set only rows will match)
  if (!is.null(pred_b))  out <- dplyr::left_join(out, dplyr::rename(pred_b[, c("track_id","predicted","prob_yes")], pred_baseline = predicted, prob_yes_baseline = prob_yes), by = "track_id")
  if (!is.null(pred_bo)) out <- dplyr::left_join(out, dplyr::rename(pred_bo[, c("track_id","predicted","prob_yes")], pred_boosted = predicted, prob_yes_boosted = prob_yes), by = "track_id")
  if (!is.null(pred_glm)) out <- dplyr::left_join(out, dplyr::rename(pred_glm[, c("track_id","predicted","prob_yes")], pred_glm = predicted, prob_yes_glm = prob_yes), by = "track_id")
  utils::write.csv(out, file = file.path(powerbi_dir, "spotify_tracks_with_predictions.csv"), row.names = FALSE)
}

# ------------------------------------------------------------
# Combined platform metrics (high level)
# ------------------------------------------------------------
sent_summary <- safe_read_csv(paste(dataset_dir, "sentiment_summary.csv", sep = ""))
emo_summary  <- safe_read_csv(paste(dataset_dir, "emotion_summary.csv", sep = ""))

combined_metrics <- NULL
if (!is.null(sent_summary)) {
  total_by_platform <- sent_summary |>
    dplyr::group_by(platform) |>
    dplyr::summarise(total = sum(count), .groups = "drop")
  pos_share <- sent_summary |>
    dplyr::filter(sentiment == "Positive") |>
    dplyr::select(platform, positive_share = share)
  neg_share <- sent_summary |>
    dplyr::filter(sentiment == "Negative") |>
    dplyr::select(platform, negative_share = share)
  combined_metrics <- total_by_platform |>
    dplyr::left_join(pos_share, by = "platform") |>
    dplyr::left_join(neg_share, by = "platform")
}
if (!is.null(combined_metrics) && !is.null(emo_summary)) {
  emo_wide <- tidyr::pivot_wider(emo_summary, names_from = emotion, values_from = proportion)
  combined_metrics <- dplyr::left_join(combined_metrics, emo_wide, by = "platform")
}
if (!is.null(combined_metrics)) {
  utils::write.csv(combined_metrics, file = file.path(powerbi_dir, "combined_platform_metrics.csv"), row.names = FALSE)
}

cat("[Export] Power BI files written to:", normalizePath(powerbi_dir, winslash = "/", mustWork = FALSE), "\n")


