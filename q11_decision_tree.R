# =========================================================================
# QUESTION 11: Decision tree to predict whether a song is by your artist/band
# Simple implementation using Spotify audio features and C5.0
# =========================================================================

# --------------------------------------------------------------------------
# Setup
# --------------------------------------------------------------------------

if (!exists("dataset_dir")) dataset_dir <- ".//data//"
if (!exists("graph_dir")) graph_dir <- ".//graphs//"
if (!dir.exists(dataset_dir)) dir.create(dataset_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(graph_dir)) dir.create(graph_dir, recursive = TRUE, showWarnings = FALSE)
if (!exists("ARTIST_NAME")) ARTIST_NAME <- "Taylor Swift"
if (!exists("SPOTIFY_ARTIST_ID")) SPOTIFY_ARTIST_ID <- ""

# Images directories (by question)
images_base_dir <- ".//images//"
q11_img_dir <- file.path(images_base_dir, "q11")
if (!dir.exists(images_base_dir)) dir.create(images_base_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(q11_img_dir)) dir.create(q11_img_dir, recursive = TRUE, showWarnings = FALSE)

suppressWarnings({
  library(spotifyr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(C50)
})

set.seed(1337)

# --------------------------------------------------------------------------
# Authenticate Spotify 
# --------------------------------------------------------------------------

# Refer to q5: source credentials from R variables into environment if available
if (exists("SPOTIFY_CLIENT_ID") && exists("SPOTIFY_CLIENT_SECRET")) {
  Sys.setenv(SPOTIFY_CLIENT_ID = SPOTIFY_CLIENT_ID)
  Sys.setenv(SPOTIFY_CLIENT_SECRET = SPOTIFY_CLIENT_SECRET)
}

token_ok <- FALSE
try({
  access_token <- get_spotify_access_token()
  token_ok <- TRUE
}, silent = TRUE)
if (!token_ok) {
  stop("Spotify authentication failed.")
}

# --------------------------------------------------------------------------
# Helper functions
# --------------------------------------------------------------------------

get_or_find_artist_id <- function(name_hint, id_hint) {
  if (!is.null(id_hint) && nzchar(id_hint)) return(id_hint)
  res <- tryCatch(search_spotify(name_hint, type = "artist", limit = 1), error = function(e) NULL)
  if (is.null(res) || nrow(res) == 0) stop("Could not find artist id for ", name_hint)
  res$id[1]
}

numeric_feature_cols <- function(df) {
  # Minimal, realistic, low-NA numeric features
  wanted <- c(
    "duration_ms","track_number","disc_number","album_year",
    "title_length","title_word_count","disc_track_ratio"
  )
  intersect(wanted, names(df))
}

clean_feature_frame <- function(df, label_value) {
  if (is.null(df) || nrow(df) == 0) return(data.frame())
  # Ensure explicit is factor and album_year is numeric
  if ("explicit" %in% names(df)) {
    df$explicit <- ifelse(isTRUE(df$explicit), "yes", "no")
    df$explicit <- factor(df$explicit, levels = c("no","yes"))
  }
  # Fill some missing numeric metadata to avoid over-dropping rows
  if (!("disc_number" %in% names(df))) df$disc_number <- NA_integer_
  if (!("track_number" %in% names(df))) df$track_number <- NA_integer_
  if (!("duration_ms" %in% names(df))) df$duration_ms <- NA_integer_
  if (!("album_year" %in% names(df))) df$album_year <- NA_integer_

  df$disc_number[is.na(df$disc_number)] <- 1L
  df$track_number[is.na(df$track_number)] <- 1L
  # Keep duration_ms and album_year as required
  cols <- unique(c("track_id","track_name","artist_name","artist_id","explicit", numeric_feature_cols(df)))
  df2 <- df[, cols[cols %in% names(df)], drop = FALSE]
  df2 <- df2[complete.cases(df2[, numeric_feature_cols(df2), drop = FALSE]), , drop = FALSE]
  df2$by_artist <- factor(ifelse(label_value, "yes", "no"), levels = c("no","yes"))
  df2
}

## (Reverted) Removed enrichment helpers to keep minimal feature set

# Add realistic, low-NA derived features from existing metadata
add_realistic_features <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  # Title-based features
  title <- as.character(df$track_name)
  title[is.na(title)] <- ""
  title_norm <- trimws(gsub("\\s+", " ", title))
  df$title_length <- nchar(title_norm)
  df$title_word_count <- if (length(title_norm) > 0) sapply(strsplit(title_norm, " "), length) else integer(0)
  ttl <- tolower(title_norm)
  df$has_feat <- factor(ifelse(grepl("(^|[\\s\\(\\[])(feat\\.?|featuring)([\\s\\)\\]:]|$)", ttl), "yes", "no"), levels = c("no","yes"))
  df$has_remix <- factor(ifelse(grepl("remix", ttl), "yes", "no"), levels = c("no","yes"))
  df$has_live <- factor(ifelse(grepl("\\blive\\b", ttl), "yes", "no"), levels = c("no","yes"))
  df$has_acoustic <- factor(ifelse(grepl("acoustic", ttl), "yes", "no"), levels = c("no","yes"))
  # Decade from album_year
  if ("album_year" %in% names(df)) {
    yr <- suppressWarnings(as.integer(df$album_year))
    decade_start <- ifelse(is.na(yr), NA_integer_, (yr %/% 10) * 10)
    df$decade <- factor(ifelse(is.na(decade_start), NA_character_, paste0(decade_start, "s")))
  }
  # Ratio feature
  dn <- suppressWarnings(as.integer(df$disc_number)); dn[is.na(dn) | dn <= 0] <- 1L
  tn <- suppressWarnings(as.integer(df$track_number)); tn[is.na(tn) | tn <= 0] <- 1L
  df$disc_track_ratio <- as.numeric(tn) / pmax(as.numeric(dn), 1)
  df
}

# Process top-tracks data.frame into feature frame
process_top_tracks <- function(tracks_df, artist_id) {
  if (is.null(tracks_df) || nrow(tracks_df) == 0) return(data.frame())
  keep <- intersect(c("id","name","duration_ms","explicit","popularity","track_number","disc_number","album.release_date"), names(tracks_df))
  df <- tracks_df[, keep, drop = FALSE]
  names(df)[names(df) == "id"] <- "track_id"
  names(df)[names(df) == "name"] <- "track_name"
  # Derive album_year from album.release_date
  if ("album.release_date" %in% names(df)) {
    df$album_year <- suppressWarnings(as.integer(substr(as.character(df$"album.release_date"), 1, 4)))
  }
  df$artist_id <- artist_id
  df
}

safe_get_artist_top_tracks_metadata <- function(artist_id) {
  tryCatch({
    tt <- spotifyr::get_artist_top_tracks(artist_id, market = "US")
    process_top_tracks(tt, artist_id)
  }, error = function(e) data.frame())
}

# Search tracks for a specific artist by name, then keep only rows that include the artist_id
safe_search_tracks_for_artist <- function(artist_id, artist_name, max_tracks = 3000) {
  out <- data.frame()
  tryCatch({
    # query by artist name; we will filter by artist_id to ensure correctness
    q <- paste0('artist:"', artist_name, '"')
    for (off in seq(0, 5000, by = 50)) {
      tr <- tryCatch(spotifyr::search_spotify(q, type = "track", market = "US", limit = 50, offset = off), error = function(e) data.frame())
      if (is.null(tr) || nrow(tr) == 0) break
      # keep only tracks where artist_id appears in the artists list
      keep_rows <- rep(FALSE, nrow(tr))
      if ("artists" %in% names(tr)) {
        for (i in seq_len(nrow(tr))) {
          arts <- tryCatch(tr$artists[[i]], error = function(e) NULL)
          if (is.data.frame(arts) && "id" %in% names(arts)) {
            if (artist_id %in% as.character(arts$id)) keep_rows[i] <- TRUE
          }
        }
      }
      tr <- tr[keep_rows, , drop = FALSE]
      if (nrow(tr) == 0) next
      keep <- intersect(c("id","name","duration_ms","explicit","track_number","disc_number","album.release_date"), names(tr))
      df <- tr[, keep, drop = FALSE]
      names(df)[names(df) == "id"] <- "track_id"
      names(df)[names(df) == "name"] <- "track_name"
      if ("album.release_date" %in% names(df)) {
        df$album_year <- suppressWarnings(as.integer(substr(as.character(df$"album.release_date"), 1, 4)))
      }
      df$artist_id <- artist_id
      out <- dplyr::bind_rows(out, df)
      if (nrow(out) >= max_tracks) break
      Sys.sleep(0.15)
    }
    if (nrow(out) > 0) out <- out[!duplicated(out$track_id), , drop = FALSE]
    out
  }, error = function(e) data.frame())
}

# Extract collaborator/peer artist IDs from a top-tracks data.frame
extract_artist_ids_from_tracks_df <- function(tracks_df, exclude_artist_id = NULL) {
  ids <- character(0)
  if (is.null(tracks_df) || nrow(tracks_df) == 0) return(ids)
  if (!("artists" %in% names(tracks_df))) return(ids)
  for (i in seq_len(nrow(tracks_df))) {
    arts <- tryCatch(tracks_df$artists[[i]], error = function(e) NULL)
    if (is.null(arts)) next
    if (is.data.frame(arts) && "id" %in% names(arts)) {
      ids <- c(ids, as.character(arts$id))
    }
  }
  ids <- unique(stats::na.omit(ids))
  if (!is.null(exclude_artist_id)) ids <- ids[ids != exclude_artist_id]
  ids
}

# Get album tracks metadata (no audio features), US market
safe_get_artist_album_tracks_metadata <- function(artist_id) {
  out <- data.frame()
  tryCatch({
    include_groups <- c("album","single","compilation")
    all_albums <- data.frame()
    for (off in c(0, 50, 100, 150)) {
      al <- tryCatch(spotifyr::get_artist_albums(artist_id, include_groups = include_groups, market = "US", limit = 50, offset = off), error = function(e) data.frame())
      if (nrow(al) == 0) break
      all_albums <- dplyr::bind_rows(all_albums, al)
      if (nrow(al) < 50) break
      Sys.sleep(0.2)
    }
    if (nrow(all_albums) == 0) return(out)
    album_ids <- unique(all_albums$id)
    # Build album -> release_date map for year derivation
    album_year_map <- data.frame(
      album_id = as.character(all_albums$id),
      album_release_date = as.character(all_albums$release_date),
      stringsAsFactors = FALSE
    )
    acc <- data.frame()
    for (aid in album_ids) {
      tr <- tryCatch(spotifyr::get_album_tracks(aid, market = "US"), error = function(e) data.frame())
      if (nrow(tr) == 0) next
      keep <- intersect(c("id","name","duration_ms","explicit","track_number","disc_number"), names(tr))
      df <- tr[, keep, drop = FALSE]
      names(df)[names(df) == "id"] <- "track_id"
      names(df)[names(df) == "name"] <- "track_name"
      # attach album id and album year
      df$album_id <- aid
      rel <- album_year_map$album_release_date[match(aid, album_year_map$album_id)]
      df$album_year <- suppressWarnings(as.integer(substr(as.character(rel), 1, 4)))
      df$artist_id <- artist_id
      acc <- dplyr::bind_rows(acc, df)
      Sys.sleep(0.1)
    }
    acc
  }, error = function(e) data.frame())
}

# Search-wide tracks metadata (negatives) excluding the main artist
safe_search_tracks_metadata <- function(exclude_artist_id, year_range = c(2006, 2024), max_tracks = 2000) {
  out <- data.frame()
  tryCatch({
    start_year <- min(year_range, na.rm = TRUE)
    end_year <- max(year_range, na.rm = TRUE)
    q <- paste0("year:", start_year, "-", end_year)
    collected <- 0
    for (off in seq(0, 5000, by = 50)) {  # up to 5000 tracks attempted
      tr <- tryCatch(spotifyr::search_spotify(q, type = "track", market = "US", limit = 50, offset = off), error = function(e) data.frame())
      if (is.null(tr) || nrow(tr) == 0) break
      # Filter out any rows where artist list contains exclude_artist_id
      keep_rows <- rep(TRUE, nrow(tr))
      if ("artists" %in% names(tr)) {
        for (i in seq_len(nrow(tr))) {
          arts <- tryCatch(tr$artists[[i]], error = function(e) NULL)
          if (is.data.frame(arts) && "id" %in% names(arts)) {
            if (exclude_artist_id %in% as.character(arts$id)) keep_rows[i] <- FALSE
          }
        }
      }
      tr <- tr[keep_rows, , drop = FALSE]
      if (nrow(tr) == 0) next
      keep <- intersect(c("id","name","duration_ms","explicit","track_number","disc_number","album.release_date"), names(tr))
      df <- tr[, keep, drop = FALSE]
      names(df)[names(df) == "id"] <- "track_id"
      names(df)[names(df) == "name"] <- "track_name"
      if ("album.release_date" %in% names(df)) {
        df$album_year <- suppressWarnings(as.integer(substr(as.character(df$"album.release_date"), 1, 4)))
      }
      df$artist_id <- NA_character_
      out <- dplyr::bind_rows(out, df)
      collected <- nrow(out)
      if (collected >= max_tracks) break
      Sys.sleep(0.15)
    }
    # Deduplicate by track_id
    if (nrow(out) > 0) out <- out[!duplicated(out$track_id), , drop = FALSE]
    out
  }, error = function(e) data.frame())
}

# --------------------------------------------------------------------------
# Build dataset: positive = your artist; negative = general tracks (not your artist)
# --------------------------------------------------------------------------

main_artist_id <- get_or_find_artist_id(ARTIST_NAME, SPOTIFY_ARTIST_ID)

# Config for dataset size
NUM_RELATED <- 20            # number of related/collab artists to include
MAX_POS <- Inf               # no cap on positive tracks
MAX_NEG_PER_ARTIST <- 200    # higher cap per negative artist

# Positive class: prefer album tracks to increase volume; fallback to top tracks and search
pos_album <- safe_get_artist_album_tracks_metadata(main_artist_id)
pos_top <- safe_get_artist_top_tracks_metadata(main_artist_id)
pos_search <- safe_search_tracks_for_artist(main_artist_id, ARTIST_NAME, max_tracks = 3000)
pos_meta <- suppressWarnings(dplyr::bind_rows(pos_album, pos_top, pos_search))
pos_meta <- pos_meta[!duplicated(pos_meta$track_id), , drop = FALSE]
pos_df <- clean_feature_frame(pos_meta, label_value = TRUE)
pos_df <- add_realistic_features(pos_df)
if (is.finite(MAX_POS) && nrow(pos_df) > MAX_POS) {
  pos_df <- pos_df[sample(seq_len(nrow(pos_df)), MAX_POS), , drop = FALSE]
}
cat("\n[Q11] Collected POS tracks:", nrow(pos_df), " (album:", nrow(pos_album), ", top:", nrow(pos_top), ", search:", nrow(pos_search), ")\n")

# Negative class: general tracks from Spotify search excluding the main artist
neg_meta <- safe_search_tracks_metadata(exclude_artist_id = main_artist_id, year_range = c(2006, 2024), max_tracks = 3000)
neg_df <- clean_feature_frame(neg_meta, label_value = FALSE)
neg_df <- add_realistic_features(neg_df)
cat("[Q11] Collected NEG tracks:", nrow(neg_df), " (general search, artist excluded)\n")

# Balance classes (downsample to the smaller class)
pos_df <- pos_df[!duplicated(pos_df$track_id), , drop = FALSE]
neg_df <- neg_df[!duplicated(neg_df$track_id), , drop = FALSE]

# Ensure core feature columns exist
core_cols <- numeric_feature_cols(rbind(pos_df[0, , drop = FALSE], neg_df[0, , drop = FALSE]))
pos_df <- pos_df[, unique(c("track_id","track_name","artist_name","artist_id", core_cols, "by_artist"))[unique(c("track_id","track_name","artist_name","artist_id", core_cols, "by_artist")) %in% names(pos_df)], drop = FALSE]
neg_df <- neg_df[, unique(c("track_id","track_name","artist_name","artist_id", core_cols, "by_artist"))[unique(c("track_id","track_name","artist_name","artist_id", core_cols, "by_artist")) %in% names(neg_df)], drop = FALSE]

n_min <- min(nrow(pos_df), nrow(neg_df))
cat("[Q11] Pre-balance sizes -> POS:", nrow(pos_df), " NEG:", nrow(neg_df), " Balance target per class:", n_min, "\n")
if (n_min < 10) stop("Insufficient data to train model (need >= 10 per class). Collected: ", nrow(pos_df), "/", nrow(neg_df))
pos_df <- pos_df[sample(seq_len(nrow(pos_df)), n_min), , drop = FALSE]
neg_df <- neg_df[sample(seq_len(nrow(neg_df)), n_min), , drop = FALSE]
cat("[Q11] Post-balance sizes -> POS:", nrow(pos_df), " NEG:", nrow(neg_df), "\n")

all_df <- suppressWarnings(dplyr::bind_rows(pos_df, neg_df))
cat("[Q11] Combined dataset size:", nrow(all_df), "\n")

# Save raw features for transparency
utils::write.csv(all_df, paste(dataset_dir, "q11_tracks_features.csv", sep = ""), row.names = FALSE)

# Quick dataset summary (printed)
cat("\n========== Q11 DATASET SUMMARY =========\n")
cat("Artist:", ARTIST_NAME, "\n")
cat("Positive (by artist):", nrow(pos_df), " | Negative (general):", nrow(neg_df), " | Total:", nrow(all_df), "\n")
cat("Features used:", paste(numeric_feature_cols(all_df), collapse = ", "), "\n")
cat("Class distribution (overall):\n"); print(table(all_df$by_artist))
cat("Sample rows (first 8):\n")
print(utils::head(all_df[, c("track_name", "by_artist", numeric_feature_cols(all_df)), drop = FALSE], 8))

# --------------------------------------------------------------------------
# Train/test split and feature selection
# --------------------------------------------------------------------------

feature_cols <- numeric_feature_cols(all_df)
categorical_cols <- intersect(c("explicit","has_feat","has_remix","has_live","has_acoustic","decade"), names(all_df))
model_df <- all_df[, c(feature_cols, categorical_cols, "by_artist"), drop = FALSE]

# Simple split 80/20 with class distribution logs and CSV exports
n <- nrow(model_df)
idx <- sample(seq_len(n), size = floor(0.8 * n))
train_df <- model_df[idx, , drop = FALSE]
test_df  <- model_df[-idx, , drop = FALSE]
cat("\n[Q11] Train/Test split -> Train:", nrow(train_df), " Test:", nrow(test_df), "\n")
cat("[Q11] Train class distribution:\n"); print(table(train_df$by_artist))
cat("[Q11] Test class distribution:\n"); print(table(test_df$by_artist))

# Save split datasets
train_dir <- file.path(dataset_dir, "train_data")
if (!dir.exists(train_dir)) dir.create(train_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(train_df, file = file.path(train_dir, "train_df.csv"), row.names = FALSE)
utils::write.csv(test_df,  file = file.path(train_dir, "test_df.csv"),  row.names = FALSE)

# --------------------------------------------------------------------------
# Baseline C5.0 tree
# --------------------------------------------------------------------------

model_baseline <- C50::C5.0(by_artist ~ ., data = train_df)
pred_base <- predict(model_baseline, newdata = test_df)

cm_base <- table(Predicted = pred_base, Actual = test_df$by_artist)
acc_base <- mean(pred_base == test_df$by_artist)

prec_base <- tryCatch(
  cm_base["yes","yes"] / sum(cm_base["yes", ]),
  error = function(e) NA_real_
)
rec_base <- tryCatch(
  cm_base["yes","yes"] / sum(cm_base[ ,"yes"]),
  error = function(e) NA_real_
)
f1_base <- if (is.na(prec_base) || is.na(rec_base) || (prec_base + rec_base) == 0) NA_real_ else 2 * (prec_base * rec_base) / (prec_base + rec_base)

# --------------------------------------------------------------------------
# Improved model: boosted C5.0 (trials = 10)
# --------------------------------------------------------------------------

model_boost <- C50::C5.0(by_artist ~ ., data = train_df, trials = 10)
pred_boost <- predict(model_boost, newdata = test_df)

cm_boost <- table(Predicted = pred_boost, Actual = test_df$by_artist)
acc_boost <- mean(pred_boost == test_df$by_artist)
prec_boost <- tryCatch(
  cm_boost["yes","yes"] / sum(cm_boost["yes", ]),
  error = function(e) NA_real_
)
rec_boost <- tryCatch(
  cm_boost["yes","yes"] / sum(cm_boost[ ,"yes"]),
  error = function(e) NA_real_
)
f1_boost <- if (is.na(prec_boost) || is.na(rec_boost) || (prec_boost + rec_boost) == 0) NA_real_ else 2 * (prec_boost * rec_boost) / (prec_boost + rec_boost)

# --------------------------------------------------------------------------
# Enhanced model via caret (e.g., logistic regression with CV)
# --------------------------------------------------------------------------

has_caret <- requireNamespace("caret", quietly = TRUE)
acc_caret <- prec_caret <- rec_caret <- f1_caret <- NA_real_
pred_caret <- NULL
prob_caret <- NULL
cm_caret <- NULL
if (has_caret) {
  # Use caret with ROC metric and repeated CV; choose a lightweight, reliable model
  library(caret)
  # Ensure positive class is 'yes'
  train_df$by_artist <- stats::relevel(train_df$by_artist, ref = "yes")
  test_df$by_artist  <- stats::relevel(test_df$by_artist,  ref = "yes")

  ctrl <- caret::trainControl(method = "repeatedcv",
                              number = 5, repeats = 2,
                              classProbs = TRUE,
                              summaryFunction = caret::twoClassSummary,
                              savePredictions = "final",
                              allowParallel = TRUE)

  # Logistic regression (glm) with centering/scaling for stability
  model_caret <- caret::train(by_artist ~ .,
                              data = train_df,
                              method = "glm",
                              family = binomial(),
                              metric = "ROC",
                              trControl = ctrl,
                              preProcess = c("center", "scale"))

  pred_caret <- predict(model_caret, newdata = test_df)
  pr_c <- tryCatch(predict(model_caret, newdata = test_df, type = "prob"), error = function(e) NULL)
  prob_caret <- if (!is.null(pr_c) && "yes" %in% colnames(pr_c)) pr_c[, "yes"] else rep(NA_real_, nrow(test_df))

  cm_caret <- table(Predicted = pred_caret, Actual = test_df$by_artist)
  acc_caret <- mean(pred_caret == test_df$by_artist)
  prec_caret <- tryCatch(cm_caret["yes","yes"] / sum(cm_caret["yes", ]), error = function(e) NA_real_)
  rec_caret  <- tryCatch(cm_caret["yes","yes"] / sum(cm_caret[ ,"yes"]), error = function(e) NA_real_)
  f1_caret   <- if (is.na(prec_caret) || is.na(rec_caret) || (prec_caret + rec_caret) == 0) NA_real_ else 2 * (prec_caret * rec_caret) / (prec_caret + rec_caret)
} else {
  cat("[Q11] caret not available; skipping caret-enhanced model. Install 'caret' to enable.\n")
}

# --------------------------------------------------------------------------
# Save evaluation artifacts
# --------------------------------------------------------------------------

# Confusion matrices (printed only; not saved to disk per request)
cm_base_df <- as.data.frame.matrix(cm_base); cm_base_df$model <- "baseline"
cm_boost_df <- as.data.frame.matrix(cm_boost); cm_boost_df$model <- "boosted"
cm_out <- dplyr::bind_rows(cm_base_df, cm_boost_df)

# Metrics summary (printed only; not saved to disk per request)
eval_summary <- data.frame(
  model = c("baseline","boosted", if (has_caret) "caret_glm" else NULL),
  accuracy = c(acc_base, acc_boost, if (has_caret) acc_caret else NULL),
  precision_yes = c(prec_base, prec_boost, if (has_caret) prec_caret else NULL),
  recall_yes = c(rec_base, rec_boost, if (has_caret) rec_caret else NULL),
  f1_yes = c(f1_base, f1_boost, if (has_caret) f1_caret else NULL),
  stringsAsFactors = FALSE
)

cat("\n========== Q11 DECISION TREE SUMMARY =========\n")
print(eval_summary)

# Print confusion matrices and key metrics to console
cat("\n[Q11] -- Baseline (single tree) confusion matrix --\n")
print(cm_base)
cat(sprintf("Accuracy: %.3f\n", acc_base))
if (!is.na(prec_base)) cat(sprintf("Precision (yes): %.3f\n", prec_base))
if (!is.na(rec_base))  cat(sprintf("Recall (yes): %.3f\n", rec_base))
if (!is.na(f1_base))   cat(sprintf("F1 (yes): %.3f\n", f1_base))

cat("\n[Q11] -- Boosted (trials=10) confusion matrix --\n")
print(cm_boost)
cat(sprintf("Accuracy: %.3f\n", acc_boost))
if (!is.na(prec_boost)) cat(sprintf("Precision (yes): %.3f\n", prec_boost))
if (!is.na(rec_boost))  cat(sprintf("Recall (yes): %.3f\n", rec_boost))
if (!is.na(f1_boost))   cat(sprintf("F1 (yes): %.3f\n", f1_boost))

if (has_caret) {
  cat("\n[Q11] -- Caret (glm with CV) confusion matrix --\n")
  print(cm_caret)
  cat(sprintf("Accuracy: %.3f\n", acc_caret))
  if (!is.na(prec_caret)) cat(sprintf("Precision (yes): %.3f\n", prec_caret))
  if (!is.na(rec_caret))  cat(sprintf("Recall (yes): %.3f\n", rec_caret))
  if (!is.na(f1_caret))   cat(sprintf("F1 (yes): %.3f\n", f1_caret))
  # Print top coefficients by absolute magnitude (feature influence proxy)
  co <- tryCatch(summary(model_caret$finalModel)$coefficients, error = function(e) NULL)
  if (!is.null(co)) {
    co_df <- data.frame(term = rownames(co), estimate = co[,1], stringsAsFactors = FALSE)
    co_df <- co_df[co_df$term != "(Intercept)", , drop = FALSE]
    co_df$abs_est <- abs(co_df$estimate)
    co_top <- utils::head(co_df[order(-co_df$abs_est), c("term","estimate")], 10)
    cat("[Q11] Caret (glm) top coefficients by |estimate|:\n"); print(co_top)
  }
}

# Build full prediction tables and save to train_data
test_indices <- setdiff(seq_len(nrow(model_df)), idx)
test_meta <- all_df[test_indices, , drop = FALSE]
prob_base <- tryCatch(predict(model_baseline, newdata = test_df, type = "prob"), error = function(e) NULL)
prob_boost <- tryCatch(predict(model_boost,   newdata = test_df, type = "prob"), error = function(e) NULL)

predictions_baseline <- data.frame(
  track_id = test_meta$track_id,
  track_name = test_meta$track_name,
  actual = test_df$by_artist,
  predicted = pred_base,
  prob_yes = if (!is.null(prob_base) && "yes" %in% colnames(prob_base)) prob_base[,"yes"] else NA_real_,
  stringsAsFactors = FALSE
)
predictions_boosted <- data.frame(
  track_id = test_meta$track_id,
  track_name = test_meta$track_name,
  actual = test_df$by_artist,
  predicted = pred_boost,
  prob_yes = if (!is.null(prob_boost) && "yes" %in% colnames(prob_boost)) prob_boost[,"yes"] else NA_real_,
  stringsAsFactors = FALSE
)
utils::write.csv(predictions_baseline, file = file.path(train_dir, "predictions_baseline.csv"), row.names = FALSE)
utils::write.csv(predictions_boosted,  file = file.path(train_dir, "predictions_boosted.csv"),  row.names = FALSE)
if (has_caret) {
  predictions_caret <- data.frame(
    track_id = test_meta$track_id,
    track_name = test_meta$track_name,
    actual = test_df$by_artist,
    predicted = pred_caret,
    prob_yes = prob_caret,
    stringsAsFactors = FALSE
  )
  utils::write.csv(predictions_caret,  file = file.path(train_dir, "predictions_caret_glm.csv"),  row.names = FALSE)
}

# Show a small table of predictions vs actuals (first 12)
cat("\n[Q11] Sample predictions (boosted) -- first 12:\n")
print(utils::head(predictions_boosted, 12))
if (has_caret) {
  cat("\n[Q11] Sample predictions (caret glm) -- first 12:\n")
  print(utils::head(predictions_caret, 12))
}

# --------------------------------------------------------------------------
# Visualizations: variable importance and 2D scatter of top features
# --------------------------------------------------------------------------

# Variable importance from boosted model
imp <- tryCatch(C50::C5imp(model_boost, metric = "usage"), error = function(e) NULL)
if (!is.null(imp) && nrow(imp) > 0) {
  imp$feature <- rownames(imp)
  imp <- imp[order(-imp$Overall), , drop = FALSE]
  top_imp <- utils::head(imp, 10)
  cat("\nTop features by usage (boosted):\n")
  print(top_imp)
  p_imp <- ggplot(top_imp, aes(x = reorder(feature, Overall), y = Overall)) +
    geom_col(fill = "#1f77b4") +
    coord_flip() +
    labs(title = paste0("Q11: Feature Importance (", ARTIST_NAME, ")"), x = "Feature", y = "Importance (usage)") +
    theme_minimal()
  ggsave(paste(graph_dir, "q11_feature_importance.png", sep = ""), p_imp, width = 8, height = 5)
  ggsave(file.path(q11_img_dir, "q11_feature_importance.png"), p_imp, width = 8, height = 5)
}

# 2D scatter: top two features if available
top2 <- if (!is.null(imp) && nrow(imp) >= 2) rownames(imp[order(-imp$Overall), , drop = FALSE])[1:2] else feature_cols[1:2]
if (length(top2) == 2) {
  plot_df <- test_df
  plot_df$by_artist <- test_df$by_artist
  p_scatter <- ggplot(plot_df, aes_string(x = top2[1], y = top2[2], color = "by_artist")) +
    geom_point(alpha = 0.7) +
    scale_color_manual(values = c("no" = "#999999", "yes" = "#d62728")) +
    labs(title = paste0("Q11: Test set scatter (", top2[1], " vs ", top2[2], ")"), color = "By Artist") +
    theme_minimal()
  ggsave(paste(graph_dir, "q11_feature_scatter.png", sep = ""), p_scatter, width = 7, height = 5)
  ggsave(file.path(q11_img_dir, "q11_feature_scatter.png"), p_scatter, width = 7, height = 5)
}

# Helper to compute ROC and AUC (no external deps)
compute_roc <- function(labels, probs) {
  y <- ifelse(as.character(labels) == "yes", 1L, 0L)
  ord <- order(probs, decreasing = TRUE, na.last = NA)
  y <- y[ord]
  p <- probs[ord]
  tp <- cumsum(y)
  fp <- cumsum(1L - y)
  P <- sum(y)
  N <- length(y) - P
  if (P == 0 || N == 0) return(list(df = data.frame(fpr = c(0,1), tpr = c(0,1)), auc = NA_real_))
  tpr <- tp / P
  fpr <- fp / N
  df <- data.frame(fpr = c(0, fpr, 1), tpr = c(0, tpr, 1))
  auc <- sum(diff(df$fpr) * (head(df$tpr, -1) + tail(df$tpr, -1)) / 2)
  list(df = df, auc = auc)
}

# Helper to compute PR curve and AUPRC
compute_pr <- function(labels, probs) {
  y <- ifelse(as.character(labels) == "yes", 1L, 0L)
  ord <- order(probs, decreasing = TRUE, na.last = NA)
  y <- y[ord]
  tp <- cumsum(y)
  fp <- cumsum(1L - y)
  P <- sum(y)
  if (P == 0) return(list(df = data.frame(recall = c(0,1), precision = c(1,1)), aupr = NA_real_))
  recall <- tp / P
  precision <- tp / pmax(tp + fp, 1)
  # prepend (0,1) point for nice plotting
  df <- data.frame(recall = c(0, recall), precision = c(1, precision))
  # approximate area under PR with trapezoid on recall
  aupr <- sum(diff(df$recall) * (head(df$precision, -1) + tail(df$precision, -1)) / 2)
  list(df = df, aupr = aupr)
}

# Probabilities for curves
prob_yes_base <- tryCatch(predict(model_baseline, newdata = test_df, type = "prob")[, "yes"], error = function(e) rep(NA_real_, nrow(test_df)))
prob_yes_boost <- tryCatch(predict(model_boost,   newdata = test_df, type = "prob")[, "yes"], error = function(e) rep(NA_real_, nrow(test_df)))
prob_yes_caret <- if (has_caret) prob_caret else rep(NA_real_, nrow(test_df))

roc_b <- compute_roc(test_df$by_artist, prob_yes_base)
roc_bo <- compute_roc(test_df$by_artist, prob_yes_boost)
pr_b <- compute_pr(test_df$by_artist, prob_yes_base)
pr_bo <- compute_pr(test_df$by_artist, prob_yes_boost)

# ROC plot
roc_c <- if (has_caret) compute_roc(test_df$by_artist, prob_yes_caret) else NULL
roc_df <- rbind(
  transform(roc_b$df, model = "Baseline"),
  transform(roc_bo$df, model = "Boosted"),
  if (has_caret) transform(roc_c$df, model = "Caret (glm)") else NULL
)
p_roc <- ggplot(roc_df, aes(x = fpr, y = tpr, color = model)) +
  geom_line(size = 1) + geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("Baseline" = "#1f77b4", "Boosted" = "#d62728", "Caret (glm)" = "#2ca02c")) +
  labs(title = sprintf("Q11 ROC (AUC: baseline=%.3f, boosted=%.3f%s)", roc_b$auc, roc_bo$auc, if (has_caret) paste0(", caret=", sprintf("%.3f", roc_c$auc)) else ""), x = "False Positive Rate", y = "True Positive Rate", color = "Model") +
  theme_minimal()
ggsave(paste(graph_dir, "q11_roc.png", sep = ""), p_roc, width = 7, height = 5)
ggsave(file.path(q11_img_dir, "q11_roc.png"), p_roc, width = 7, height = 5)

# PR plot
pr_c <- if (has_caret) compute_pr(test_df$by_artist, prob_yes_caret) else NULL
pr_df <- rbind(
  transform(pr_b$df, model = "Baseline"),
  transform(pr_bo$df, model = "Boosted"),
  if (has_caret) transform(pr_c$df, model = "Caret (glm)") else NULL
)
p_pr <- ggplot(pr_df, aes(x = recall, y = precision, color = model)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Baseline" = "#1f77b4", "Boosted" = "#d62728", "Caret (glm)" = "#2ca02c")) +
  labs(title = sprintf("Q11 Precision-Recall (AUPRC: baseline=%.3f, boosted=%.3f%s)", pr_b$aupr, pr_bo$aupr, if (has_caret) paste0(", caret=", sprintf("%.3f", pr_c$aupr)) else ""), x = "Recall", y = "Precision", color = "Model") +
  theme_minimal()
ggsave(paste(graph_dir, "q11_pr.png", sep = ""), p_pr, width = 7, height = 5)
ggsave(file.path(q11_img_dir, "q11_pr.png"), p_pr, width = 7, height = 5)

# Confusion matrix heatmaps
cm_to_df <- function(cm) {
  df <- as.data.frame(cm)
  names(df) <- c("Predicted", "Actual", "Freq")
  df
}
p_cm <- function(df, title) {
  ggplot(df, aes(x = Actual, y = Predicted, fill = Freq)) +
    geom_tile() + geom_text(aes(label = Freq), color = "white", fontface = "bold") +
    scale_fill_gradient(low = "#6baed6", high = "#08306b") +
    labs(title = title, x = "Actual", y = "Predicted") +
    theme_minimal()
}
ggsave(paste(graph_dir, "q11_cm_baseline.png", sep = ""), p_cm(cm_to_df(cm_base), "Q11 Confusion Matrix - Baseline"), width = 5, height = 4)
ggsave(paste(graph_dir, "q11_cm_boosted.png", sep = ""), p_cm(cm_to_df(cm_boost), "Q11 Confusion Matrix - Boosted"), width = 5, height = 4)
ggsave(file.path(q11_img_dir, "q11_cm_baseline.png"), p_cm(cm_to_df(cm_base), "Q11 Confusion Matrix - Baseline"), width = 5, height = 4)
ggsave(file.path(q11_img_dir, "q11_cm_boosted.png"), p_cm(cm_to_df(cm_boost), "Q11 Confusion Matrix - Boosted"), width = 5, height = 4)
if (has_caret) {
  ggsave(paste(graph_dir, "q11_cm_caret_glm.png", sep = ""), p_cm(cm_to_df(cm_caret), "Q11 Confusion Matrix - Caret (glm)"), width = 5, height = 4)
  ggsave(file.path(q11_img_dir, "q11_cm_caret_glm.png"), p_cm(cm_to_df(cm_caret), "Q11 Confusion Matrix - Caret (glm)"), width = 5, height = 4)
}

# Feature distributions for top 3 numeric features by importance or fallback
numeric_used <- intersect(feature_cols, names(test_df))
top_num <- if (!is.null(imp) && nrow(imp) > 0) {
  intersect(rownames(imp[order(-imp$Overall), , drop = FALSE]), numeric_used)[1:min(3, length(numeric_used))]
} else {
  head(numeric_used, 3)
}
if (length(top_num) > 0) {
  dist_long <- tidyr::pivot_longer(cbind(test_df[, c(top_num, "by_artist")]), cols = all_of(top_num), names_to = "feature", values_to = "value")
  p_dist <- ggplot(dist_long, aes(x = by_artist, y = value, fill = by_artist)) +
    geom_violin(trim = TRUE, alpha = 0.6) +
    geom_boxplot(width = 0.2, outlier.size = 0.5, alpha = 0.7) +
    facet_wrap(~ feature, scales = "free_y") +
    scale_fill_manual(values = c("no" = "#999999", "yes" = "#d62728")) +
    labs(title = "Q11: Feature distributions by class (test set)", x = "Class", y = "Value", fill = "Class") +
    theme_minimal()
  ggsave(paste(graph_dir, "q11_feature_distributions.png", sep = ""), p_dist, width = 9, height = 5)
  ggsave(file.path(q11_img_dir, "q11_feature_distributions.png"), p_dist, width = 9, height = 5)
}
# --------------------------------------------------------------------------
# Notes for report
# --------------------------------------------------------------------------

cat("\nNotes:\n")
cat("- Dataset: equal samples from target artist and general tracks (balanced).\n")
cat("- Models: baseline C5.0, boosted C5.0 (trials=10)")
if (has_caret) cat(", and caret glm with CV")
cat(".\n")
cat("- Saved data to data/: q11_tracks_features.csv; splits in data/train_data/train_df.csv & test_df.csv.\n")
cat("- Saved predictions to data/train_data/: predictions_baseline.csv, predictions_boosted.csv")
if (has_caret) cat(", predictions_caret_glm.csv")
cat(".\n")
cat("- Saved plots to graphs/: q11_feature_importance.png, q11_feature_scatter.png, q11_roc.png, q11_pr.png, q11_cm_*.png.\n")

# Print a concise summary report to terminal (no file save)
report_lines <- c(
  "Q11 Decision Tree Report",
  paste0("Artist: ", ARTIST_NAME),
  paste0("Pos samples (post-balance): ", nrow(pos_df), ", Neg samples: ", nrow(neg_df), ", Total: ", nrow(all_df)),
  paste0("Features used: ", paste(numeric_feature_cols(all_df), collapse = ", ")),
  "",
  "Baseline (single tree):",
  paste0("  Accuracy=", sprintf("%.3f", acc_base), ", Precision_y=", sprintf("%.3f", prec_base), ", Recall_y=", sprintf("%.3f", rec_base), ", F1_y=", sprintf("%.3f", f1_base)),
  "",
  "Boosted (trials=10):",
  paste0("  Accuracy=", sprintf("%.3f", acc_boost), ", Precision_y=", sprintf("%.3f", prec_boost), ", Recall_y=", sprintf("%.3f", rec_boost), ", F1_y=", sprintf("%.3f", f1_boost)),
  "",
  paste0("Predictions saved: ", file.path(train_dir, "predictions_baseline.csv"), "; ", file.path(train_dir, "predictions_boosted.csv"),
         if (has_caret) paste0("; ", file.path(train_dir, "predictions_caret_glm.csv")) else "")
)
cat(paste0(report_lines, collapse = "\n"), "\n")