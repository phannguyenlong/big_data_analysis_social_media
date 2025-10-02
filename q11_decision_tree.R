# =========================================================================
# QUESTION 11: Predict Reddit comment engagement (high vs low)
# Models: baseline C5.0 (base features), boosted C5.0 (base), boosted C5.0 (enhanced), caret GLM (base)
# =========================================================================

# --------------------------------------------------------------------------
# Setup
# --------------------------------------------------------------------------

if (!exists("dataset_dir")) dataset_dir <- ".//data//"
if (!exists("graph_dir")) graph_dir <- ".//graphs//"
if (!dir.exists(dataset_dir)) dir.create(dataset_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(graph_dir)) dir.create(graph_dir, recursive = TRUE, showWarnings = FALSE)

images_base_dir <- ".//images//"
q11_img_dir <- file.path(images_base_dir, "q11")
if (!dir.exists(images_base_dir)) dir.create(images_base_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(q11_img_dir)) dir.create(q11_img_dir, recursive = TRUE, showWarnings = FALSE)

suppressWarnings({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(C50)
  library(lubridate)
  library(syuzhet)
})

set.seed(1337)

cat("\n[Q11] Starting Reddit engagement modeling...\n")
cat("[Q11] Output directories:\n  graphs  -> ", normalizePath(graph_dir, winslash = "/", mustWork = FALSE),
    "\n  images  -> ", normalizePath(q11_img_dir, winslash = "/", mustWork = FALSE),
    "\n  data    -> ", normalizePath(dataset_dir, winslash = "/", mustWork = FALSE), "\n", sep = "")

# --------------------------------------------------------------------------
# Load Reddit data
# --------------------------------------------------------------------------

rd_rds <- paste(dataset_dir, "rd_data.rds", sep = "")
if (!file.exists(rd_rds)) stop("Reddit dataset not found at ", rd_rds)
rd_data <- readRDS(rd_rds)
cat("[Q11] Loaded rd_data with rows:", nrow(rd_data), "\n")

# Keep essential columns and remove missing
if (!all(c("comment","comment_score") %in% names(rd_data))) {
  stop("rd_data must contain 'comment' and 'comment_score' columns")
}
rd_data <- rd_data %>%
  dplyr::filter(!is.na(comment), !is.na(comment_score))
cat("[Q11] After NA filtering, rows:", nrow(rd_data), "\n")

# --------------------------------------------------------------------------
# Target and features
# --------------------------------------------------------------------------

# Target: high if comment_score > median, else low
median_score <- stats::median(rd_data$comment_score, na.rm = TRUE)
rd_data$engagement <- ifelse(rd_data$comment_score > median_score, "high", "low")
rd_data$engagement <- factor(rd_data$engagement, levels = c("low","high"))
cat("[Q11] Median comment_score:", median_score, "\n")

comment_text <- as.character(rd_data$comment)

# Base (lab-style) features
base_df <- rd_data %>%
  dplyr::mutate(
    comment_preview = substr(comment_text, 1, 120),
    comment_length = nchar(comment_text),
    has_link = factor(ifelse(grepl("http", comment_text), "yes", "no"), levels = c("no","yes")),
    time_hour = {
      if ("comm_date" %in% names(rd_data)) {
        if (is.numeric(rd_data$comm_date)) {
          lubridate::hour(as.POSIXct(rd_data$comm_date, origin = "1970-01-01", tz = "UTC"))
        } else {
          lubridate::hour(suppressWarnings(lubridate::ymd_hms(rd_data$comm_date, quiet = TRUE)))
        }
      } else 0L
    },
    sentiment_score = syuzhet::get_sentiment(comment_text, method = "afinn"),
    sentiment = dplyr::case_when(
      sentiment_score >  0.1 ~ "positive",
      sentiment_score < -0.1 ~ "negative",
      TRUE ~ "neutral"
    )
  ) %>%
  dplyr::mutate(sentiment = factor(sentiment, levels = c("negative","neutral","positive"))) %>%
  dplyr::select(comment_preview, comment_length, has_link, time_hour, sentiment, engagement)

# Enhanced features (simple metadata beyond lab)
# - exclamation_count, question_count
# - uppercase_ratio, word_count, unique_word_ratio
# - link_count, mention_count (@), subreddit_ref_count ("r/")
# - sentiment_abs (magnitude)
text_upper_ratio <- function(x) {
  n <- nchar(x)
  ifelse(n > 0, sapply(gregexpr("[A-Z]", x), function(m) max(sum(m > 0), 0)) / n, 0)
}
word_count_fun <- function(x) sapply(strsplit(x, "\\s+"), function(v) sum(nzchar(v)))
unique_word_ratio_fun <- function(x) sapply(strsplit(x, "\\s+"), function(v) {
  v <- v[nzchar(v)]; if (length(v) == 0) return(0); length(unique(tolower(v))) / length(v)
})
count_pattern <- function(x, pattern) sapply(gregexpr(pattern, x, perl = TRUE), function(m) sum(m > 0))

sent_afinn <- syuzhet::get_sentiment(comment_text, method = "afinn")

enh_df <- rd_data %>%
  dplyr::mutate(
    comment_preview = substr(comment_text, 1, 120),
    comment_length = nchar(comment_text),
    word_count = word_count_fun(comment_text),
    unique_word_ratio = unique_word_ratio_fun(comment_text),
    exclamation_count = count_pattern(comment_text, "!"),
    question_count = count_pattern(comment_text, "\\?"),
    uppercase_ratio = text_upper_ratio(comment_text),
    link_count = count_pattern(comment_text, "http[s]?://"),
    mention_count = count_pattern(comment_text, "@\\w+"),
    subreddit_ref_count = count_pattern(comment_text, "\\br/\\w+"),
    has_link = factor(ifelse(link_count > 0, "yes", "no"), levels = c("no","yes")),
    time_hour = {
      if ("comm_date" %in% names(rd_data)) {
        if (is.numeric(rd_data$comm_date)) {
          lubridate::hour(as.POSIXct(rd_data$comm_date, origin = "1970-01-01", tz = "UTC"))
        } else {
          lubridate::hour(suppressWarnings(lubridate::ymd_hms(rd_data$comm_date, quiet = TRUE)))
        }
      } else 0L
    },
    sentiment_score = sent_afinn,
    sentiment_abs = abs(sent_afinn)
  ) %>%
  dplyr::select(comment_preview, engagement,
                comment_length, word_count, unique_word_ratio,
                exclamation_count, question_count, uppercase_ratio,
                link_count, mention_count, subreddit_ref_count,
                has_link, time_hour, sentiment_score, sentiment_abs)

# --------------------------------------------------------------------------
# Train/Test split and save to CSV
# --------------------------------------------------------------------------

base_df <- base_df[stats::complete.cases(base_df), , drop = FALSE]
enh_df <- enh_df[stats::complete.cases(enh_df), , drop = FALSE]

n <- nrow(base_df)
if (n < 20) stop("Not enough rows in Reddit data to train models.")
idx <- sample(seq_len(n), size = floor(0.8 * n))

train_base <- base_df[idx, , drop = FALSE]
test_base  <- base_df[-idx, , drop = FALSE]

train_enh <- enh_df[idx, , drop = FALSE]
test_enh  <- enh_df[-idx, , drop = FALSE]

# Drop character columns from enhanced frames to avoid encoding issues during modeling
train_enh_x <- train_enh[, setdiff(names(train_enh), c("comment_preview")), drop = FALSE]
test_enh_x  <- test_enh[,  setdiff(names(test_enh),  c("comment_preview")),  drop = FALSE]

base_features <- c("comment_length","has_link","time_hour","sentiment")
enhanced_features <- setdiff(names(train_enh_x), c("engagement"))
cat("[Q11] Base features used:      ", paste(base_features, collapse = ", "), "\n", sep = "")
cat("[Q11] Enhanced features used:  ", paste(enhanced_features, collapse = ", "), "\n", sep = "")
cat("[Q11] Difference (enhanced adds): ", paste(setdiff(enhanced_features, base_features), collapse = ", "), "\n", sep = "")

cat("[Q11] Train/Test sizes (base) ->", nrow(train_base), "/", nrow(test_base), "\n")
cat("[Q11] Train/Test sizes (enhanced) ->", nrow(train_enh), "/", nrow(test_enh), "\n")
cat("[Q11] Train class distribution (base):\n"); print(table(train_base$engagement))
cat("[Q11] Test class distribution (base):\n"); print(table(test_base$engagement))

# Save splits (base and enhanced)
train_dir <- file.path(dataset_dir, "train_data")
if (!dir.exists(train_dir)) dir.create(train_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(train_base, file = file.path(train_dir, "train_df_base.csv"), row.names = FALSE)
utils::write.csv(test_base,  file = file.path(train_dir, "test_df_base.csv"),  row.names = FALSE)
utils::write.csv(train_enh,  file = file.path(train_dir, "train_df_enhanced.csv"), row.names = FALSE)
utils::write.csv(test_enh,   file = file.path(train_dir, "test_df_enhanced.csv"),  row.names = FALSE)
cat("[Q11] Saved train/test CSVs to ", normalizePath(train_dir, winslash = "/", mustWork = FALSE), "\n", sep = "")

# --------------------------------------------------------------------------
# Models
# --------------------------------------------------------------------------

# 1) Baseline C5.0 (base features)
cat("[Q11] Training baseline C5.0 (base features)...\n")
model_baseline <- C50::C5.0(engagement ~ comment_length + has_link + time_hour + sentiment, data = train_base)
pred_base_test <- predict(model_baseline, newdata = test_base)
cm_base <- table(Predicted = pred_base_test, Actual = test_base$engagement)
acc_base <- mean(pred_base_test == test_base$engagement)
prec_base <- tryCatch(cm_base["high","high"] / sum(cm_base["high", ]), error = function(e) NA_real_)
rec_base  <- tryCatch(cm_base["high","high"] / sum(cm_base[ ,"high"]), error = function(e) NA_real_)
f1_base   <- if (is.na(prec_base) || is.na(rec_base) || (prec_base + rec_base) == 0) NA_real_ else 2 * (prec_base * rec_base) / (prec_base + rec_base)
cat(sprintf("[Q11] Baseline -> Acc=%.3f, Prec(high)=%.3f, Rec(high)=%.3f, F1(high)=%.3f\n", acc_base, prec_base, rec_base, f1_base))

# 2) Boosted C5.0 (base features)
cat("[Q11] Training boosted C5.0 (base features, trials=10)...\n")
model_boost_base <- C50::C5.0(engagement ~ comment_length + has_link + time_hour + sentiment, data = train_base, trials = 10)
pred_boost_base_test <- predict(model_boost_base, newdata = test_base)
cm_boost_base <- table(Predicted = pred_boost_base_test, Actual = test_base$engagement)
acc_boost_base <- mean(pred_boost_base_test == test_base$engagement)
prec_boost_base <- tryCatch(cm_boost_base["high","high"] / sum(cm_boost_base["high", ]), error = function(e) NA_real_)
rec_boost_base  <- tryCatch(cm_boost_base["high","high"] / sum(cm_boost_base[ ,"high"]), error = function(e) NA_real_)
f1_boost_base   <- if (is.na(prec_boost_base) || is.na(rec_boost_base) || (prec_boost_base + rec_boost_base) == 0) NA_real_ else 2 * (prec_boost_base * rec_boost_base) / (prec_boost_base + rec_boost_base)
cat(sprintf("[Q11] Boosted (base) -> Acc=%.3f, Prec(high)=%.3f, Rec(high)=%.3f, F1(high)=%.3f\n", acc_boost_base, prec_boost_base, rec_boost_base, f1_boost_base))

# 3) Boosted C5.0 (enhanced features)
cat("[Q11] Training boosted C5.0 (enhanced features, trials=10)...\n")
model_boost_enh <- C50::C5.0(engagement ~ ., data = train_enh_x, trials = 10)
pred_boost_enh_test <- predict(model_boost_enh, newdata = test_enh_x)
cm_boost_enh <- table(Predicted = pred_boost_enh_test, Actual = test_enh_x$engagement)
acc_boost_enh <- mean(pred_boost_enh_test == test_enh_x$engagement)
prec_boost_enh <- tryCatch(cm_boost_enh["high","high"] / sum(cm_boost_enh["high", ]), error = function(e) NA_real_)
rec_boost_enh  <- tryCatch(cm_boost_enh["high","high"] / sum(cm_boost_enh[ ,"high"]), error = function(e) NA_real_)
f1_boost_enh   <- if (is.na(prec_boost_enh) || is.na(rec_boost_enh) || (prec_boost_enh + rec_boost_enh) == 0) NA_real_ else 2 * (prec_boost_enh * rec_boost_enh) / (prec_boost_enh + rec_boost_enh)
cat(sprintf("[Q11] Boosted (enhanced) -> Acc=%.3f, Prec(high)=%.3f, Rec(high)=%.3f, F1(high)=%.3f\n", acc_boost_enh, prec_boost_enh, rec_boost_enh, f1_boost_enh))

# 4) caret GLM (base features)
has_caret <- requireNamespace("caret", quietly = TRUE)
acc_caret <- prec_caret <- rec_caret <- f1_caret <- NA_real_
pred_caret_test <- NULL
cm_caret <- NULL
prob_caret_high <- NULL
if (has_caret) {
  cat("[Q11] Training caret GLM (base features)...\n")
  library(caret)
  tb <- train_base; te <- test_base
  tb$engagement <- stats::relevel(tb$engagement, ref = "high")
  te$engagement <- stats::relevel(te$engagement, ref = "high")
  ctrl <- caret::trainControl(method = "repeatedcv", number = 5, repeats = 2, classProbs = TRUE)
  model_caret <- caret::train(
    engagement ~ comment_length + has_link + time_hour + sentiment,
    data = tb,
    method = "glm",
    family = binomial(),
    trControl = ctrl
  )
  pred_caret_test <- predict(model_caret, newdata = te)
  cm_caret <- table(Predicted = pred_caret_test, Actual = te$engagement)
  acc_caret <- mean(pred_caret_test == te$engagement)
  prec_caret <- tryCatch(cm_caret["high","high"] / sum(cm_caret["high", ]), error = function(e) NA_real_)
  rec_caret  <- tryCatch(cm_caret["high","high"] / sum(cm_caret[ ,"high"]), error = function(e) NA_real_)
  f1_caret   <- if (is.na(prec_caret) || is.na(rec_caret) || (prec_caret + rec_caret) == 0) NA_real_ else 2 * (prec_caret * rec_caret) / (prec_caret + rec_caret)
  cat(sprintf("[Q11] Caret GLM -> Acc=%.3f, Prec(high)=%.3f, Rec(high)=%.3f, F1(high)=%.3f\n", acc_caret, prec_caret, rec_caret, f1_caret))
} else {
  cat("[Q11] caret not available; skipping caret GLM.\n")
}

# --------------------------------------------------------------------------
# Evaluation summary and CSV
# --------------------------------------------------------------------------

eval_summary <- data.frame(
  model = c("baseline","boosted_base","boosted_enhanced", if (has_caret) "caret_glm" else NULL),
  accuracy = c(acc_base, acc_boost_base, acc_boost_enh, if (has_caret) acc_caret else NULL),
  precision_high = c(prec_base, prec_boost_base, prec_boost_enh, if (has_caret) prec_caret else NULL),
  recall_high = c(rec_base, rec_boost_base, rec_boost_enh, if (has_caret) rec_caret else NULL),
  f1_high = c(f1_base, f1_boost_base, f1_boost_enh, if (has_caret) f1_caret else NULL),
  stringsAsFactors = FALSE
)
utils::write.csv(eval_summary, paste(dataset_dir, "q11_eval_summary.csv", sep = ""), row.names = FALSE)

cat("\n========== Q11: Reddit Engagement (High/Low) =========\n")
print(eval_summary)

# --------------------------------------------------------------------------
# Predictions (train/test CSV + sample to console)
# --------------------------------------------------------------------------

# Probabilities (positive class = 'high') for base and enhanced models
prob_base_test <- tryCatch(predict(model_baseline, newdata = test_base, type = "prob"), error = function(e) NULL)
prob_boost_base_test <- tryCatch(predict(model_boost_base, newdata = test_base, type = "prob"), error = function(e) NULL)
prob_boost_enh_test <- tryCatch(predict(model_boost_enh, newdata = test_enh_x, type = "prob"), error = function(e) NULL)
prob_base_high <- if (!is.null(prob_base_test) && "high" %in% colnames(prob_base_test)) prob_base_test[, "high"] else rep(NA_real_, nrow(test_base))
prob_boost_base_high <- if (!is.null(prob_boost_base_test) && "high" %in% colnames(prob_boost_base_test)) prob_boost_base_test[, "high"] else rep(NA_real_, nrow(test_base))
prob_boost_enh_high <- if (!is.null(prob_boost_enh_test) && "high" %in% colnames(prob_boost_enh_test)) prob_boost_enh_test[, "high"] else rep(NA_real_, nrow(test_enh_x))

# Train-side predictions (useful for diagnostics)
pred_base_train <- predict(model_baseline, newdata = train_base)
pred_boost_base_train <- predict(model_boost_base, newdata = train_base)
pred_boost_enh_train <- predict(model_boost_enh, newdata = train_enh_x)
prob_base_train <- tryCatch(predict(model_baseline, newdata = train_base, type = "prob"), error = function(e) NULL)
prob_boost_base_train <- tryCatch(predict(model_boost_base, newdata = train_base, type = "prob"), error = function(e) NULL)
prob_boost_enh_train <- tryCatch(predict(model_boost_enh, newdata = train_enh_x, type = "prob"), error = function(e) NULL)
prob_base_train_high <- if (!is.null(prob_base_train) && "high" %in% colnames(prob_base_train)) prob_base_train[, "high"] else rep(NA_real_, nrow(train_base))
prob_boost_base_train_high <- if (!is.null(prob_boost_base_train) && "high" %in% colnames(prob_boost_base_train)) prob_boost_base_train[, "high"] else rep(NA_real_, nrow(train_base))
prob_boost_enh_train_high <- if (!is.null(prob_boost_enh_train) && "high" %in% colnames(prob_boost_enh_train)) prob_boost_enh_train[, "high"] else rep(NA_real_, nrow(train_enh_x))

# Build and save CSVs
predictions_baseline_test <- data.frame(
  comment_preview = test_base$comment_preview,
  actual = test_base$engagement,
  predicted = pred_base_test,
  prob_high = prob_base_high,
  stringsAsFactors = FALSE
)
predictions_boosted_base_test <- data.frame(
  comment_preview = test_base$comment_preview,
  actual = test_base$engagement,
  predicted = pred_boost_base_test,
  prob_high = prob_boost_base_high,
  stringsAsFactors = FALSE
)
predictions_boosted_enh_test <- data.frame(
  comment_preview = test_enh$comment_preview,
  actual = test_enh$engagement,
  predicted = pred_boost_enh_test,
  prob_high = prob_boost_enh_high,
  stringsAsFactors = FALSE
)

utils::write.csv(predictions_baseline_test,      file = file.path(train_dir, "q11_predictions_test_baseline.csv"),      row.names = FALSE)
utils::write.csv(predictions_boosted_base_test,  file = file.path(train_dir, "q11_predictions_test_boosted_base.csv"),  row.names = FALSE)
utils::write.csv(predictions_boosted_enh_test,   file = file.path(train_dir, "q11_predictions_test_boosted_enh.csv"),   row.names = FALSE)

predictions_baseline_train <- data.frame(
  comment_preview = train_base$comment_preview,
  actual = train_base$engagement,
  predicted = pred_base_train,
  prob_high = prob_base_train_high,
  stringsAsFactors = FALSE
)
predictions_boosted_base_train <- data.frame(
  comment_preview = train_base$comment_preview,
  actual = train_base$engagement,
  predicted = pred_boost_base_train,
  prob_high = prob_boost_base_train_high,
  stringsAsFactors = FALSE
)
predictions_boosted_enh_train <- data.frame(
  comment_preview = train_enh$comment_preview,
  actual = train_enh$engagement,
  predicted = pred_boost_enh_train,
  prob_high = prob_boost_enh_train_high,
  stringsAsFactors = FALSE
)

utils::write.csv(predictions_baseline_train,     file = file.path(train_dir, "q11_predictions_train_baseline.csv"),     row.names = FALSE)
utils::write.csv(predictions_boosted_base_train, file = file.path(train_dir, "q11_predictions_train_boosted_base.csv"), row.names = FALSE)
utils::write.csv(predictions_boosted_enh_train,  file = file.path(train_dir, "q11_predictions_train_boosted_enh.csv"),  row.names = FALSE)

if (has_caret) {
  pr_c_test <- tryCatch(predict(model_caret, newdata = te, type = "prob"), error = function(e) NULL)
  prob_caret_high <- if (!is.null(pr_c_test) && "high" %in% colnames(pr_c_test)) pr_c_test[, "high"] else rep(NA_real_, nrow(te))
  predictions_caret_test <- data.frame(
    comment_preview = test_base$comment_preview,
    actual = test_base$engagement,
    predicted = pred_caret_test,
    prob_high = prob_caret_high,
    stringsAsFactors = FALSE
  )
  utils::write.csv(predictions_caret_test, file = file.path(train_dir, "q11_predictions_test_caret_glm.csv"), row.names = FALSE)
}

cat("\n[Q11] Sample predictions (baseline) -- first 8:\n"); print(utils::head(predictions_baseline_test, 8))
cat("\n[Q11] Sample predictions (boosted_base) -- first 8:\n");  print(utils::head(predictions_boosted_base_test, 8))
cat("\n[Q11] Sample predictions (boosted_enh) -- first 8:\n");   print(utils::head(predictions_boosted_enh_test, 8))
if (has_caret) { cat("\n[Q11] Sample predictions (caret glm) -- first 8:\n"); print(utils::head(predictions_caret_test, 8)) }

# --------------------------------------------------------------------------
# Visualizations (confusion matrices + feature importance + ROC/PR)
# --------------------------------------------------------------------------

cm_to_df <- function(cm) {
  df <- as.data.frame(cm)
  names(df) <- c("Predicted", "Actual", "Freq")
  df
}
p_cm <- function(df, title) {
  ggplot(df, aes(x = .data$Actual, y = .data$Predicted, fill = .data$Freq)) +
    geom_tile() + geom_text(aes(label = .data$Freq), color = "white", fontface = "bold") +
    scale_fill_gradient(low = "#6baed6", high = "#08306b") +
    labs(title = title, x = "Actual", y = "Predicted") +
    theme_minimal()
}

ggsave(paste(graph_dir, "q11_cm_baseline.png", sep = ""),         p_cm(cm_to_df(cm_base),        "Q11 Confusion Matrix - Baseline"),        width = 5, height = 4)

ggsave(paste(graph_dir, "q11_cm_boosted_base.png", sep = ""),    p_cm(cm_to_df(cm_boost_base),   "Q11 Confusion Matrix - Boosted (Base)"),  width = 5, height = 4)

ggsave(paste(graph_dir, "q11_cm_boosted_enh.png", sep = ""),     p_cm(cm_to_df(cm_boost_enh),    "Q11 Confusion Matrix - Boosted (Enhanced)"), width = 5, height = 4)

ggsave(file.path(q11_img_dir, "q11_cm_baseline.png"),             p_cm(cm_to_df(cm_base),        "Q11 Confusion Matrix - Baseline"),        width = 5, height = 4)

ggsave(file.path(q11_img_dir, "q11_cm_boosted_base.png"),        p_cm(cm_to_df(cm_boost_base),   "Q11 Confusion Matrix - Boosted (Base)"),  width = 5, height = 4)

ggsave(file.path(q11_img_dir, "q11_cm_boosted_enh.png"),         p_cm(cm_to_df(cm_boost_enh),    "Q11 Confusion Matrix - Boosted (Enhanced)"), width = 5, height = 4)

if (has_caret) {
  ggsave(paste(graph_dir, "q11_cm_caret_glm.png", sep = ""),      p_cm(cm_to_df(cm_caret),       "Q11 Confusion Matrix - Caret (glm)"),     width = 5, height = 4)
  ggsave(file.path(q11_img_dir, "q11_cm_caret_glm.png"),           p_cm(cm_to_df(cm_caret),       "Q11 Confusion Matrix - Caret (glm)"),     width = 5, height = 4)
}

# Feature importance from boosted enhanced model
imp_enh <- tryCatch(C50::C5imp(model_boost_enh, metric = "usage"), error = function(e) NULL)
if (!is.null(imp_enh) && nrow(imp_enh) > 0) {
  imp_enh$feature <- rownames(imp_enh)
  imp_enh <- imp_enh[order(-imp_enh$Overall), , drop = FALSE]
  top_imp_enh <- utils::head(imp_enh, 12)
  p_imp <- ggplot(top_imp_enh, aes(x = reorder(feature, Overall), y = Overall)) +
    geom_col(fill = "#1f77b4") +
    coord_flip() +
    labs(title = "Q11: Feature Importance (Boosted Enhanced)", x = "Feature", y = "Importance (usage)") +
    theme_minimal()
  ggsave(paste(graph_dir, "q11_feature_importance_enhanced.png", sep = ""), p_imp, width = 8, height = 5)
  ggsave(file.path(q11_img_dir, "q11_feature_importance_enhanced.png"), p_imp, width = 8, height = 5)
}

# ROC / PR curves (positive class = 'high')
compute_roc <- function(labels, probs) {
  y <- ifelse(as.character(labels) == "high", 1L, 0L)
  ord <- order(probs, decreasing = TRUE, na.last = NA)
  y <- y[ord]
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
compute_pr <- function(labels, probs) {
  y <- ifelse(as.character(labels) == "high", 1L, 0L)
  ord <- order(probs, decreasing = TRUE, na.last = NA)
  y <- y[ord]
  tp <- cumsum(y)
  fp <- cumsum(1L - y)
  P <- sum(y)
  if (P == 0) return(list(df = data.frame(recall = c(0,1), precision = c(1,1)), aupr = NA_real_))
  recall <- tp / P
  precision <- tp / pmax(tp + fp, 1)
  df <- data.frame(recall = c(0, recall), precision = c(1, precision))
  aupr <- sum(diff(df$recall) * (head(df$precision, -1) + tail(df$precision, -1)) / 2)
  list(df = df, aupr = aupr)
}

# Get probabilities for curves
prob_base_high_test <- prob_base_high
prob_boost_base_high_test <- prob_boost_base_high
prob_boost_enh_high_test <- prob_boost_enh_high

roc_df <- data.frame(); pr_df <- data.frame()
rb <- compute_roc(test_base$engagement, prob_base_high_test); roc_df <- rbind(roc_df, transform(rb$df, model = "Baseline"))
rb2 <- compute_roc(test_base$engagement, prob_boost_base_high_test); roc_df <- rbind(roc_df, transform(rb2$df, model = "Boosted (Base)"))
re <- compute_roc(test_enh_x$engagement, prob_boost_enh_high_test); roc_df <- rbind(roc_df, transform(re$df, model = "Boosted (Enhanced)"))

pb <- compute_pr(test_base$engagement, prob_base_high_test); pr_df <- rbind(pr_df, transform(pb$df, model = "Baseline"))
pb2 <- compute_pr(test_base$engagement, prob_boost_base_high_test); pr_df <- rbind(pr_df, transform(pb2$df, model = "Boosted (Base)"))
pe <- compute_pr(test_enh_x$engagement, prob_boost_enh_high_test); pr_df <- rbind(pr_df, transform(pe$df, model = "Boosted (Enhanced)"))

if (has_caret && !is.null(prob_caret_high)) {
  rc <- compute_roc(test_base$engagement, prob_caret_high); roc_df <- rbind(roc_df, transform(rc$df, model = "Caret (glm)"))
  pc <- compute_pr(test_base$engagement, prob_caret_high); pr_df  <- rbind(pr_df,  transform(pc$df, model = "Caret (glm)"))
}

p_roc <- ggplot(roc_df, aes(x = fpr, y = tpr, color = model)) +
  geom_line(size = 1) + geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("Baseline" = "#1f77b4", "Boosted (Base)" = "#d62728", "Boosted (Enhanced)" = "#9467bd", "Caret (glm)" = "#2ca02c")) +
  labs(title = "Q11 ROC (positive: high)", x = "False Positive Rate", y = "True Positive Rate", color = "Model") +
  theme_minimal()

ggsave(paste(graph_dir, "q11_roc.png", sep = ""), p_roc, width = 7, height = 5)

ggsave(file.path(q11_img_dir, "q11_roc.png"), p_roc, width = 7, height = 5)

p_pr <- ggplot(pr_df, aes(x = recall, y = precision, color = model)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Baseline" = "#1f77b4", "Boosted (Base)" = "#d62728", "Boosted (Enhanced)" = "#9467bd", "Caret (glm)" = "#2ca02c")) +
  labs(title = "Q11 Precision-Recall (positive: high)", x = "Recall", y = "Precision", color = "Model") +
  theme_minimal()

ggsave(paste(graph_dir, "q11_pr.png", sep = ""), p_pr, width = 7, height = 5)

ggsave(file.path(q11_img_dir, "q11_pr.png"), p_pr, width = 7, height = 5)

# --------------------------------------------------------------------------
# Final summary
# --------------------------------------------------------------------------

cat("\n[Q11] Improvement check (enhanced vs base boosted):\n")
cat(sprintf("  F1:  %.3f (enhanced) vs %.3f (base) -> %+0.3f\n", f1_boost_enh, f1_boost_base, f1_boost_enh - f1_boost_base))
cat(sprintf("  Acc: %.3f (enhanced) vs %.3f (base) -> %+0.3f\n", acc_boost_enh, acc_boost_base, acc_boost_enh - acc_boost_base))
cat(sprintf("  Prec:%.3f (enhanced) vs %.3f (base) -> %+0.3f\n", prec_boost_enh, prec_boost_base, prec_boost_enh - prec_boost_base))
cat(sprintf("  Rec: %.3f (enhanced) vs %.3f (base) -> %+0.3f\n", rec_boost_enh, rec_boost_base, rec_boost_enh - rec_boost_base))

# Better end-of-run summary including file outputs
generated_files <- list(
  splits = c(
    file.path(train_dir, "train_df_base.csv"),
    file.path(train_dir, "test_df_base.csv"),
    file.path(train_dir, "train_df_enhanced.csv"),
    file.path(train_dir, "test_df_enhanced.csv")
  ),
  predictions_test = c(
    file.path(train_dir, "q11_predictions_test_baseline.csv"),
    file.path(train_dir, "q11_predictions_test_boosted_base.csv"),
    file.path(train_dir, "q11_predictions_test_boosted_enh.csv"),
    if (has_caret) file.path(train_dir, "q11_predictions_test_caret_glm.csv") else NULL
  ),
  predictions_train = c(
    file.path(train_dir, "q11_predictions_train_baseline.csv"),
    file.path(train_dir, "q11_predictions_train_boosted_base.csv"),
    file.path(train_dir, "q11_predictions_train_boosted_enh.csv")
  ),
  metrics = c(paste(dataset_dir, "q11_eval_summary.csv", sep = "")),
  plots_graphs = c(
    paste(graph_dir, "q11_cm_baseline.png", sep = ""),
    paste(graph_dir, "q11_cm_boosted_base.png", sep = ""),
    paste(graph_dir, "q11_cm_boosted_enh.png", sep = ""),
    paste(graph_dir, "q11_roc.png", sep = ""),
    paste(graph_dir, "q11_pr.png", sep = ""),
    paste(graph_dir, "q11_feature_importance_enhanced.png", sep = "")
  ),
  plots_images = c(
    file.path(q11_img_dir, "q11_cm_baseline.png"),
    file.path(q11_img_dir, "q11_cm_boosted_base.png"),
    file.path(q11_img_dir, "q11_cm_boosted_enh.png"),
    file.path(q11_img_dir, "q11_roc.png"),
    file.path(q11_img_dir, "q11_pr.png"),
    file.path(q11_img_dir, "q11_feature_importance_enhanced.png")
  )
)

cat("\n[Q11] Files generated:\n")
for (group in names(generated_files)) {
  cat("  ", group, ":\n", sep = "")
  files <- generated_files[[group]]
  files <- files[!is.na(files)]
  for (f in files) cat("    - ", normalizePath(f, winslash = "/", mustWork = FALSE), "\n", sep = "")
}

cat("\n[Q11] Artifacts saved to:\n  ", normalizePath(graph_dir, winslash = "/", mustWork = FALSE), "\n  ", normalizePath(q11_img_dir, winslash = "/", mustWork = FALSE), "\n  ", normalizePath(train_dir, winslash = "/", mustWork = FALSE), "\n", sep = "")