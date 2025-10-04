# =========================================================================
# Q14: Additional methods & comparisons
# - Network: Infomap communities vs Louvain/Girvan–Newman
# - ML: XGBoost engagement model vs C5.0/GLM (Q11)
# Outputs written to ./data, ./graphs and ./images/q14
# =========================================================================

if (!exists("dataset_dir")) dataset_dir <- ".//data//"
if (!exists("graph_dir")) graph_dir <- ".//graphs//"
images_base_dir <- ".//images//"
q14_img_dir <- file.path(images_base_dir, "q14")
if (!dir.exists(images_base_dir)) dir.create(images_base_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(q14_img_dir)) dir.create(q14_img_dir, recursive = TRUE, showWarnings = FALSE)

suppressWarnings({
  library(igraph)
  library(ggplot2)
  library(tidyr)
  library(Matrix)
})

cat("\n[Q14] ================= Additional Methods & Comparisons =================\n")

# ---------------------------------------------------------------------------
# Utility: safe write CSV
# ---------------------------------------------------------------------------

write_csv_safe <- function(df, path) {
  tryCatch(utils::write.csv(df, file = path, row.names = FALSE), error = function(e) {
    message("[Q14] Failed to write ", path, ": ", e$message)
    invisible(NULL)
  })
}

# ---------------------------------------------------------------------------
# PART A. Network Analysis: Infomap vs Louvain (existing) and GN (existing)
# ---------------------------------------------------------------------------

analyze_infomap <- function(g, label_prefix, out_dir) {
  if (is.null(g)) return(NULL)

  ug <- igraph::as_undirected(g, mode = "collapse")
  cat("[Q14][", label_prefix, "] Graph loaded: ", vcount(ug), " nodes, ", ecount(ug), " edges\n", sep = "")

  # Existing baselines (recompute to ensure same graph base)
  cat("[Q14][", label_prefix, "] Running Louvain ... ", sep = ""); t0 <- proc.time()
  louv <- tryCatch(igraph::cluster_louvain(ug), error = function(e) { cat("error: ", e$message, "\n", sep = ""); NULL })
  if (!is.null(louv)) cat("done (", length(louv), " communities, modularity=", round(tryCatch(igraph::modularity(louv), error = function(e) NA_real_), 4), ") in ", round((proc.time()-t0)[[3]], 2), "s\n", sep = "")

  cat("[Q14][", label_prefix, "] Running Girvan–Newman ... ", sep = ""); t1 <- proc.time()
  gn   <- tryCatch(igraph::cluster_edge_betweenness(ug), error = function(e) { cat("error: ", e$message, "\n", sep = ""); NULL })
  if (!is.null(gn)) cat("done (", length(gn), " communities) in ", round((proc.time()-t1)[[3]], 2), "s\n", sep = "")

  cat("[Q14][", label_prefix, "] Running Infomap ... ", sep = ""); t2 <- proc.time()
  info <- tryCatch(igraph::cluster_infomap(ug), error = function(e) { cat("error: ", e$message, "\n", sep = ""); NULL })
  if (!is.null(info)) cat("done (", length(info), " communities) in ", round((proc.time()-t2)[[3]], 2), "s\n", sep = "")

  # Membership exports
  if (!is.null(info)) {
    info_df <- data.frame(node = igraph::V(ug)$name, community = info$membership, stringsAsFactors = FALSE)
    outp <- paste(out_dir, paste0(tolower(label_prefix), "_infomap_membership.csv"), sep = "")
    write_csv_safe(info_df, outp)
    cat("[Q14][", label_prefix, "] Wrote Infomap membership: ", outp, " (", nrow(info_df), " rows)\n", sep = "")
  }

  # Summary comparison
  summary_list <- list()
  if (!is.null(louv)) summary_list[[length(summary_list)+1L]] <- data.frame(
    method = "louvain",
    communities = length(louv),
    modularity = tryCatch(igraph::modularity(louv), error = function(e) NA_real_),
    stringsAsFactors = FALSE
  )
  if (!is.null(gn)) summary_list[[length(summary_list)+1L]] <- data.frame(
    method = "girvan_newman",
    communities = length(gn),
    modularity = NA_real_,  # GN modularity not directly comparable
    stringsAsFactors = FALSE
  )
  if (!is.null(info)) summary_list[[length(summary_list)+1L]] <- data.frame(
    method = "infomap",
    communities = length(info),
    modularity = NA_real_,  # Infomap is not modularity-based
    stringsAsFactors = FALSE
  )

  comp_df <- if (length(summary_list) > 0) do.call(rbind, summary_list) else data.frame()
  if (nrow(comp_df) > 0) {
    outp2 <- paste(out_dir, paste0(tolower(label_prefix), "_community_method_comparison.csv"), sep = "")
    write_csv_safe(comp_df, outp2)
    cat("[Q14][", label_prefix, "] Community method comparison saved: ", outp2, "\n", sep = "")
    cat("[Q14][", label_prefix, "] Summary — ", paste(comp_df$method, comp_df$communities, sep = ": ", collapse = "; "), "\n", sep = "")
  }

  invisible(list(graph = ug, louvain = louv, girvan = gn, infomap = info, comparison = comp_df))
}

# Run for Reddit and YouTube actor graphs if present
# Only run for Reddit for now (YouTube temporarily disabled)
# for (platform in c("Reddit", "YouTube")) {
for (platform in c("Reddit")) {
  f <- paste(graph_dir, paste0(platform, "Actor.rds"), sep = "")
  if (file.exists(f)) {
    cat("[Q14] Loading actor graph for ", platform, ": ", f, "\n", sep = "")
    res <- analyze_infomap(readRDS(f), platform, dataset_dir)
  } else {
    cat("[Q14] ", platform, " actor graph not found: ", f, "\n", sep = "")
  }
}

# ---------------------------------------------------------------------------
# PART B. ML: XGBoost for engagement vs Q11 C5.0/GLM
# ---------------------------------------------------------------------------

has_xgb <- requireNamespace("xgboost", quietly = TRUE)
if (!has_xgb) {
  cat("\n[Q14] xgboost package not available. Skipping XGBoost training.\n")
} else {
  library(xgboost)

  train_dir <- file.path(dataset_dir, "train_data")
  train_enh_path <- file.path(train_dir, "train_df_enhanced.csv")
  test_enh_path  <- file.path(train_dir,  "test_df_enhanced.csv")

  if (!file.exists(train_enh_path) || !file.exists(test_enh_path)) {
    cat("\n[Q14] Enhanced train/test splits not found. Please run q11_decision_tree.R first.\n")
  } else {
    cat("[Q14][XGB] Loading splits:\n  - ", train_enh_path, "\n  - ", test_enh_path, "\n", sep = "")
    train_enh <- utils::read.csv(train_enh_path, stringsAsFactors = FALSE)
    test_enh  <- utils::read.csv(test_enh_path,  stringsAsFactors = FALSE)

    # Prepare features and labels
    to_numeric_frame <- function(df) {
      # Drop preview text if present
      drop_cols <- intersect(c("comment_preview"), names(df))
      if (length(drop_cols) > 0) df <- df[ , setdiff(names(df), drop_cols), drop = FALSE]
      # Ensure engagement is factor with low/high; keep separate
      y <- NULL
      if ("engagement" %in% names(df)) {
        y <- factor(df$engagement, levels = c("low","high"))
        df$engagement <- NULL
      }
      # Model matrix encodes factors safely
      mm <- stats::model.matrix(~ . - 1, data = df)
      list(X = mm, y = y)
    }

    prep_tr <- to_numeric_frame(train_enh)
    prep_te <- to_numeric_frame(test_enh)
    cat("[Q14][XGB] Train raw dims: ", nrow(train_enh), " x ", ncol(train_enh), "; Test raw dims: ", nrow(test_enh), " x ", ncol(test_enh), "\n", sep = "")

    # Align columns between train and test
    common_cols <- intersect(colnames(prep_tr$X), colnames(prep_te$X))
    X_tr <- prep_tr$X[ , common_cols, drop = FALSE]
    X_te <- prep_te$X[ , common_cols, drop = FALSE]
    cat("[Q14][XGB] Common feature columns: ", length(common_cols), "\n", sep = "")

    y_tr <- if (!is.null(prep_tr$y)) as.integer(prep_tr$y == "high") else NULL
    y_te <- if (!is.null(prep_te$y)) as.integer(prep_te$y == "high") else NULL
    if (!is.null(prep_tr$y)) cat("[Q14][XGB] Train label distribution (high=1): ", sum(y_tr), "/", length(y_tr), "\n", sep = "")
    if (!is.null(prep_te$y)) cat("[Q14][XGB] Test  label distribution (high=1): ", sum(y_te), "/", length(y_te), "\n", sep = "")

    if (is.null(y_tr) || is.null(y_te)) {
      stop("[Q14] Could not find engagement labels in enhanced splits.")
    }

    dtrain <- xgboost::xgb.DMatrix(data = X_tr, label = y_tr)
    dtest  <- xgboost::xgb.DMatrix(data = X_te, label = y_te)

    params <- list(
      objective = "binary:logistic",
      eval_metric = c("logloss","auc"),
      eta = 0.1,
      max_depth = 6,
      subsample = 0.8,
      colsample_bytree = 0.8,
      lambda = 1.0,
      alpha = 0
    )

    cat("\n[Q14][XGB] Training XGBoost (enhanced features) ...\n")
    cat("[Q14][XGB] Params: ", paste(paste(names(params), params, sep = "="), collapse = ", "), "; rounds=300, early_stop=25\n", sep = "")
    watch <- list(train = dtrain, eval = dtest)
    t3 <- proc.time()
    xgb_fit <- xgboost::xgb.train(
      params = params,
      data = dtrain,
      nrounds = 300,
      watchlist = watch,
      early_stopping_rounds = 25,
      verbose = 1
    )
    cat("[Q14][XGB] Training completed in ", round((proc.time()-t3)[[3]], 2), "s; best_iter=", xgb_fit$best_iteration, ", best_score=", round(xgb_fit$best_score, 4), " (", xgb_fit$best_msg, ")\n", sep = "")

    # Predictions
    prob_te <- predict(xgb_fit, dtest)
    pred_te <- ifelse(prob_te >= 0.5, 1L, 0L)
    actual  <- y_te

    # Metrics
    acc <- mean(pred_te == actual)
    tp <- sum(pred_te == 1L & actual == 1L)
    fp <- sum(pred_te == 1L & actual == 0L)
    fn <- sum(pred_te == 0L & actual == 1L)
    prec <- if ((tp + fp) == 0) NA_real_ else tp / (tp + fp)
    rec  <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
    f1   <- if (is.na(prec) || is.na(rec) || (prec + rec) == 0) NA_real_ else 2 * (prec * rec) / (prec + rec)

    cat(sprintf("[Q14][XGB] Metrics — Acc=%.3f, Prec(high)=%.3f, Rec(high)=%.3f, F1(high)=%.3f\n", acc, prec, rec, f1))

    # Save predictions
    pred_out <- data.frame(
      comment_preview = test_enh$comment_preview,
      actual = factor(ifelse(actual == 1L, "high","low"), levels = c("low","high")),
      predicted = factor(ifelse(pred_te == 1L, "high","low"), levels = c("low","high")),
      prob_high = prob_te,
      stringsAsFactors = FALSE
    )
    pred_path <- file.path(train_dir, "q14_predictions_test_xgboost.csv")
    write_csv_safe(pred_out, pred_path)
    cat("[Q14][XGB] Wrote predictions: ", pred_path, " (", nrow(pred_out), " rows)\n", sep = "")

    # Append comparison vs Q11
    eval_q14 <- data.frame(
      model = c("xgboost_enhanced"),
      accuracy = c(acc),
      precision_high = c(prec),
      recall_high = c(rec),
      f1_high = c(f1),
      stringsAsFactors = FALSE
    )
    q14_eval_path <- paste(dataset_dir, "q14_eval_summary.csv", sep = "")
    write_csv_safe(eval_q14, q14_eval_path)
    cat("[Q14][XGB] Wrote eval summary: ", q14_eval_path, "\n", sep = "")

    # Merge with Q11 eval if present
    q11_eval_path <- paste(dataset_dir, "q11_eval_summary.csv", sep = "")
    if (file.exists(q11_eval_path)) {
      q11_eval <- tryCatch(utils::read.csv(q11_eval_path, stringsAsFactors = FALSE), error = function(e) NULL)
      if (!is.null(q11_eval)) {
        # Standardize column names for merge
        names(q11_eval) <- tolower(names(q11_eval))
        comp <- rbind(q11_eval, eval_q14)
        comp_path <- paste(dataset_dir, "q14_eval_compare.csv", sep = "")
        write_csv_safe(comp, comp_path)
        cat("[Q14][XGB] Wrote eval comparison with Q11: ", comp_path, "\n", sep = "")
      }
    }

    # ROC and PR curves
    compute_roc <- function(labels01, probs) {
      ord <- order(probs, decreasing = TRUE, na.last = NA)
      y <- labels01[ord]
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
    compute_pr <- function(labels01, probs) {
      ord <- order(probs, decreasing = TRUE, na.last = NA)
      y <- labels01[ord]
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

    roc <- compute_roc(actual, prob_te)
    pr  <- compute_pr(actual, prob_te)

    p_roc <- ggplot(roc$df, aes(x = fpr, y = tpr)) +
      geom_line(color = "#1f77b4", linewidth = 1) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
      labs(title = "Q14 ROC — XGBoost (positive: high)", x = "False Positive Rate", y = "True Positive Rate") +
      theme_minimal()
    roc_path_g <- paste(graph_dir, "q14_xgb_roc.png", sep = "")
    roc_path_i <- file.path(q14_img_dir, "q14_xgb_roc.png")
    ggsave(roc_path_g, p_roc, width = 7, height = 5)
    ggsave(roc_path_i, p_roc, width = 7, height = 5)
    cat("[Q14][XGB] Saved ROC plots to:\n  - ", roc_path_g, "\n  - ", roc_path_i, "\n", sep = "")

    p_pr <- ggplot(pr$df, aes(x = recall, y = precision)) +
      geom_line(color = "#d62728", linewidth = 1) +
      labs(title = "Q14 Precision-Recall — XGBoost (positive: high)", x = "Recall", y = "Precision") +
      theme_minimal()
    pr_path_g <- paste(graph_dir, "q14_xgb_pr.png", sep = "")
    pr_path_i <- file.path(q14_img_dir, "q14_xgb_pr.png")
    ggsave(pr_path_g, p_pr, width = 7, height = 5)
    ggsave(pr_path_i, p_pr, width = 7, height = 5)
    cat("[Q14][XGB] Saved PR plots to:\n  - ", pr_path_g, "\n  - ", pr_path_i, "\n", sep = "")
  }
}

cat("\n[Q14] Done. Key outputs:\n",
    "  - data/*_infomap_membership.csv\n",
    "  - data/*_community_method_comparison.csv\n",
    "  - data/q14_eval_summary.csv, data/q14_eval_compare.csv\n",
    "  - data/train_data/q14_predictions_test_xgboost.csv\n",
    "  - graphs/q14_xgb_{roc,pr}.png and images/q14/*.png\n", sep = "")

# ---------------------------------------------------------------------------
# PART C. Visualizations for report (Network compare, ML compare)
# ---------------------------------------------------------------------------

# 1) Network method comparison (Reddit)
net_cmp_path <- paste(dataset_dir, "reddit_community_method_comparison.csv", sep = "")
if (file.exists(net_cmp_path)) {
  net_df <- tryCatch(utils::read.csv(net_cmp_path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (!is.null(net_df) && nrow(net_df) > 0) {
    louv_mod <- tryCatch(net_df$modularity[net_df$method == "louvain"][1], error = function(e) NA_real_)
    net_df$method <- factor(net_df$method,
                            levels = c("louvain", "girvan_newman", "infomap"),
                            labels = c("Louvain", "Girvan–Newman", "Infomap"))
    p_nc <- ggplot(net_df, aes(x = method, y = communities, fill = method)) +
      geom_col(width = 0.65) +
      geom_text(aes(label = communities), vjust = -0.5, size = 3.5) +
      scale_fill_manual(values = c("Louvain" = "#1b9e77", "Girvan–Newman" = "#d95f02", "Infomap" = "#7570b3")) +
      labs(title = "Reddit Community Detection: Method Comparison",
           subtitle = paste0("Louvain modularity = ", ifelse(is.na(louv_mod), "NA", sprintf("%.3f", louv_mod))),
           x = "", y = "Number of communities") +
      theme_minimal() +
      theme(legend.position = "none")
    out_g <- paste(graph_dir, "q14_network_compare.png", sep = "")
    out_i <- file.path(q14_img_dir, "q14_network_compare.png")
    ggsave(out_g, p_nc, width = 7, height = 5)
    ggsave(out_i, p_nc, width = 7, height = 5)
    cat("[Q14][Viz] Saved network comparison plot to:\n  - ", out_g, "\n  - ", out_i, "\n", sep = "")
  }
}

# 2) ML model comparison (F1 and Accuracy)
eval_cmp_path <- paste(dataset_dir, "q14_eval_compare.csv", sep = "")
eval_src <- if (file.exists(eval_cmp_path)) eval_cmp_path else paste(dataset_dir, "q14_eval_summary.csv", sep = "")
if (file.exists(eval_src)) {
  ev <- tryCatch(utils::read.csv(eval_src, stringsAsFactors = FALSE), error = function(e) NULL)
  if (!is.null(ev) && nrow(ev) > 0) {
    names(ev) <- tolower(names(ev))
    needed <- c("model", "accuracy", "f1_high")
    if (all(needed %in% names(ev))) {
      long <- tidyr::pivot_longer(ev, cols = c("f1_high", "accuracy"), names_to = "metric", values_to = "value")
      long$metric <- factor(long$metric, levels = c("f1_high", "accuracy"), labels = c("F1 (high)", "Accuracy"))
      p_ml <- ggplot(long, aes(x = reorder(model, value), y = value, fill = metric)) +
        geom_col(position = position_dodge(width = 0.7), width = 0.65) +
        geom_text(aes(label = sprintf("%.3f", value)), position = position_dodge(width = 0.7), vjust = -0.4, size = 3) +
        coord_flip() +
        scale_fill_manual(values = c("F1 (high)" = "#7570b3", "Accuracy" = "#1b9e77")) +
        scale_y_continuous(limits = c(0, 1)) +
        labs(title = "Model Performance Comparison",
             subtitle = "Higher is better (Reddit engagement: positive class = high)",
             x = "Model", y = "Score", fill = "Metric") +
        theme_minimal()
      out_g2 <- paste(graph_dir, "q14_ml_compare.png", sep = "")
      out_i2 <- file.path(q14_img_dir, "q14_ml_compare.png")
      ggsave(out_g2, p_ml, width = 8, height = 5)
      ggsave(out_i2, p_ml, width = 8, height = 5)
      cat("[Q14][Viz] Saved ML comparison plot to:\n  - ", out_g2, "\n  - ", out_i2, "\n", sep = "")
    }
  }
}


