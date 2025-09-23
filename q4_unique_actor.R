# =========================================================================
# QUESTION 4: Unique actors across datasets
# Calculates unique actor counts for YouTube, Reddit, and combined.
# =========================================================================

# Ensure dataset_dir exists when running standalone
if (!exists("dataset_dir")) {
  dataset_dir <- ".//data//"
}

# Helper to load dataset from RDS with CSV fallback
safe_load_dataset <- function(rds_path, csv_path) {
  data_obj <- NULL
  if (file.exists(rds_path)) {
    data_obj <- tryCatch(readRDS(rds_path), error = function(e) NULL)
  }
  if (is.null(data_obj) && file.exists(csv_path)) {
    data_obj <- tryCatch(utils::read.csv(csv_path, stringsAsFactors = FALSE), error = function(e) NULL)
  }
  return(data_obj)
}

# Helper to pick first existing column from candidates
first_existing_column <- function(df, candidates) {
  for (candidate in candidates) {
    if (candidate %in% names(df)) return(candidate)
  }
  return(NA_character_)
}

# Helper to normalize author names for comparison
normalize_authors <- function(x) {
  values <- tolower(trimws(as.character(x)))
  values[values == ""] <- NA_character_
  values <- stats::na.omit(values)
  return(unique(values))
}

# File paths
yt_rds  <- paste(dataset_dir, "yt_data.rds", sep = "")
yt_csv  <- paste(dataset_dir, "yt_data.csv", sep = "")
rd_rds  <- paste(dataset_dir, "rd_data.rds", sep = "")
rd_csv  <- paste(dataset_dir, "rd_data.csv", sep = "")

# Load datasets
yt_data <- safe_load_dataset(yt_rds, yt_csv)
rd_data <- safe_load_dataset(rd_rds, rd_csv)

# Extract and normalize author names
yt_authors <- character(0)
rd_authors <- character(0)

if (!is.null(yt_data) && is.data.frame(yt_data) && nrow(yt_data) > 0) {
  yt_author_col <- first_existing_column(
    yt_data,
    c("AuthorDisplayName", "author", "author_display_name", "user", "User", "screen_name")
  )
  if (!is.na(yt_author_col)) {
    yt_authors <- normalize_authors(yt_data[[yt_author_col]])
  }
}

if (!is.null(rd_data) && is.data.frame(rd_data) && nrow(rd_data) > 0) {
  rd_author_col <- first_existing_column(
    rd_data,
    c("user", "User", "screen_name", "author", "AuthorDisplayName")
  )
  if (!is.na(rd_author_col)) {
    rd_authors <- normalize_authors(rd_data[[rd_author_col]])
  }
}

# Compute counts
n_yt_unique <- length(yt_authors)
n_rd_unique <- length(rd_authors)
overlap_count <- length(intersect(yt_authors, rd_authors))
n_combined_unique <- length(unique(c(yt_authors, rd_authors)))
n_yt_only <- length(setdiff(yt_authors, rd_authors))
n_rd_only <- length(setdiff(rd_authors, yt_authors))

# Prepare summary
summary_df <- data.frame(
  metric = c(
    "unique_youtube_actors",
    "unique_reddit_actors",
    "unique_combined_actors",
    "overlap_same_names",
    "youtube_only",
    "reddit_only"
  ),
  value = c(
    n_yt_unique,
    n_rd_unique,
    n_combined_unique,
    overlap_count,
    n_yt_only,
    n_rd_only
  ),
  stringsAsFactors = FALSE
)

# Write summary to disk
summary_path <- paste(dataset_dir, "unique_actors_summary.csv", sep = "")
utils::write.csv(summary_df, file = summary_path, row.names = FALSE)

# Write per-platform unique actor lists
yt_list_path <- paste(dataset_dir, "yt_unique_actors.csv", sep = "")
rd_list_path <- paste(dataset_dir, "rd_unique_actors.csv", sep = "")
utils::write.csv(
  data.frame(actor = sort(yt_authors), platform = "YouTube", stringsAsFactors = FALSE),
  file = yt_list_path,
  row.names = FALSE
)
utils::write.csv(
  data.frame(actor = sort(rd_authors), platform = "Reddit", stringsAsFactors = FALSE),
  file = rd_list_path,
  row.names = FALSE
)

# Write combined and overlap lists
combined_authors <- sort(unique(c(yt_authors, rd_authors)))
overlap_authors  <- sort(intersect(yt_authors, rd_authors))
combined_list_path <- paste(dataset_dir, "combined_unique_actors.csv", sep = "")
overlap_list_path  <- paste(dataset_dir, "overlap_actors.csv", sep = "")
utils::write.csv(data.frame(actor = combined_authors, stringsAsFactors = FALSE), combined_list_path, row.names = FALSE)
utils::write.csv(data.frame(actor = overlap_authors,  stringsAsFactors = FALSE), overlap_list_path,  row.names = FALSE)

# Console output
cat("\n========== UNIQUE ACTOR SUMMARY =========\n")
cat(paste0("YouTube unique actors: ", n_yt_unique, "\n"))
cat(paste0("Reddit unique actors:  ", n_rd_unique, "\n"))
cat(paste0("Combined unique actors (case-insensitive): ", n_combined_unique, "\n"))
cat(paste0("Overlap (same names across platforms): ", overlap_count, "\n"))
cat(paste0("YouTube-only: ", n_yt_only, "; Reddit-only: ", n_rd_only, "\n"))
cat(paste0("Summary written to: ", summary_path, "\n"))
cat(paste0("YouTube unique list: ", yt_list_path, "\n"))
cat(paste0("Reddit unique list:  ", rd_list_path, "\n"))
cat(paste0("Combined unique list: ", combined_list_path, "\n"))
cat(paste0("Overlap list: ", overlap_list_path, "\n"))

# ==================================================
# Visualization
# ==================================================

# Load required libraries
library(ggplot2)
library(tidyr)

# Create data frame with the results
actor_data <- data.frame(
  Platform = rep(c("YouTube", "Reddit"), each = 2),
  Metric = rep(c("Unique Actors", "Total Comments"), 2),
  Count = c(4129, 5076,  # YouTube: unique actors, total comments
            2865, 4968)  # Reddit: unique actors, total comments
)

# Create grouped bar chart
p <- ggplot(actor_data, aes(x = Platform, y = Count, fill = Metric)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = format(Count, big.mark = ",")), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Unique Actors" = "#1f77b4", 
                               "Total Comments" = "#ff7f0e")) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, max(actor_data$Count) * 1.1),
                     labels = scales::comma) +
  labs(title = "Unique Actors vs Total Comments by Platform",
       subtitle = "Zero overlap between platforms (0 shared users)",
       x = "Platform",
       y = "Count",
       fill = "Metric") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# Display the plot
print(p)

# Save the plot
ggsave("actor_comparison.png", plot = p, width = 8, height = 6, dpi = 300)

# Alternative: Create a summary table to accompany the visualization
summary_table <- data.frame(
  Platform = c("YouTube", "Reddit", "Combined"),
  `Unique Actors` = c(4129, 2865, 6994),
  `Total Comments` = c(5076, 4968, 10044),
  `Comments per Actor` = c(1.23, 1.73, 1.44),
  `Platform Share` = c("59%", "41%", "100%")
)

print(summary_table)

