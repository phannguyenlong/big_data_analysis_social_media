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
    c("author", "user", "User", "screen_name", "AuthorDisplayName")
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


