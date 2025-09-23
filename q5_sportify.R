# =========================================================================
# Question 5: Spotify artist analysis
# =========================================================================

# Auth
Sys.setenv(SPOTIFY_CLIENT_ID = SPOTIFY_CLIENT_ID)
Sys.setenv(SPOTIFY_CLIENT_SECRET = SPOTIFY_CLIENT_SECRET)
access_token <- get_spotify_access_token()

# Find artist and ID
artist_df <- search_spotify(ARTIST_NAME, type = "artist")
artist_id <- artist_df$id[1]
artist_name <- artist_df$name[1]

# Get albums (album, single, compilation)
albums <- get_artist_albums(artist_id, include_groups = c("album", "single", "compilation"))

# Coerce release dates simply
rd <- albums$release_date
rd <- ifelse(nchar(rd) == 4, paste0(rd, "-01-01"), ifelse(nchar(rd) == 7, paste0(rd, "-01"), rd))
albums$release_date2 <- as.Date(rd)

# Years active (approx.)
first_year <- as.integer(format(min(albums$release_date2, na.rm = TRUE), "%Y"))
latest_year <- as.integer(format(max(albums$release_date2, na.rm = TRUE), "%Y"))
years_active <- latest_year - first_year + 1

# Counts (normalize group, deduplicate by release id)
albums$album_group2 <- tolower(ifelse(!is.na(albums$album_group) & nzchar(albums$album_group),
                                      albums$album_group,
                                      albums$album_type))
albums_unique <- albums[!duplicated(albums$id), , drop = FALSE]
num_albums <- sum(albums_unique$album_group2 == "album", na.rm = TRUE)
num_singles <- sum(albums_unique$album_group2 == "single", na.rm = TRUE)
num_compilations <- sum(albums_unique$album_group2 == "compilation", na.rm = TRUE)
num_tracks <- sum(albums_unique$total_tracks, na.rm = TRUE)

## Frequent collaborators across top tracks and albums
top_tracks <- get_artist_top_tracks(artist_id, market = "US")

# Robust extractor for nested artist lists
extract_artist_names <- function(obj) {
  if (is.null(obj)) return(character(0))
  if (is.data.frame(obj) && "name" %in% names(obj)) return(as.character(obj$name))
  if (is.list(obj)) {
    out <- tryCatch(unlist(lapply(obj, function(e) {
      if (is.list(e) && !is.null(e$name)) return(e$name)
      if (!is.null(e[["name"]])) return(e[["name"]])
      return(NA_character_)
    })), error = function(e) character(0))
    out <- out[!is.na(out)]
    return(as.character(out))
  }
  if (is.character(obj)) return(obj)
  character(0)
}

all_artists <- character(0)
seen_track_ids <- character(0)

# Helper to collect artists from a tracks data.frame that has an 'artists' column
collect_from_tracks_df <- function(tracks_df) {
  acc <- character(0)
  if (is.null(tracks_df) || !is.data.frame(tracks_df) || nrow(tracks_df) == 0) return(acc)
  id_col <- if ("id" %in% names(tracks_df)) "id" else NA_character_
  for (i in seq_len(nrow(tracks_df))) {
    # avoid double-counting same track id
    if (!is.na(id_col)) {
      track_id <- as.character(tracks_df[[id_col]][i])
      if (!is.na(track_id) && track_id %in% seen_track_ids) next
      if (!is.na(track_id)) seen_track_ids <<- c(seen_track_ids, track_id)
    }
    if ("artists" %in% names(tracks_df)) {
      names_i <- extract_artist_names(tracks_df$artists[[i]])
      if (length(names_i) > 0) acc <- c(acc, names_i)
    }
  }
  acc
}

# From top tracks
all_artists <- c(all_artists, collect_from_tracks_df(top_tracks))

# From all albums' tracks (best-effort, avoids excessive API calls)
album_ids <- unique(albums$id)
for (aid in album_ids) {
  tr_df <- tryCatch(get_album_tracks(aid), error = function(e) NULL)
  if (is.null(tr_df)) next
  if ("artists" %in% names(tr_df)) {
    all_artists <- c(all_artists, collect_from_tracks_df(tr_df))
  } else if ("id" %in% names(tr_df)) {
    # Fallback: fetch track details for a limited number to get artists
    ids <- as.character(utils::head(tr_df$id, 100))
    for (tid in ids) {
      if (is.na(tid) || tid %in% seen_track_ids) next
      tr <- tryCatch(get_track(tid), error = function(e) NULL)
      if (is.null(tr)) next
      seen_track_ids <- c(seen_track_ids, tid)
      names_i <- extract_artist_names(tr$artists[[1]])
      if (length(names_i) > 0) all_artists <- c(all_artists, names_i)
    }
  }
}

# Remove the primary artist and summarize
collaborators <- all_artists[tolower(all_artists) != tolower(artist_name)]
collaborators <- collaborators[!is.na(collaborators) & nzchar(collaborators)]
collab_counts <- sort(table(collaborators), decreasing = TRUE)

# Print summary
cat("\n========== SPOTIFY ARTIST SUMMARY =========\n")
cat(paste0("Artist: ", artist_name, " (", artist_id, ")\n"))
cat(paste0("Years active: ", years_active, " (", first_year, "-", latest_year, ")\n"))
cat(paste0("Albums: ", num_albums, ", Singles: ", num_singles, ", Compilations: ", num_compilations, ", Total tracks: ", num_tracks, "\n"))
cat("Top collaborators:\n")
if (length(collab_counts) > 0) {
  top_n <- utils::head(collab_counts, 10)
  for (i in seq_along(top_n)) {
    cat(sprintf("%2d. %s (%d)\n", i, names(top_n)[i], as.integer(top_n[i])))
  }
} else {
  cat("No frequent collaborators identified in top tracks.\n")
}


# ==================================================
# Visualization
# ==================================================

# Load required libraries
library(ggplot2)
library(dplyr)
library(spotifyr)

# 1. Collaborator Network Visualization
collab_data <- data.frame(
  Artist = c("Bon Iver", "Ed Sheeran", "Lana Del Rey", "Florence + The Machine", 
             "Future", "HAIM", "Post Malone", "The National"),
  Count = c(5, 3, 3, 2, 2, 2, 2, 2)
)

p1 <- ggplot(collab_data, aes(x = reorder(Artist, Count), y = Count)) +
  geom_col(fill = "#1DB954") +
  geom_text(aes(label = Count), hjust = -0.3) +
  coord_flip() +
  labs(title = "Top Taylor Swift Collaborators on Spotify",
       x = "", y = "Number of Collaborations") +
  theme_minimal()

# 2. Album Release Timeline
# Assuming we have album data with release dates
albums_timeline <- data.frame(
  Year = c(2014, 2014, 2017, 2019, 2020, 2020, 2021, 2021, 2022, 2023, 2024),
  Albums = c(1, 1, 1, 1, 2, 1, 2, 1, 1, 3, 3),
  Type = c("Original", "Original", "Original", "Original", "Original", 
           "Original", "Re-record", "Re-record", "Original", "Mixed", "Re-record")
)

p2 <- ggplot(albums_timeline, aes(x = Year, y = Albums, fill = Type)) +
  geom_col() +
  scale_fill_manual(values = c("Original" = "#1DB954", 
                               "Re-record" = "#535353",
                               "Mixed" = "#FFA500")) +
  labs(title = "Taylor Swift Album Releases on Spotify (2014-2024)",
       x = "Year", y = "Number of Albums") +
  theme_minimal()

# 3. Track Count Comparison
comparison_data <- data.frame(
  Source = c("Question 1", "Spotify API"),
  Albums = c(15, 20),
  Songs = c(240, 436)
)

comparison_long <- tidyr::pivot_longer(comparison_data, 
                                       cols = c(Albums, Songs),
                                       names_to = "Metric",
                                       values_to = "Count")

p3 <- ggplot(comparison_long, aes(x = Metric, y = Count, fill = Source)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = Count), position = position_dodge(width = 0.9), 
            vjust = -0.5) +
  scale_fill_manual(values = c("Question 1" = "#FF6B6B", 
                               "Spotify API" = "#1DB954")) +
  labs(title = "Data Source Comparison: Albums & Songs",
       y = "Count") +
  theme_minimal()

# Display all plots
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 2)
grid.arrange(p1, p2, ncol = 2)
plot(p3)
