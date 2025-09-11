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

# Counts
num_albums <- sum(albums$album_group == "album", na.rm = TRUE)
num_singles <- sum(albums$album_group == "single", na.rm = TRUE)
num_compilations <- sum(albums$album_group == "compilation", na.rm = TRUE)
num_tracks <- sum(albums$total_tracks, na.rm = TRUE)

# Frequent collaborators from top tracks
top_tracks <- get_artist_top_tracks(artist_id, market = "US")
all_artists <- c()
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
for (i in seq_len(nrow(top_tracks))) {
  names_i <- extract_artist_names(top_tracks$artists[[i]])
  if (length(names_i) > 0) all_artists <- c(all_artists, names_i)
}
collaborators <- all_artists[tolower(all_artists) != tolower(artist_name)]
collab_counts <- sort(table(collaborators), decreasing = TRUE)

# Print summary
cat("\n========== SPOTIFY ARTIST SUMMARY (Simple) =========\n")
cat(paste0("Artist: ", artist_name, " (", artist_id, ")\n"))
cat(paste0("Years active: ", years_active, " (", first_year, "-", latest_year, ")\n"))
cat(paste0("Albums: ", num_albums, ", Singles: ", num_singles, ", Compilations: ", num_compilations, ", Total tracks: ", num_tracks, "\n"))
cat("Top collaborators (from top tracks):\n")
if (length(collab_counts) > 0) {
  top_n <- utils::head(collab_counts, 10)
  for (i in seq_along(top_n)) {
    cat(sprintf("%2d. %s (%d)\n", i, names(top_n)[i], as.integer(top_n[i])))
  }
} else {
  cat("No frequent collaborators identified in top tracks.\n")
}
