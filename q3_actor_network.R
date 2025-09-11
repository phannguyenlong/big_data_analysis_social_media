# ============================================================================
# QUESTION 3: Create actor network
# Create actor networks from your data and list the top 5 most influential actors for your
# artist/band according to page rank. Explain the results
# ============================================================================

# ============================================================================
# CREATE ACTOR NETWORK
# ============================================================================

# Youtube Actor Graph ------------------------------------------------------
# Load YouTube data
yt_data <- readRDS(paste(dataset_dir,"yt_data.rds",sep=""))
# Create actor network 
yt_actor_network <- yt_data |> 
  Create("actor") |> 
  AddText(yt_data,
          repliesFromText = TRUE,
          verbose = TRUE) |> 
  AddVideoData(yt_auth,
               actorSubOnly = TRUE,
               verbose = TRUE)

# Create graph from the network and change IDs to screen names
yt_actor_graph <- yt_actor_network |> Graph()
V(yt_actor_graph)$name <- V(yt_actor_graph)$screen_name
plot(yt_actor_graph)

# Save and write graph to file
saveRDS(yt_actor_graph, file = paste(graph_dir,"YouTubeActor.rds",sep=""))
write_graph(yt_actor_graph, file = paste(graph_dir,"YouTubeActor.graphml",sep=""), format = "graphml")

# Reddit Actor Graph ------------------------------------------------------
# Load Reddit data
rd_data <- readRDS(paste(dataset_dir,"rd_data.rds",sep=""))
# Create actor network 
rd_actor_network <- rd_data |> 
  Create("actor") |> 
  AddText(rd_data,
          verbose = TRUE) 

# Create graph from the network
rd_actor_graph <- rd_actor_network |> Graph()
rd_actor_graph
V(rd_actor_graph)$name <- V(rd_actor_graph)$user
rd_actor_graph
plot(rd_actor_graph)

# Save and write graph to file
saveRDS(rd_actor_graph, file = paste(graph_dir,"RedditActor.rds",sep=""))
write_graph(rd_actor_graph, file = paste(graph_dir,"RedditActor.graphml",sep=""), format = "graphml")

# ============================================================================
# List the top 5 most influential actors for your according to page rank
# ============================================================================
# Youtube Actor Page Rank ------------------------------------------------------
rank_yt_actor <- sort(page_rank(yt_actor_graph)$vector, decreasing=TRUE)
authors <- yt_data$AuthorDisplayName

# display top find 
rank_yt_actor[1:5]

# compare the Page Rank results with the table of users from above
table(authors[duplicated(authors)])

# Reddit Actor Page Rank ------------------------------------------------------
rank_rd_actor <- sort(page_rank(rd_actor_graph)$vector, decreasing=TRUE)
authors <- rd_data$author

# display top find 
rank_rd_actor[1:5]

# compare the Page Rank results with the table of users from above
table(authors[duplicated(authors)])

