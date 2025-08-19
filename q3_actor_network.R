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
yt_data <- readRDS("yt_data.rds")
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
saveRDS(yt_actor_graph, file = "YouTubeActor.rds")
write_graph(yt_actor_graph, file = "YouTubeActor.graphml", format = "graphml")

# Reddit Actor Graph ------------------------------------------------------
# Load Reddit data
rd_data <- readRDS("rd_data.rds")
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
saveRDS(rd_actor_graph, file = "RedditActor.rds")
write_graph(rd_actor_graph, file = "RedditActor.graphml", format = "graphml")

# ============================================================================
# List the top 5 most influential actors for your according to page rank
# ============================================================================
