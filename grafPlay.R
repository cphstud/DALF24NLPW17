library(igraph)

# Create a graph with vertices and edges
vertices <- data.frame(
  name = c("Alice", "Bob", "Carol", "Dave"),
  age = c(25, 30, 35, 40),
  gender = c("F", "M", "F", "M")
)

edges <- data.frame(
  from = c("Alice", "Bob", "Carol", "Dave", "Alice"),
  to = c("Bob", "Carol", "Dave", "Alice", "Carol"),
  weight = c(1, 2, 3, 4, 5),
  relationship = c("friend", "colleague", "neighbor", "classmate", "partner")
)

# Create the graph
g <- graph_from_data_frame(d = edges, vertices = vertices, directed = TRUE)

# Inspect the graph
plot(g)
g


# 
gdat=read_csv("data/lesmis-el.csv")
g <- graph_from_data_frame(d = gdat, directed = TRUE)
plot(g)
