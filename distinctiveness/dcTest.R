setwd("C:/Users/IV/Desktop/UniPG/distinctiveness-R/distinctiveness")
source("dc.R")
# getwd()


# 
# g <- graph_from_literal(
#   Andre ---- Beverly:Diane:Fernando:Carol,
#   Beverly -- Andre:Diane:Garth:Ed,
#   Carol ---- Andre:Diane:Fernando,
#   Diane ---- Andre:Carol:Fernando:Garth:Ed:Beverly,
#   Ed ------- Beverly:Diane:Garth,
#   Fernando - Carol:Andre:Diane:Garth:Heather,
#   Garth ---- Ed:Beverly:Diane:Fernando:Heather,
#   Heather -- Fernando:Garth:Ike,
#   Ike ------ Heather:Jane,
#   Jane ----- Ike
# )
# 
# ### Hand-drawn coordinates
# coords <- c(
#   5, 5, 119, 256, 119, 256, 120, 340, 478,
#   622, 116, 330, 231, 116, 5, 330, 451, 231, 231, 231
# )
# coords <- matrix(coords, ncol = 2)
# 
# 
# ### Labels the same as names
# V(g)$label <- V(g)$name
# g$layout <- coords # $
# 
# 
# ### Take a look at it
# plotG <- function(g) {
#   plot(g,
#        asp = FALSE, vertex.label.color = "blue", vertex.label.cex = 1.5,
#        vertex.label.font = 2, vertex.size = 25, vertex.color = "white",
#        vertex.frame.color = "white", edge.color = "darkgray"
#   )
# }
# plotG(g)
# 
# 
# 
# G.add_edge("A","E", weight = 5)
# G.add_edge("A","B", weight = 2)
# G.add_edge("B","F", weight = 5)
# G.add_edge("B","C", weight = 2)
# G.add_edge("B","D", weight = 2)
# G.add_edge("C","D", weight = 5)

# library('magrittr')

g <- make_empty_graph(directed = FALSE) %>%
  add_vertices(1, name = "A") %>%
  add_vertices(1, name = "B") %>%
  add_vertices(1, name = "C") %>%
  add_vertices(1, name = "D") %>%
  add_vertices(1, name = "E") %>%
  add_vertices(1, name = "F")
g <- g + edge(1, 5, weight = 5) + edge(1, 2, weight = 2) + 
     edge(2, 6, weight = 5) + edge(2, 3, weight = 2) + 
     edge(2, 4, weight = 2) + edge(3, 4, weight = 5)
plot(g, edge.width = E(g)$weight)

distinctiveness(g)

adj <- get.adjacency(g)
adj
for (rows in 1:nrow(adj)) {
  for (j in 1:ncol(adj)) {
    print(adj[i, j])
  }
}
E(g)[4]

weisumalpha(g, 2)
weisumalpha(g, 1)
total_wei(g)

getwd()
g <- read_graph("../resources/Undirected.net", format = "pajek")

trace(distinctiveness)
trace(max_weight)
trace(g_preprocess)
trace(calculate_d1)
trace(copy_graph)
trace(simplify)
trace(check_weights)
trace(total_wei)
distinctiveness(g, measures = c("D1"))

check_weights(g)

g <- simplify(g)
calculate_d1(g, 1, gorder(g))
calculate_d2(g, 1, gorder(g))
calculate_d3(g, 1, gorder(g))
calculate_d4(g, 1, gorder(g))
calculate_d5(g, 1, gorder(g))

edges <- cbind( get.edgelist(g, names = FALSE) , round( E(g)$weight, 3 ))
edges
edges[2, 0]
edges[2, 1]
edges[2, 2]
edges[2, 3]
for (edge in edges) {
  print(edge[2])
  break
}

total_wei(g)
g_prime <- copy_graph(g)
plot(g_prime, edge.width = E(g)$weight)

g_dir <- make_empty_graph(directed = TRUE) %>%
  add_vertices(1) %>%
  add_vertices(1, name = "B") %>%
  add_vertices(1, name = "C") %>%
  add_vertices(1, name = "D") %>%
  add_vertices(1, name = "E") %>%
  add_vertices(1, name = "F")
g_dir <- g_dir + edge(1, 5, weight = 5) + edge(1, 2, weight = 6) +
  edge(2, 1, weight = 2) + edge(2, 6, weight = 5) + edge(2, 3, weight = 2) +
  edge(2, 4, weight = 2) + edge(3, 4, weight = 3) + edge(4, 3, weight = 5)
plot(g_dir, edge.width = E(g_dir)$weight)

unlist(V(g_dir)[1]$name)

v_names(g_dir)

edges <- cbind( get.edgelist(g_dir, names = FALSE) , E(g)$weight )
edges
edges[1, 2]

distinctiveness(g_dir, alpha = 2, normalize = FALSE)

g_ex_1 <- erdos.renyi.game(1000, .1, "gnm")
E(g_ex_1)$weight <- runif(length(E(g_ex_1)), 1, 5)
plot(g_ex_1)
write_graph(g_ex_1, "ex1", "graphml")
distinctiveness(g_ex_1)







g_dups <- make_empty_graph(directed = FALSE) %>%
  add_vertices(1, name = "A") %>%
  add_vertices(1, name = "B") %>%
  add_vertices(1, name = "C") %>%
  add_vertices(1, name = "D") %>%
  add_vertices(1, name = "E") %>%
  add_vertices(1, name = "F")
g_dups <- g_dups + edge(1, 5, weight = 5) + edge(1, 2, weight = 2) + 
  edge(2, 6, weight = 5) + edge(2, 3, weight = 2) + 
  edge(2, 4, weight = 2) + edge(3, 4, weight = 5) + 
  edge(2, 4, weight = 3) + edge(3, 4, weight = 5)
plot(g_dups, edge.width = E(g_dups)$weight)
distinctiveness(g_dups)
