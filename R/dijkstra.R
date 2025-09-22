#' Dijkstra Algorithm
#'
#' Calculate the shortest path from an initial node to all other nodes in the graph
#' using Dijkstra's algorithm. The algorithm maintains a set of unvisited nodes and
#' iteratively selects the unvisited node with the smallest distance, updating the
#' distances to its neighbors until all nodes have been processed.
#'
#' @param graph A data.frame describing the graph. Must contain three variables (v1, v2, and w) containing the edges of the graph (from v1 to v2) and the weight of the edge (w)
#' @param init_node A numeric scalar representing the initial node from which to calculate shortest paths
#'
#' @returns A numeric vector of shortest distances from init_node to all other nodes
#' @export
#'
#' @examples
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 3)
#'
#' @references \url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}
dijkstra <- function(graph, init_node) {
  if(!is.data.frame(graph)){
    stop("graph must be a data.frame containing the three variables v1, v2, and w")
  }
  if(!is.numeric(init_node) || length(init_node)!=1 || !(any(graph$v1==init_node)||any(graph$v2==init_node))){
    stop("init node must be a numeric scalar and exist in the graph")
  }

  # create set of unvisited nodes
  Q <- unique(c(graph$v1))
  node_set <- unique(c(graph$v1))
  # assign distances from the start
  distances <- rep(Inf, length(Q))
  # set init node distance to 0
  distances[node_set==init_node] <- 0
  prev <- rep(NA, length(Q))

  while(length(Q)!=0){
    u <- Q[distances==min(distances)]
    if(is.na(u)){
      break
    }

    # truncate to get only one node if multiple have the same min distance
    if(length(u)>1){
      u <- u[1]
    }
    # get all neighbors
    ns <- graph[graph$v1==u,]

    for (i in c(1:nrow(ns))) {
      n <- ns[i,]
      # update distance
      n_dist <- distances[node_set==u] + n$w
      if(n_dist<(distances[node_set==n$v2])){
        distances[node_set==n$v2] <- n_dist
      }
    }
    # remove current node from set
    Q <- Q[Q!=u]
  }
  return(distances)
}
