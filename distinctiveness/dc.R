if(!require('igraph')) {
  install.packages('igraph')
  library('igraph')
}


weisumalpha <- function(G, a) {
  wsalength <- gorder(G)
  wei_sum_alpha <- numeric(wsalength)
  if (a != 1) {
    adj <- get.adjacency(G)
    for (i in 1:wsalength) {
      for (j in 1:wsalength) {
        if(adj[i, j] == 1) {
          wei_sum_alpha[i] <- wei_sum_alpha[i] + 
            E(g)[get.edge.ids(G, c(i, j))]$weight ** a
        }
      }
    }
  }
  else {
    for (node in 1:wsalength) {
      wei_sum_alpha[node] <- strength(G, vids = node)
    }
  }
  return(wei_sum_alpha)
}

weiinoutsumalpha <- function(G, a) {
  wsalength <- gorder(G)
  wei_outsum_alpha <- numeric(wsalength)
  wei_insum_alpha <- numeric(wsalength)
  if (a != 1) {
    adj <- get.adjacency(G)
    for (i in 1:nrow(adj)) {
      for (j in 1:ncol(adj)) {
        if(adj[i, j] == 1) {
          curr_weight = E(G)[get.edge.ids(G, c(i, j))]$weight
          wei_outsum_alpha[i] <- wei_outsum_alpha[i] + 
            (curr_weight) ** a
          wei_insum_alpha[j] <- wei_insum_alpha[j] + 
            (curr_weight) ** a
        }
      }
    }
  }
  else {
    for (node in 1:wsalength) {
      wei_outsum_alpha[node] <- strength(G, vids = node, mode = "out")
      wei_insum_alpha[node] <- strength(G, vids = node, mode = "in")
    }
  }
  # python version returns both lists rather than the two combined into one list
  wei_sums <- list("insum" = wei_insum_alpha, "outsum" = wei_outsum_alpha)
  return(wei_sums)
  #wei_sums$insum = in sum, wei_sums$outsum = out sum
}

total_wei <- function(G) {
  n <- gorder(G)
  total <- 0
  for (i in 1:n) {
    total <- total + strength(G, vids = i, mode = "all")
  }
  return(total/2)
}

copy_graph <- function(G) {
  n <- gorder(G)
  return(induced_subgraph(G, 1:n))
}

v_names <- function(g) {
  n <- gorder(g)
  cee <- c()
  vs <- V(g)
  for (i in 1:n) {
    cee <- append(cee, unlist(vs[i]$name))
  }
  return(cee)
}

dc_frame <- function(dc, g) {
  return(
    data.frame(
      Node = v_names(g), 
      D1 = unlist(dc$D1), 
      D2 = unlist(dc$D2),
      D3 = unlist(dc$D3),
      D4 = unlist(dc$D4),
      D5 = unlist(dc$D5)
    )
  )
}

dc_frame_dir <- function(dc, g) {
  return(
    data.frame(
    Node = v_names(g), 
    D1_IN = unlist(dc$D1_IN), 
    D2_IN = unlist(dc$D2_IN),
    D3_IN = unlist(dc$D3_IN),
    D4_IN = unlist(dc$D4_IN),
    D5_IN = unlist(dc$D5_IN),
    D1_OUT = unlist(dc$D1_OUT),
    D2_OUT = unlist(dc$D2_OUT),
    D3_OUT = unlist(dc$D3_OUT),
    D4_OUT = unlist(dc$D4_OUT),
    D5_OUT = unlist(dc$D5_OUT)
    )
  )
}

process_alpha <- function(alpha) {
  if (is.list(alpha) && length(alpha) == 5) {
    for (i in alpha) {
      if (! (is.numeric(i) || is.integer(i))) {
        # not valid when one of the values in alphalist is not a number
        return(NaN)
        print("one alpha value not a number")
      }
    }
    return(alpha)
  }
  else if (is.numeric(alpha) || is.integer(alpha)) {
    return(list(alpha, alpha, alpha, alpha, alpha))
  }
  else {
    return(NaN)
  }
}

check_weights <- function(G) {
  n <- gorder(G)
  adj <- get.adjacency(G)
  for (i in 1:nrow(adj)) {
    for (j in 1:ncol(adj)) {
      if(adj[i, j] == 1) {
        if(E(G)[get.edge.ids(G, c(i, j))]$weight < 1) {
          return(TRUE)
        }
      }
    }
  }
  return(FALSE)
}

calculate_d1 <- function(G, alpha, n) {
  n1 <- n - 1
  d1 <- numeric(n)
  adj <- get.adjacency(G)
  for (i in 1:nrow(adj)) {
    for (j in 1:ncol(adj)) {
      if(adj[i, j] == 1) {
        w <- E(G)[get.edge.ids(G, c(i, j))]$weight
        deg_i <- degree(G, v = i)
        deg_j <- degree(G, v = j)
        d1[i] <- d1[i] + w * log10(n1 / deg_j ** alpha)
        #d1[j] <- d1[j] + w * log10(n1 / deg_i ** alpha)
      }
    }
  }
  return(d1)
}

calculate_d1_directed <- function(G, alpha, n) {
  n1 <- n - 1
  d1_in <- numeric(n)
  d1_out <- numeric(n)
  adj <- get.adjacency(G)
  for (i in 1:nrow(adj)) {
    for (j in 1:ncol(adj)) {
      if(adj[i, j] == 1) {
        w <- E(G)[get.edge.ids(G, c(i, j))]$weight
        outdeg_i <- degree(G, v = i, mode = "out")
        indeg_j <- degree(G, v = j, mode = "in")
        d1_in[j] <- d1_in[j] + w * log10(n1 / outdeg_i ** alpha)
        d1_out[i] <- d1_out[i] + w * log10(n1 / indeg_j ** alpha)
      }
    }
  }
  return(list(d1_in, d1_out))
}

calculate_d2 <- function(G, alpha, n) {
  n1 <- n - 1
  d2 <- numeric(n)
  adj <- get.adjacency(G)
  for (i in 1:nrow(adj)) {
    for (j in 1:ncol(adj)) {
      if(adj[i, j] == 1) {
        #w <- E(G)[get.edge.ids(G, c(i, j))]$weight
        deg_i <- degree(G, v = i)
        deg_j <- degree(G, v = j)
        d2[i] <- d2[i] + log10(n1 / deg_j ** alpha)
        #d1[j] <- d1[j] + log10(n1 / deg_i ** alpha)
      }
    }
  }
  return(d2)
}

calculate_d2_directed <- function(G, alpha, n) {
  n1 <- n - 1
  d2_in <- numeric(n)
  d2_out <- numeric(n)
  adj <- get.adjacency(G)
  for (i in 1:nrow(adj)) {
    for (j in 1:ncol(adj)) {
      if(adj[i, j] == 1) {
        #w <- E(G)[get.edge.ids(G, c(i, j))]$weight
        outdeg_i <- degree(G, v = i, mode = "out")
        indeg_j <- degree(G, v = j, mode = "in")
        d2_in[j] <- d2_in[j] + log10(n1 / outdeg_i ** alpha)
        d2_out[i] <- d2_out[i] + log10(n1 / indeg_j ** alpha)
        
      }
    }
  }
  return(list(d2_in, d2_out))
}

calculate_d3 <- function(G, alpha, n) {
  total <- total_wei(G)
  wsa <- weisumalpha(G, alpha)
  n1 <- n - 1
  d3 <- numeric(n)
  adj <- get.adjacency(G)
  for (i in 1:nrow(adj)) {
    for (j in 1:ncol(adj)) {
      if(adj[i, j] == 1) {
        w <- E(G)[get.edge.ids(G, c(i, j))]$weight
        #deg_i <- degree(G, v = i)
        deg_j <- degree(G, v = j)
        d3[i] <- d3[i] + w * log10(
          total
          / (wsa[j] - w ** alpha + 1)
        )
        #d3[j] <- d3[j] + w * log10(n1 / deg_i ** alpha)
      }
    }
  }
  return(d3)
}

calculate_d3_directed <- function(G, alpha, n) {
  total <- total_wei(G)
  wsaio <- weiinoutsumalpha(G, alpha)
  wsa_in <- wsaio$insum
  wsa_out <- wsaio$outsum
  n1 <- n - 1
  d3_in <- numeric(n)
  d3_out <- numeric(n)
  adj <- get.adjacency(G)
  for (i in 1:nrow(adj)) {
    for (j in 1:ncol(adj)) {
      if(adj[i, j] == 1) {
        w <- E(G)[get.edge.ids(G, c(i, j))]$weight
        # outdeg_i <- degree(G, v = i, mode = "out")
        # indeg_j <- degree(G, v = j, mode = "in")
        # d3_in[j] <- d3_in[j] + w * log10(n1 / deg_i ** alpha)
        d3_in[j] <- d3_in[j] + w * log10(
          total
          / (wsa_out[i] - w ** alpha + 1)
        )
        d3_out[i] <- d3_out[i] + w * log10(
          total
          / (wsa_in[j] - w ** alpha + 1)
        )
      }
    }
  }
  return(list(d3_in, d3_out))
}

calculate_d4 <- function(G, alpha, n) {
  #total <- total_wei(G)
  wsa <- weisumalpha(G, alpha)
  n1 <- n - 1
  d4 <- numeric(n)
  adj <- get.adjacency(G)
  for (i in 1:nrow(adj)) {
    for (j in 1:ncol(adj)) {
      if(adj[i, j] == 1) {
        w <- E(G)[get.edge.ids(G, c(i, j))]$weight
        #deg_i <- degree(G, v = i)
        deg_j <- degree(G, v = j)
        d4[i] <- d4[i] + w * (
          w ** alpha / wsa[j]
        )
        #d4[j] <- d4[j] + w * log10(n1 / deg_i ** alpha)
      }
    }
  }
  return(d4)
}

calculate_d4_directed <- function(G, alpha, n) {
  wsaio <- weiinoutsumalpha(G, alpha)
  wsa_in <- wsaio$insum
  wsa_out <- wsaio$outsum
  n1 <- n - 1
  d4_in <- numeric(n)
  d4_out <- numeric(n)
  adj <- get.adjacency(G)
  for (i in 1:nrow(adj)) {
    for (j in 1:ncol(adj)) {
      if(adj[i, j] == 1) {
        w <- E(G)[get.edge.ids(G, c(i, j))]$weight
        d4_in[j] <- d4_in[j] + w * (
          w ** alpha / wsa_out[i]
        )
        d4_out[i] <- d4_out[i] + w * (
          w ** alpha / wsa_in[j]
        )
      }
    }
  }
  return(list(d4_in, d4_out))
}

calculate_d5 <- function(G, alpha, n) {
  total <- total_wei(G)
  wsa <- weisumalpha(G, alpha)
  n1 <- n - 1
  d5 <- numeric(n)
  adj <- get.adjacency(G)
  for (i in 1:nrow(adj)) {
    for (j in 1:ncol(adj)) {
      if(adj[i, j] == 1) {
        w <- E(G)[get.edge.ids(G, c(i, j))]$weight
        #deg_i <- degree(G, v = i)
        deg_j <- degree(G, v = j)
        d5[i] <- d5[i] + (1 / deg_j ** alpha)
        #d1[j] <- d1[j] + w * log10(n1 / deg_i ** alpha)
      }
    }
  }
  return(d5)
}

calculate_d5_directed <- function(G, alpha, n) {
  n1 <- n - 1
  d5_in <- numeric(n)
  d5_out <- numeric(n)
  adj <- get.adjacency(G)
  for (i in 1:nrow(adj)) {
    for (j in 1:ncol(adj)) {
      if(adj[i, j] == 1) {
        w <- E(G)[get.edge.ids(G, c(i, j))]$weight
        outdeg_i <- degree(G, v = i, mode = "out")
        indeg_j <- degree(G, v = j, mode = "in")
        d5_in[j] <- d5_in[j] + (1 / outdeg_i ** alpha)
        d5_out[i] <- d5_out[i] + (1 / indeg_j ** alpha)
      }
    }
  }
  return(list(d5_in, d5_out))
}

max_weight <- function(G, n) {
  max <- 0
  adj <- get.adjacency(G)
  for (i in 1:nrow(adj)) {
    for (j in 1:ncol(adj)) {
      if(adj[i, j] == 1) {
        w <- E(G)[get.edge.ids(G, c(i, j))]$weight
        if(w > max || is.nan(max)) {
          max <- w
        }
      }
    }
  }
  return(max)
}

min_weight <- function(G, n) {
  min <- NaN
  adj <- get.adjacency(G)
  for (i in 1:nrow(adj)) {
    for (j in 1:ncol(adj)) {
      if(adj[i, j] == 1) {
        w <- E(G)[get.edge.ids(G, c(i, j))]$weight
        if(w < min || is.nan(min)) {
          min <- w
        }
      }
    }
  }
  return(min)
}

g_preprocess <- function(G, alpha = 1, 
                         measures = c("D1", "D2", "D3", "D4", "D5")) {
  alphalist <- process_alpha(alpha)
  if (!is.list(alphalist)) {
    print("For alpha, please input a single number or a list of five numbers.")
    return(list(NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN))
  }
  
  G <- copy_graph(G)
  n <- gorder(G)
  n1 <- n - 1

  if (n1 < 2) {
    print("Input graph must have at least three nodes.")
    return(list(NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN))
  }
  
  # simplify, sum multiple edges
  if (!(is_simple(G))) {
    simplify(G, remove.multiple = TRUE, remove.loops = TRUE,
             edge.attr.comb = "sum")
  }
  
  if(check_weights(G)) {
    print("Graph has edges with weight < 1. Edge weights must be >= 1.")
    return(list(NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN))
  }
  
  totalWEI <- total_wei(G)
  
  if (! (is.directed(G))) {
    if ("D1" %in% measures || "D2" %in% measures || "D5" %in% measures) {
      deg <- degree(G, v = 1:n)
    }
    else {
      deg <- NaN
    }
    indeg <- outdeg <- NaN
  }
  
  else {
    deg <- NaN
    if ("D1" %in% measures || "D2" %in% measures || "D5" %in% measures) {
      indeg <- degree(G, v = 1:n, mode = "IN")
      outdeg <- degree(G, v = 1:n, mode = "OUT")
    }
    else {
      indeg <- outdeg <- NaN
    }
  }
  
  if (gsize(G) > 0) {
    hasedges <- TRUE
    if ("D1" %in% measures || "D3" %in% measures || "D4" %in% measures) {
      maxwij <- max_weight(G, n)
    }
    else {
      maxwij <- NaN
    }
    if ("D3" %in% measures) {
      minwij <- min_weight(G, n)
    }
    else {
      minwij <- NaN
    }
  }
  
  else {
    print("The graph has no edges (after simplification). The function will
          return all zeroes, regardless of normalization.")
    hasedges <- FALSE
    maxwij <- NaN
    minwij <- NaN
  }
  
  return(list(
    "G" = G,
    "n1" = n1,
    "deg" = deg,
    "indeg" = indeg,
    "outdeg" = outdeg,
    # wei_insum_alpha_list,
    # wei_outsum_alpha_list,
    # wei_sum_alpha_list,
    "total" = totalWEI,
    "maxwij" = maxwij,
    "minwij" = minwij,
    "hasedges" = hasedges,
    "alphalist" = alphalist
  ))
  
}

distinctiveness <- function(G, alpha = 1, normalize = FALSE,
                            measures = c("D1", "D2", "D3", "D4", "D5")) {
  # ( G,
  #   n1,
  #   deg,
  #   indeg,
  #   outdeg,
  #   wei_insum_alpha_list,
  #   wei_outsum_alpha_list,
  #   wei_sum_alpha_list,
  #   totalWEI,
  #   maxwij,
  #   minwij,
  #   hasedges,
  # ) <- g_preprocess(G, alpha=alpha, measures=measures)
  
  pre <- g_preprocess(G, alpha=alpha, measures=measures)
  G <- pre$G
  n1 <- pre$n1
  deg <- pre$deg
  indeg <- pre$indeg
  outdeg <- pre$outdeg
  # wsa_in <- pre[6]
  # wsa_out <- pre[7]
  # wsa <- pre[8]
  total <- pre$total
  maxwij <- pre$maxwij
  minwij <- pre$minwij
  hasedges <- pre$hasedges
  alphalist <- pre$alphalist
  n <- n1 + 1
  
  for (a in alphalist) {
    if (unlist(a) < 1) {
      print("Alpha should be >= 1.")
      if (normalize) {
        print("Normalization deactivated for all metrics.")
        normalize <- FALSE
      }
    }
  }
  
  if (! hasedges) {
    normalize <- FALSE
  }
  
  if (normalize) {
    if ("D1" %in% measures) {
      d1_max <- log10(n1) * n1 * maxwij
      d1_min <- (1 - unlist(alphalist[1])) * maxwij * log10(n1) * n1
    }
    if("D2" %in% measures) {
      d2_max <- log10(n1) * n1
      d2_min <- (1 - unlist(alphalist[2])) * log10(n1) * n1
    }
    if("D3" %in% measures) {
      if (! (is.directed(G))) {
        d3_max <- log10(maxwij * (n) * n1 * 0.5) * maxwij * n1
      }
      else {
        d3_max <- log10(maxwij * (n) * n1) * maxwij * n1
      }
      threshold <- (n1 - 1) * (maxwij ** unlist(alphalist[3]) - maxwij)
      if ((minwij - 1) > threshold) {
        d3_min <- 0 # considers isolates
      }
      else {
        d3_min <- (
          n1
          * maxwij
          * log10(
            ((n1 - 1) * maxwij + minwij)
            / ((n1 - 1) * (maxwij) ** unlist(alphalist[3]) + 1)
          )
        )
      }
    }
    if("D4" %in% measures) {
      d4_max <- n1 * maxwij
      d4_min <- 0 # considers isolates
    }
    if("D5" %in% measures) {
      d5_max <- n1
      d5_min <- 0 # considers isolates
    }
  }
  
  else {
    d1_max <- d2_max <- d3_max <- d4_max <- d5_max <- 1
    d1_min <- d2_min <- d3_min <- d4_min <- d5_min <- 0
  }
  
  if (is_directed(G)) {
    if ("D1" %in% measures) {
      d1 <- calculate_d1_directed(G, unlist(alphalist[1]), n)
      d1_in <- d1[1]
      d1_out <- d1[2]
    }
    else {
      d1_in <- NaN
      d1_out <- NaN
    }
    if("D2" %in% measures) {
      d2 <- calculate_d2_directed(G, unlist(alphalist[2]), n)
      d2_in <- d2[1]
      d2_out <- d2[2]
    }
    else {
      d2_in <- NaN
      d2_out <- NaN
    }
    if("D3" %in% measures) {
      d3 <- calculate_d3_directed(G, unlist(alphalist[3]), n)
      d3_in <- d3[1]
      d3_out <- d3[2]
    }
    else {
      d3_in <- NaN
      d3_out <- NaN
    }
    if("D4" %in% measures) {
      d4 <- calculate_d4_directed(G, unlist(alphalist[4]), n)
      d4_in <- d4[1]
      d4_out <- d4[2]
    }
    else {
      d4_in <- NaN
      d4_out <- NaN
    }
    if("D5" %in% measures) {
      d5 <- calculate_d5_directed(G, unlist(alphalist[5]), n)
      d5_in <- d5[1]
      d5_out <- d5[2]
    }
    else {
      d5_in <- NaN
      d5_out <- NaN
    }
    d1 <- d2 <- d3 <- d4 <- d5 <- NaN
    if(normalize) {
      if ("D1" %in% measures) {
        d1_in_unlist <- unlist(d1_in)
        d1_out_unlist <- unlist(d1_out)
        for(i in 1:n) {
          d1_in[i] <- ((d1_in_unlist[i] - d1_min) / (d1_max - d1_min))
          d1_out[i] <- ((d1_out_unlist[i] - d1_min) / (d1_max - d1_min))
        }
      }
      if("D2" %in% measures) {
        d2_in_unlist <- unlist(d2_in)
        d2_out_unlist <- unlist(d2_out)
        for(i in 1:n) {
          d2_in[i] <- ((d2_in_unlist[i] - d2_min) / (d2_max - d2_min))
          d2_out[i] <- ((d2_out_unlist[i] - d2_min) / (d2_max - d2_min))
        }
      }
      if("D3" %in% measures) {
        d3_in_unlist <- unlist(d3_in)
        d3_out_unlist <- unlist(d3_out)
        for(i in 1:n) {
          d3_in[i] <- ((d3_in_unlist[i] - d3_min) / (d3_max - d3_min))
          d3_out[i] <- ((d3_out_unlist[i] - d3_min) / (d3_max - d3_min))
        }
      }
      if("D4" %in% measures) {
        d4_in_unlist <- unlist(d4_in)
        d4_out_unlist <- unlist(d4_out)
        for(i in 1:n) {
          d4_in[i] <- ((d4_in_unlist[i] - d4_min) / (d4_max - d4_min))
          d4_out[i] <- ((d4_out_unlist[i] - d4_min) / (d4_max - d4_min))
        }
      }
      if("D5" %in% measures) {
        d5_in_unlist <- unlist(d5_in)
        d5_out_unlist <- unlist(d5_out)
        for(i in 1:n) {
          d5_in[i] <- ((d5_in_unlist[i] - d5_min) / (d5_max - d5_min))
          d5_out[i] <- ((d5_out_unlist[i] - d5_min) / (d5_max - d5_min))
        }
      }
    }
    return(dc_frame_dir(list
           ("D1_IN" = d1_in, 
            "D2_IN" = d2_in, 
            "D3_IN" = d3_in,
            "D4_IN" = d4_in, 
            "D5_IN" = d5_in, 
            "D1_OUT" = d1_out, 
            "D2_OUT" = d2_out, 
            "D3_OUT" = d3_out, 
            "D4_OUT" = d4_out, 
            "D5_OUT" = d5_out), G))
  }
  
  else {
    if ("D1" %in% measures) {
      d1 <- calculate_d1(G, unlist(alphalist[1]), n)
    }
    else {
      d1 <- NaN
    }
    if("D2" %in% measures) {
      d2 <- calculate_d2(G, unlist(alphalist[2]), n)
    }
    else {
      d2 <- NaN
    }
    if("D3" %in% measures) {
      d3 <- calculate_d3(G, unlist(alphalist[3]), n)
    }
    else {
      d3 <- NaN
    }
    if("D4" %in% measures) {
      d4 <- calculate_d4(G, unlist(alphalist[4]), n)
    }
    else {
      d4 <- NaN
    }
    if("D5" %in% measures) {
      d5 <- calculate_d5(G, unlist(alphalist[5]), n)
    }
    else {
      d5 <- NaN
    }
    d1_in <- d2_in <- d3_in <- d4_in <- d5_in <- NaN
    d1_out <- d2_out <- d3_out <- d4_out <- d5_out <- NaN
    if(normalize) {
      if ("D1" %in% measures) {
        d1_unlist <- unlist(d1)
        for(i in 1:n) {
          d1[i] <- ((d1_unlist[i] - d1_min) / (d1_max - d1_min))
        }
      }
      if("D2" %in% measures) {
        d2_unlist <- unlist(d2)
        for(i in 1:n) {
          d2[i] <- ((d2_unlist[i] - d2_min) / (d2_max - d2_min))
        }
      }
      if("D3" %in% measures) {
        d3_unlist <- unlist(d3)
        for(i in 1:n) {
          d3[i] <- ((d3_unlist[i] - d3_min) / (d3_max - d3_min))
        }
      }
      if("D4" %in% measures) {
        d4_unlist <- unlist(d4)
        for(i in 1:n) {
          d4[i] <- ((d4_unlist[i] - d4_min) / (d4_max - d4_min))
        }
      }
      if("D5" %in% measures) {
        d5_unlist <- unlist(d5)
        for(i in 1:n) {
          d5[i] <- ((d5_unlist[i] - d5_min) / (d5_max - d5_min))
        }
      }
    }
    return(dc_frame(list(
      "D1" = d1, 
      "D2" = d2,
      "D3" = d3,
      "D4" = d4,
      "D5" = d5), G))
  }
}