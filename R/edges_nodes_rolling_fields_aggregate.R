library(tidyverse)
library(igraph)
library(biblionetwork)
library(RColorBrewer)
library(jsonlite)



normalize_size <- function(x, min_size = 5, max_size = 30) {
  x <- x - min(x, na.rm = TRUE)
  x <- x / max(x, na.rm = TRUE)
  x * (max_size - min_size) + min_size
}

#####################Force Atlas 2 function ########################################################

layout.forceatlas2 <- function(g, iterations = 100, linlog = FALSE, pos = NULL, nohubs = FALSE, 
                               k = 400, gravity=1, ks=0.1, ksmax=10, delta = 1, center=NULL,
                               tolerate = 0.1, dim = 2, plotstep=10, plotlabels=FALSE){ 
  ####  g is a igraph network
  ####  iterations is the number of iterations to be performed
  ####  linlog is a variant which uses logarithmic attraction force F <- log (1+F)
  ####  pos is the table (NumberOfNodes x dimension) of the initial locations of points, if specified
  ####  nohubs is a variant in which nodes with high indegree have more central position than nodes with outdegree (for directed graphs) 
  ####  k is the repel constant : the greater the constant k the stronger the repulse force between points 
  ####  gravity is the gravity constant : indicates how strongly the nodes should be attracted to the center of gravity
  ####  ks is the speed constant : the greater the value of ks the more movement the nodes make under the acting forces
  ####  ksmax limits the speed from above
  ####  delta is the parameter to modify attraction force; means that weights are raised to the power = delta
  ####  center is the center of gravity  
  ####  tolerance is the tolerance to swinging constant
  ####  dim is the dimension
  ####  plotstep is the frequency of plotting intermediate iterations
  ####  plotlabels is TRUE if the labels should be included in the intermediate interations plot
  
  A <- igraph::get.adjacency(g, type="both",
                             attr=NULL, edges=FALSE, names=TRUE,
                             sparse=FALSE)
  
  #### center of gravity is by default set to the origin
  if(is.null(center)) center <- rep(0,dim)
  
  nnodes <- nrow(A)
  #### Binary will be a matrix of simple incidence (0-not connected, 1-connected)
  Binary <- A
  Binary[Binary!=0] <- 1
  #### Deg will be a vector of the degrees of vertices
  Deg <- rowSums(Binary)
  #### Forces1 will be a table containing all the sums of forces acting on points at a step
  Forces1 <- matrix(0, nrow = dim, ncol = nnodes)
  
  #### If there are no initial coordinates of points, 
  #### they are chosen at random from 1000^dim square
  if (is.null(pos))  {
    difference <- 2000/(nnodes*dim) 
    position <- matrix(sample(seq(-1000,1000,difference),nnodes*dim),nnodes,dim)
  }  else  {
    position <- pos
  }
  
  #### None of the nodes should be exactly at the center of gravity###
  temp <- which(position[,1] == center[1])
  for( index in 2:ncol(position)){
    temp <- intersect(temp,which(position[,index] == center[index]))
  }
  position[temp,] <- center + 0.01
  rm(index,temp)
  
  #### displacement will be a matrix of points' movement at the current iteration
  displacement <- matrix(rep(0,dim*nnodes),dim,nnodes)
  
  m <- nrow(position) 
  
  for (iteration in 1:iterations)
  {
    displacement <- displacement * 0
    #### Forces2 is the table of the forces from previous step
    #### Forces1 is the table of the forces from current step
    Forces2 <- Forces1 
    Forces1 <- matrix(, nrow = dim, ncol = 0)
    
    #### Calculate the Forces for each node
    ### Distance matrix between all nodes
    distances <- as.matrix(dist(position))
    distances[which(distances < 0.01)] <- 0.01 #We impose a minimum distance
    ### Each element of the list contains a matrix with the j = 1,2,..., dim dimension of the unitary vector 1    
    mylist <- vector("list",dim)
    for (j in 1:dim){
      mylist[[j]] <- (tcrossprod(position[,j],rep(1,m))-tcrossprod(rep(1,m),position[,j]))/distances  
    }
    ### Calculate the repulsion Force   
    Fr <- k*((tcrossprod(rep(1,m),Deg)+1)*(tcrossprod(Deg,rep(1,m))+1))/distances
    
    #The classical attraction force is just based on distance
    Fa <- distances   
    #The linlog mode calculates the attraction force as log(1+d(n1,n2))
    if(linlog){
      Fa <- log(1+Fa)
    }
    #Edge weights. The edges are weighted based on parameter delta. delta=0 implies no weight
    Fa <- (A^delta)*Fa
    
    #Dissuade Hubs. This mode is meant to grant authorities (nodes with high indegree)
    #a more central position than hubs (nodes with high outdegree)
    if(nohubs){
      Fa <- Fa/(tcrossprod(Deg,rep(1,m))+1)
    }
    
    ### Function to calculate the Attraction and Repulsion forces
    Farfunction <- function(x) rowSums(x*(Fr-Fa),na.rm=T)
    ### And we aggregate it over all dimensions
    Far <- do.call(rbind,lapply(mylist,Farfunction))
    ### Unitary Vector 2, the directions between each point and the center
    uv2 <- apply(matrix(rep(center,m),nrow=m,byrow=T)-position,1,function(x) x/sqrt(sum(x^2)))
    ### The gravity force
    #Fg <- uv2*matrix(rep(gravity*(rowSums(A)+1),dim),nrow=dim,byrow=T)
    Fg <- uv2*matrix(rep(gravity*(Deg+1),dim),nrow=dim,byrow=T)
    ### Forces 1 is the sum between all forces: Far (Fa + Fr) and Fg
    Forces1 <- Far+Fg
    Forces1 <- round(Forces1,2) #Use the first two decimals for the Forces.
    
    #### Swing is the vector of the swingings of all points
    swing <- abs(colSums((Forces1-Forces2)^2)^(1/2))
    Global_swing <- sum((Deg + 1)*swing)
    
    #### tra is the vector of the traction of all points
    tra <- abs(colSums((Forces1+Forces2)^2)^(1/2))/2
    Global_tra <- sum((Deg+1)*tra)
    
    #### Global speed calculation
    Global_speed <- tolerate * Global_tra/Global_swing
    #### speed is the vector of individual speeds of points
    speed <- ks * Global_speed /  (1 + Global_speed * (swing)^(1/2))
    
    #### Imposing constrains on speed
    speed_constrain <- ksmax/abs(colSums((Forces1^2))^(1/2))
    speed <- ifelse(speed>=speed_constrain,speed_constrain,speed)
    
    #### calculating displacement and final position of points after iteration
    displacement <- Forces1 * t(matrix(rep(speed,dim),nnodes,dim))
    position <- position + t(displacement)
    
    #### Iteration plot. This is simply to see the evolution of the positions over iterations
    #### Is much faster to visualize directly using R base plots instead of igraph plots
    
    if(!plotstep==0&dim==2){
      if(iteration%%plotstep==0)  {
        plot(position, main=paste0("iteration: ",iteration), xlab="", ylab="")
        if(plotlabels) text(position, labels=V(g)$name, cex= 0.7, pos=3)
      }
    }
  }
  return (position)  
}
normalize <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(0, length(x)))
  (x - mean(rng)) / (diff(rng) / 2)
}


data <- readRDS("data_final_1970.RDS")
years <- sort(unique(data$publication_year))
windows <- tibble(
  start = years[1]:(max(years) - 4),
  end   = (years[1] + 4):max(years)
)

# --- Stable color mapping across all windows ---
all_fields <- sort(unique(data$field_1))
n_fields <- length(all_fields)

# Base palette
base_palette <- brewer.pal(12, "Set3")
if (n_fields > length(base_palette)) {
  extra <- colorRampPalette(brewer.pal(8, "Dark2"))(n_fields - length(base_palette))
  global_palette <- c(base_palette, extra)
} else {
  global_palette <- base_palette[1:n_fields]
}

# Field -> color mapping
field_colors <- setNames(global_palette, all_fields)

# --- Process each rolling window ---
for (i in seq_len(nrow(windows))) {
  
  w <- windows[i, ]
  
  data_bucket <- data |> 
    filter(publication_year >= w$start, publication_year <= w$end) |> 
    drop_na(field_1)

  
  
  # --- Edges and Nodes ---
  bib_tbl <- data_bucket %>% 
    select(oa_id, referenced_works) %>% 
    unnest_longer(referenced_works)
  
  coupling_tbl <- biblio_coupling(
    bib_tbl, source = "oa_id", ref = "referenced_works", weight_threshold = 3
  )
  
  edges <- coupling_tbl %>% select(from, to, weight)
  
  nodes <- tibble(oa_id = unique(c(edges$from, edges$to))) %>%
    left_join(
      data_bucket %>% 
        select(oa_id, title, publication_year, cited_by_count, domain_1, field_1, authorships) %>% 
        unnest(authorships, keep_empty = TRUE) %>% 
        group_by(oa_id, title, publication_year, cited_by_count, domain_1, field_1) %>% 
        summarise(authors = paste(display_name, collapse = ";"), .groups = "drop"),
      by = "oa_id"
    ) |> 
    mutate(window = paste0(min(publication_year), "-", max(publication_year)))
  
  
  n_nodes <- nodes |> 
    group_by(field_1) |> 
    summarise(N_nodes = n())
  # --- Field-level edge counts ---
  from_to_field <- edges |> 
    left_join(nodes |> select(oa_id, field_1), by = c("from" = "oa_id")) |> 
    rename(field_from = field_1) |> 
    left_join(nodes |> select(oa_id, field_1), by = c("to" = "oa_id")) |> 
    rename(field_to = field_1) |> 
    # make field-pairs undirected
    mutate(
      field_min = pmin(field_from, field_to),
      field_max = pmax(field_from, field_to)
    ) |> 
    group_by(field_min, field_max) |> 
    summarise(N_edges = n(), .groups = "drop")
  
  # --- Within-field vs between-field edges ---
  within_edges_df <- from_to_field |> 
    filter(field_min == field_max) |> 
    transmute(field_1 = field_min, within_edges = N_edges)
  
  between_edges_df <- from_to_field |> 
    filter(field_min != field_max) |> 
    # duplicate edges so both fields get counted
    tidyr::pivot_longer(cols = c(field_min, field_max), names_to = NULL, values_to = "field_1") |> 
    group_by(field_1) |> 
    summarise(between_edges = sum(N_edges), .groups = "drop")
  
  
  # --- Final summary ---
  between_within_df <- n_nodes |> 
    full_join(within_edges_df, by = "field_1") |> 
    full_join(between_edges_df, by = "field_1") |> 
    mutate(
      across(c(within_edges, between_edges), ~replace_na(.x, 0)),
      total_edges = within_edges + between_edges,
      external_ratio = between_edges / total_edges,
      internal_ratio = within_edges / total_edges
    )
  
  
  
  g <- graph_from_data_frame(edges, vertices = nodes, directed = FALSE)
  
  field_membership <- as.numeric(factor(V(g)$field_1))
  g_field <- contract(g, mapping =field_membership, 
                      vertex.attr.comb = list(field_1 = unique,
                                              cited_by_count = sum,
                                              window = unique))
  V(g_field)$name <- V(g_field)$field_1
  g_field <- simplify(g_field, remove.loops = T, edge.attr.comb = "sum")

  
  
  V(g_field)$size <- as.numeric(V(g_field)$cited_by_count)
  V(g_field)$color <- field_colors[V(g_field)$field_1]
  V(g_field)$label <- V(g_field)$field_1
  
  g_field <- delete_vertex_attr(g_field, name = "title")
  g_field <- delete_vertex_attr(g_field, name = "publication_year")
  g_field <- delete_vertex_attr(g_field, name = "domain_1")
  g_field <- delete_vertex_attr(g_field, name = "authors")
  
  V(g_field)$N_nodes <- n_nodes$N_nodes
  
  E(g_field)$dist <- 1 / E(g_field)$weight

  V(g_field)$degree <- degree(g_field)
  V(g_field)$betweeness <- betweenness(g_field, weights = E(g_field)$dist)
  V(g_field)$closeness <- closeness(g_field, weights = E(g_field)$dist)
  
  
  V(g_field)$between_edges <- between_within_df$between_edges
  V(g_field)$within_edges <- between_within_df$within_edges
  V(g_field)$total_edges <- between_within_df$total_edges
  V(g_field)$external_ratio <- between_within_df$external_ratio
  V(g_field)$internal_ratio <- between_within_df$internal_ratio
  
  ### NEW: Compute ForceAtlas2-like coordinates (FR layout in igraph)
  coords <- layout.forceatlas2(
    g_field,
    iterations = 500,
    k = 1000,
    gravity = 0.5,
    ks = 0.05,
    delta = 1,
    plotstep = 0
  )  
  
  coords_df <- tibble(
    x = normalize(coords[,1]),
    y = normalize(coords[,2])
  )  
  # Normalize to [-1, 1]
  
  
  
  # --- Prepare JSON output ---
  nodes_json <- tibble(
    id = V(g_field)$name,
    label = V(g_field)$label,
    size_citations = normalize_size(V(g_field)$cited_by_count),
    size_nodes = normalize_size(V(g_field)$N_nodes),
    color = V(g_field)$color,
    citations = V(g_field)$cited_by_count, 
    field = V(g_field)$field_1,
    numbernodes = V(g_field)$N_nodes,
    degree = V(g_field)$degree,
    betweeness = V(g_field)$betweeness,
    closeness = V(g_field)$closeness,
    between_edges = V(g_field)$between_edges,
    withinedges = V(g_field)$within_edges,
    totaledges = V(g_field)$total_edges,
    externalratio = V(g_field)$external_ratio,
    internalratio = V(g_field)$internal_ratio,
    window = V(g_field)$window,
    stringsAsFactors = FALSE
  ) %>% bind_cols(coords_df)   ### NEW
  
  edges_json <- tibble(
    id = paste0("e", seq_len(ecount(g_field))),
    source = as.character(ends(g_field, E(g_field))[,1]),
    target = as.character(ends(g_field, E(g_field))[,2]),
    weight = E(g_field)$weight,
    stringsAsFactors = FALSE
  )
  
  # --- Write JSON to disk ---
  write(
    toJSON(nodes_json, dataframe = "rows", auto_unbox = TRUE, pretty = TRUE),
    file = sprintf("sigma_network_aggregate/rolling_network/public/windows_field_aggregate/nodes_window_%03d.json", i)
  )
  
  write(
    toJSON(edges_json, dataframe = "rows", auto_unbox = TRUE, pretty = TRUE),
    file = sprintf("sigma_network_aggregate/rolling_network/public/windows_field_aggregate/edges_window_%03d.json", i)
  )
  
  saveRDS(nodes_json,   file = paste0("Evolution Fields/", w$start, "-", w$end, "_metrics.RDS"))
  
  message("Saved window ", i, " (", w$start, "-", w$end, ") with ", vcount(g_field), " nodes and ", ecount(g_field), " edges.")
}
