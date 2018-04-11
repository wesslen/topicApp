new.topic.network <- function(stmFit, threshold, topic.names){
  #mod.out.corr <- topicCorr(stmFit, method = "simple", cutoff = threshold)
  cormat <- cor(stmFit$theta)
  adjmat <- ifelse(abs(cormat) > threshold,1,0)
  
  links2 <- as.matrix(adjmat)
  net2 <- graph_from_adjacency_matrix(links2, mode = "undirected")
  net2 <- igraph::simplify(net2,  remove.multiple = FALSE, remove.loops = TRUE) 

  data <- toVisNetworkData(net2)
  
  nodes <- data[[1]]
  edges <- data[[2]]
  
  # Community Detection
  clp <- cluster_label_prop(net2)
  nodes$community <- clp$membership
  qual_col_pals = RColorBrewer::brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  col_vector <- c(col_vector,col_vector)
  
  col <- col_vector[nodes$community+1]
  
  links <- igraph::as_data_frame(net2, what="edges")
  nodes <- igraph::as_data_frame(net2, what="vertices")

  TopicProportions = colMeans(stmFit$theta)
  
  #visNetwork edits
  nodes$shape <- "dot"  
  nodes$shadow <- TRUE # Nodes will drop shadow
  nodes$title <- topic.names # Text on click
  nodes$label <- topic.names # Node label
  nodes$size <- (TopicProportions / max(TopicProportions)) * 40 # Node size
  nodes$borderWidth <- 2 # Node border width
  
  nodes$color.background <- col
  nodes$color.border <- "black"
  nodes$color.highlight.background <- "orange"
  nodes$color.highlight.border <- "darkred"
  nodes$id <- 1:nrow(nodes)
  
  v <- list(nodes, edges)
  
  return(v)
}
