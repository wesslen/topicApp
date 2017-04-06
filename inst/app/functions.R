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

  #means <- as.data.frame(unlist(STMresults$means))
  #colnames(means) <- "means"
  #color <- colorRamp(c("white","blue"))(abs(means$means)/0.05)
  #means$colorDem <- rgb(color[,1],color[,2],color[,3],  maxColorValue=255)
  
  #color <- colorRamp(c("white","red"))(abs(means$means)/0.05)
  #means$colorRep <- rgb(color[,1],color[,2],color[,3],  maxColorValue=255)
  
  #means$color <- ifelse(means$means>0,means$colorDem,means$colorRep)
  
  #visNetwork edits
  nodes$shape <- "dot"  
  nodes$shadow <- TRUE # Nodes will drop shadow
  nodes$title <- topic.names # Text on click
  nodes$label <- topic.names # Node label
  #nodes$size <- (topic$TopicProportions / max(topic$TopicProportions)) * 40 # Node size
  nodes$borderWidth <- 2 # Node border width
  
  nodes$color.background <- col
  nodes$color.border <- "black"
  nodes$color.highlight.background <- "orange"
  nodes$color.highlight.border <- "darkred"
  nodes$id <- 1:nrow(nodes)
  
  v <- list(nodes, edges)
  
  return(v)
}
