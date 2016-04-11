#poorly written function, will fix later
#compute the median graph
median.graph <- function(graph.list, target.quantile = 0.99){
  len = length(graph.list)
  #WARNING: need to check all graphs are of same size
  
  sum.graph = matrix(0, nrow(graph.list[[1]]), 
   ncol(graph.list[[1]]))

  for(i in 1:length(graph.list)){
    sum.graph = sum.graph + graph.list[[i]]
  }

  sum.graph[lower.tri(sum.graph)] = 0
 
  median.graph = matrix(0, nrow(graph.list[[1]]), 
   ncol(graph.list[[1]]))

  val = quantile(abs(sum.graph[sum.graph != 0]), prob = target.quantile)
  idx = which(abs(sum.graph) >= val)
  median.graph[idx] = sum.graph[idx]

  median.graph = median.graph + t(median.graph)

  median.graph
}
