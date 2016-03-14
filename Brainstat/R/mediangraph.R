#poorly written function, will fix later
#compute the median graph
median.graph <- function(huge.list, target.sparsity = 0.025, sparsity = 150){
  len = length(huge.list)
  #WARNING: need to check all graphs are of same size

  sum.graph = matrix(0, nrow(huge.list[[1]]$path[[1]]), 
   ncol(huge.list[[1]]$path[[1]]))

  for(i in 1:length(huge.list)){
    idx = which.min(abs(huge.list[[i]]$sparsity - target.sparsity))
    sum.graph = sum.graph + as.matrix(huge.list[[i]]$path[[idx]])
  }

  sum.graph[lower.tri(sum.graph)] = 0
  print(length(sum.graph != 0))
  print(quantile(sum.graph[sum.graph != 0]))

  median.graph = matrix(0, nrow(huge.list[[1]]$path[[1]]), 
   ncol(huge.list[[1]]$path[[1]]))

  idx = order(sum.graph, decreasing = TRUE)[1:sparsity]
  median.graph[idx] = 1

  median.graph = median.graph + t(median.graph)

  median.graph
}
