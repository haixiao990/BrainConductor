graph.difference <- function(graph.list, case.control.pairings, target.sparsity = 0.025){
  diff.list = vector("list", nrow(case.control.pairings))

  for(i in 1:length(diff.list)){
    idx = which.min(abs(graph.list[[case.control.pairings[i,1]]]$sparsity - target.sparsity))
    graph1 = as.matrix(graph.list[[case.control.pairings[i,1]]]$path[[idx]])

    idx = which.min(abs(graph.list[[case.control.pairings[i,2]]]$sparsity - target.sparsity))
    graph2 = as.matrix(graph.list[[case.control.pairings[i,2]]]$path[[idx]])

    diff.list[[i]] = graph1-graph2
  }

  diff.list
}
