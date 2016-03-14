######################## Defines Thresholding Function #################
thres = function(mat, s){
  # Threshold based on s
  mat[upper.tri(mat)] = abs(mat[upper.tri(mat)]) + 1
  mat[lower.tri(mat)] = 0
  indices = order(abs(mat),decreasing = TRUE)[1:s]
  
  mat[indices] = 1;
  mat[-indices] = 0;
  
  return(mat)
}
#########################################################################