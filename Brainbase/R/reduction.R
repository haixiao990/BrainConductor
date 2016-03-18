setGeneric("BCoReduce", function(obj, template, ...) standardGeneric("BCoReduce"))

#WARNING: need a method if there template is just a vector
#WARNING: need a way to pass in a function 

setMethod("BCoReduce", signature("NIdata", "Template"), function(obj, template, 
 method = "mean", verbose = TRUE){

  #WARNING: Currently we have to have it in 2D
  assert_that(class(obj@data) == "BCoData2D")
  assert_that(method %in% c("mean", "pca"))

  if(method == "mean"){
    func = .reduction.mean
  } else if(method == "pca"){
    func = .reduction.pca
  }   

  #find out which voxel locations are empty
  nonempty.col = which(obj@data@mat[1,] != 0)

  #WARNING: This code is only for parcellations
  idx = obj@data@mask[nonempty.col]
  assert_that(length(nonempty.col) <= length(obj@data@mask))

  uniq = unique(as.numeric(template@data@mat))
  uniq = uniq[-which(uniq == 0)]
  newmat = matrix(0, ncol = length(uniq), nrow = nrow(obj@data@mat))

  for(i in 1:length(uniq)){
    idx.inmat = intersect(idx, which(template@data@mat == uniq[i]))

    if(length(idx.inmat) > 0){
      col.idx = mapvalues(idx.inmat, from = idx, to = nonempty.col, warn_missing = FALSE)
      assert_that(length(col.idx) == length(idx.inmat))
      newmat[,i] = func(obj@data@mat, col.idx)
 
      if(verbose && i %% floor(length(uniq)/10) == 0) cat('*')
    }
  }
  
  newobj = obj
  newobj@data = .BCoData2DReduc(mat = newmat, col.mapping = uniq, type = "AAL")

  newobj
})

#generic wrapper function to reduce
BCoreduce <- function(dat, template){
  #FILL THIS IN WHEN WE DECIDE UPON THE REPRESENTATION
}

#reduce voxel-level 2D matrix into parcel-level 2D matrix

#reduce voxel-level 2D matrix by gray matter tissue prior

#do reduction by doing pca on all the time-series in "idx"
# and taking the leading eigenvector
.reduction.pca <- function(dat, idx){
  #USE THE R PACKAGE THE SPECIALIZES IN LEADING EIGENVECTOR
}

#do reduction by taking the average of all the time-series in
# "idx"
.reduction.mean <- function(dat, idx){
  if(length(idx) > 0) apply(dat[,idx, drop = F], 1, mean) else rep(0, nrow(dat))
}
