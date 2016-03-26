setGeneric("BCoReduce", function(obj, template, ...) standardGeneric("BCoReduce"))

setMethod("BCoReduce", signature("NIdata", "Template"), function(obj, template, 
 method = "mean", verbose = TRUE){

  #check dimensions are the same
  #need to figure out which representation it uses...
  if(class(template@data) == "BCoData4D") {
    assert_that(all(dim(template@data) == obj@data@base.dim))
  } else if(class(template@data) == "BCoData2D") {
    assert_that(all(template@data@base.dim == obj@data@base.dim))
  }

  #extract the vector
  if(class(template@data) == "BCoData4D") {
    vec = template@data@mat[obj@data@mask]
  } else {
    #handle BCoData2D
    #WARNING: Test this!!!
    idx = intersect(template@data@mask, obj@data@mask)
    idx.remap = mapvalues(idx, from = obj@data@mask, to = 1:ncol(obj@data@mat))
    vec = rep(0, ncol(obj@data@mat))
    vec[idx.remap] = template[idx]
  }

  #run in vector form
  BCoReduce(obj, vec, method, verbose)

})

setMethod("BCoReduce", signature("NIdata", "numeric"), function(obj, template,
 method = "mean", verbose = TRUE){
  assert_that(class(method) == "function" || method %in% c("mean", "pca"))
  assert_that(ncol(obj@data@mat) == length(template))

  if(class(obj@data) == "BCoData4D"){
    if(verbose) print("Converting 4D matrix into 2D matrix.\n")
    obj = convert.4Dto2D(obj)
  }

  if(class(method) == "function"){
    func = method
  } else if(method == "mean"){
    func = .reduction.mean
  } else if(method == "pca"){
    func = .reduction.pca
  } 

  #find out which voxel locations are empty
  nonempty.col = which(obj@data@mat[1,] != 0)
  assert_that(length(nonempty.col) > 0)

  idx = obj@data@mask[nonempty.col]
  assert_that(length(nonempty.col) <= length(obj@data@mask))

  uniq = unique(template)
  if(length(which(uniq == 0)) > 0) uniq = uniq[-which(uniq == 0)]
  newmat = matrix(0, ncol = length(uniq), nrow = nrow(obj@data@mat))

  for(i in 1:length(uniq)){
    idx.inter = intersect(idx, which(template == uniq[i]))

    if(length(idx.inter) > 0){
      newmat[,i] = func(obj@data@mat, idx.inter)
 
      if(verbose && i %% floor(length(uniq)/10) == 0) cat('*')
    }
  }
  
  newobj = obj
  newobj@data = .BCoData2DReduc(mat = newmat, col.mapping = uniq, type = "AAL")

  newobj
})

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
