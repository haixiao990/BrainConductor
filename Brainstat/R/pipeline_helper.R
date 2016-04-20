setGeneric("perturb.columns", function(obj) standardGeneric("perturb.columns"))

setMethod("perturb.columns", signature("NIdata"), function(obj, verbose = T) {
  assert_that(dim(obj@data) == 2)

  colsums = apply(obj@data@mat, 2, function(x){sum(abs(x))})
  idx.zero = which(colsums == 0)

  if(length(idx.zero) > 0){
    print(paste0(deparse(substitute(obj)), " has ", length(idx.zero), " zero-columns."))
    numrow = nrow(obj@data@mat)

    tmpmat = obj@data@mat

    for(j in 1:length(idx.zero)){
      tmpmat[,idx.zero[j]] = rnorm(numrow)
    }

    obj@data@mat = tmpmat
  }

  obj
})
