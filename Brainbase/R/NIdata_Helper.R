#WE NEED THE FOLLOWING FUNCTIONS: 
# -apply mask (just an intersection between template and NIdata)
# -form csv from the loaded NIdata objects based on the phenotypes and notes

setGeneric("get.matrix", function(obj, ...) standardGeneric("get.matrix"))

#WARNING: make sure all these have reasonable inputs
setMethod("get.matrix", signature("BCoBase"), function(obj, output2D = T){
  get.matrix(obj@data, output2D)
})

setMethod("get.matrix", signature("BCoData"), function(obj, output2D = T){
  if(output2D & class(obj) == "BCoData4D"){
    .convert.4Dto2Dmat(obj, verbose = F)
  } else {
    obj@mat
  }
})

#WARNING: Check this
setGeneric("dim", function(obj, ...) standardGeneric("dim"))

setMethod("dim", signature("BCoData"), function(obj){
  if(class(obj) == "BCoData4D"){
    res = dim(obj@mat)
    if(length(res) == 3) res = c(res, 1)
  } else {
    res = c(obj@base.dim, nrow(obj@mat))
  }

  res
})

setMethod("dim", signature("BCoBase"), function(obj){
  dim(obj@data)
})


setGeneric("is.functional", function(obj) standardGeneric("is.functional"))

setMethod("is.functional", signature("BCoData"), function(obj){
  if(class(obj) == "BCoData4D"){
    #WARNING: are you sure arrays always have 4 dim? is it sometimes 3?
    if(dim(obj@mat)[4] > 1) return(TRUE) else return(FALSE)
  } else if(class(obj) == "BCoData2D" | class(obj) == "BCoData2DReduc"){
    if(nrow(obj@mat) > 1) return(TRUE) else return(FALSE)
  } else return(FALSE)
})

setMethod("is.functional", signature("NIdata"), function(obj){
  is.functional(obj@data)
})

setGeneric("get.phenotype", function(obj) standardGeneric("get.phenotype"))

setMethod("get.phenotype", signature("NIdata"), function(obj){
  obj@phenotype
})


setMethod("show", "NIdata", function(object){
  if(length(object@ID) == 0) subj.name = "(Unidentified Subject)" else subj.name = paste0("Subject ", object@ID)
  cat(paste0("NIdata object for ", subj.name, "\n"))

  if(all(dim(object@data@mat) == 0)){
    cat("  No data stored in NIdata.\n")
  } else {
    if(class(object@data) == "BCoData2D"){
      perc = length(which(object@data@mat[1,] != 0))

      cat(paste0("  2-Dimensional data representing a 4D image of dimension ",
        paste0(object@data@base.dim, collapse = ", "), ":\n    ", nrow(object@data@mat),
        " elements in series, ", perc,
        " voxels active (", round(100*perc/ncol(object@data@mat), 2),
        "% of all voxels in mask).\n"))
    } else if (class(object@data) == "BCoData4D") {
      cat(paste0("  4-Dimensional data of dimension ",
        paste0(dim(object@data@mat), collapse = ", "), ": ", dim(object@data@mat)[4],
        " elements in series.\n"))

    } else if (class(object@data) == "BCoData2DReduc") {
      cat(paste0("  2-Dimensional data reduced from voxel-level data. ",
       nrow(object@data@mat), " elements in series, ", ncol(object@data@mat),
       " different series.\n"))
    }
  }

  cat(paste0("  Object consumes about ", round(as.numeric(object.size(object))/1048576, 2), " megabytes.\n"))

  cat(paste0("  Object has slots: ", paste0(names(getSlots(class(object))), collapse = ", "), ".\n"))
})


