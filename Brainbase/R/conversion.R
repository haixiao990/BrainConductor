setGeneric("convert.4Dto2D", function(obj, ...) standardGeneric("convert.4Dto2D"))

setMethod("convert.4Dto2D", signature("NIdata"), function(obj, template = NULL, verbose = TRUE){
  assert_that(class(obj@data) == "BCoData4D")
 
  new.obj = obj

  if(!is.null(template)){
    assert_that(class(template) == "Template") #WARNING: need to relax this 
    template = template@data@mat
  }

  res = .convert.4Dto2Dmat(obj@data@mat, template, verbose)

  new.obj@data = .BCoData2D(mat = res$mat, mask = res$mask, base.dim = dim(obj@data@mat)[1:3])

  new.obj
})

setGeneric("convert.2Dto4D", function(obj) standardGeneric("convert.2Dto4D"))

#WARNING: Make sure all functions that don't need to be specifically
# NIData are BCoBase
setMethod("convert.2Dto4D", signature("BCoBase"), function(obj){
  assert_that(class(obj@data) == "BCoData2D")

  new.obj = obj

  res = .convert.2Dto4Dmat(obj@data@mat, obj@data@mask, obj@data@base.dimen)

  new.obj@data = .BCoData4D(mat = res)

  new.obj
})

#convert 4D matrix into 2D matrix
.convert.4Dto2Dmat <- function(dat, template = NULL, verbose = TRUE){
  assert_that(is.numeric(dat) & length(dim(dat))==4)
  assert_that(is.null(template) || all(dim(dat)[1:3] == dim(template)))

  dimen = dim(dat)
  if(is.null(template)){
    #NEED A FUNCTION HERE TO MAKE SURE THERE IS NO MOTION
    mask = which(dat[,,,1] != 0)
  } else {
    mask = .extract.mask(template)
  }

  mat = matrix(NA, dimen[4], length(mask))
  for(j in 1:dimen[4]){
    mat[j,] = as.numeric(dat[,,,j])[mask]

    if(j%% max(floor(dimen[4]/10),1) ==0 & verbose) cat('*')
  }

  list(mat = mat, mask = mask)
}

#convert 2D matrix back to 4D matrix
.convert.2Dto4Dmat <- function(mat, mask, dimen){
  assert_that(min(mask) >= 0)
  assert_that(max(mask) <= prod(dimen))
  assert_that(ncol(mat) == length(mask))

  dat = array(0, dim = c(dimen, nrow(mat)))

  #change the mask to handle the indices across different time slices
  idx = as.numeric(sapply(0:(nrow(mat)-1), function(x){mask+prod(dimen)*x}))  

  #fill in the values
  dat[idx] = t(mat)

  dat
}

#create mask from a 3D MNI template
.extract.mask <- function(template){
  assert_that(length(dim(template)) == 3)

  which(template != 0)
}

#onvert a location (single matrix index from 1 to prod(dimen)) into 3D coordinates
.convert.2Dto3Dloc <- function(idx, dimen) {
  assert_that(is.numeric(idx))
  assert_that(length(dimen)==3 & is.numeric(dimen))
  assert_that(idx <= prod(dimen))

  z = ceiling(idx / (dimen[1]*dimen[2]))

  tmp = idx %% (dimen[1]*dimen[2])
  if(tmp==0) tmp = dimen[1]*dimen[2]
  y = ceiling(tmp / dimen[1])

  x = tmp %% dimen[1]
  if(x==0) x = dimen[1]

  c(x,y,z)
}

# convert a location (3D by coordinates) into an index for a matrix
.convert.3Dto2Dloc = function(loc, dimen) {
  assert_that(length(loc)==3 & is.numeric(loc))
  assert_that(length(dimen)==3 & is.numeric(dimen))
  assert_that(all(loc<=dimen))
  assert_that(all(loc>0))

  loc[1]+dimen[1]*(loc[2]-1)+(dimen[1]*dimen[2])*(loc[3]-1)
}

