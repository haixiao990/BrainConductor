#convert 4D matrix into 2D matrix
.convert.4Dto2Dmat <- function(dat, template = NULL, verbose = TRUE){
  assert_that(is.numeric(dat) & length(dim(dat))==4)
  assert_that(is.null(template) || all(dim(dat) == dim(template)))

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

    if(j%%floor(dimen[4]/10)==0 & verbose) cat('*')
  }

  list(mat = mat, mask = mask)
}

#convert 2D matrix back to 4D matrix
.convert.2Dto4Dmat <- function(mat, mask, dimen, verbose = TRUE){
  assert_that(min(mask) >= 0)
  assert_that(max(mask) <= prod(dimen))
  assert_that(ncol(mat) == length(mask))

  dat = array(NA, dim = dimen)
  array.idx = sapply(mask, .convert.2Dto3Dloc, dimen = dimen)
  dat[array.idx[,1], array.idx[,2], array.idx[,3],] = mat #NEED TO TEST IF THIS WORKS

  dat
}

#create mask from a 3D MNI template
.extract.mask <- function(template){
  assert_that(length(dim(template)) == 3)

  which(template != 0)
}

#onvert a location (single matrix index from 1 to prod(dimen)) into 3D coordinates
.convert.2Dto3Dloc <- function(idx, dimen){
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
.convert.3Dto2Dloc = function(loc, dimen)
{
  assert_that(length(loc)==3 & is.numeric(loc))
  assert_that(length(dimen)==3 & is.numeric(dimen))
  assert_that(all(loc<=dimen))
  assert_that(all(loc>0))

  loc[1]+dimen[1]*(loc[2]-1)+(dimen[1]*dimen[2])*(loc[3]-1)
}

