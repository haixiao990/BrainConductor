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
  apply(dat[,idx], 1, mean)
}
