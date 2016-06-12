.plot2dparcelcontrol <- setClass("plot2dparcelcontrol", representation(
 num.slices = "numeric", view = "character", fill.holes = "logical"),
 prototype(num.slices = 12, view = "sagittal", fill.holes = TRUE))
 
#WARNING: make sure this uses the same view finder as in the 3d parcellation

setGeneric("plot2D.parcellation", function(obj, ...) 
 standardGeneric("plot2D.parcellation"))

setMethod("plot2D.parcellation", signature("factor"), function(obj,
  template = NULL,  controls = list(colors = NULL,
  num.slices = 12, view = "sagittal", fill.holes = TRUE)){

  assert_that(class(template@data) == "BCoData")
  con = .convert.list2control(controls, "plot2dparcelcontrol")

  #WARNING: replace /ALL/ "@" in the code with a function to grab
  if(class(template@data) == "BCoData4D"){
    mat = get.matrix(template, output2D = F)
    dim.mat = dim(mat)
    mask = which(mat != 0)
 } else {
    mask = template@data@mask
    dim.mat = template@data@base.dim
  }

 assert_that(length(obj) = length(mask))
 
 .plot2d.parcellation(obj, mask, dim.mat, controls)
})

setMethod("plot2D.parcellation", signature("integer"), function(obj,
  template = NULL,  controls = list(colors = NULL,
  num.slices = 12, view = "sagittal", fill.holes = TRUE)){

  plot2D.parcellation(as.factor(obj), template, controls)
}



setMethod("plot2D.parcellation", signature("BCoBase"), function(obj, 
  template = NULL,  controls = list(colors = NULL,
  num.slices = 12, view = "sagittal", fill.holes = TRUE)){

  con = .convert.list2control(controls, "plot2dparcelcontrol")
 
  assert_that(class(obj@data) == "BCoData")
  if(class(obj@data) == "BCoData4D"){
    mat = get.matrix(obj, output2D = F)
    mask = which(mat != 0)
    partition = as.factor(mat[mask])
  } else {
    #WARNING: Does this work?
    mask = obj@data@mask
    partition = get.matrix(obj)[1,]
  }

  #WARNING: need a way to fill in the holes
  if(!is.null(template)){
    if(class(template@data) == "BCoData4D"){
      t.mat = get.matrix(template, output2D = F)
      t.mask = which(t.mat != 0)
    } else {
      t.mask = template@data@mask
    }
    #determine if further action is needed
    if(length(mask) != length(t.mask) || all(sort(mask) != sort(t.mask))){
      
      #remove the voxels in mask and not in t.mask

      #fill in the missing voxels
      if(con@fill.holes){
      } else {
   
      }
    }
  }
 
  plot2d.parcellation(partition, obj, controls)
})

#partition must be a factor
.plot2d.parcellation <- function(partition, mask, templatedim,
  con){

  assert_that(class(partition) == "factor")
  assert_that(length(templatedim) == 3)
  assert_that(length(partition) == length(mask))
  assert_that(length(partition) <= prod(templatedim))
  assert_that(max(mask) <= prod(templatedim))  

  #convert the partition into consecutive numerics
  vec = numeric(length(partition))
  lev = levels(partition)

  for(i in 1:length(lev)) { vec[which(partition == lev[i])] = i }

  #determine the slices to be plotted
  templateMRI = array(0, dim = templatedim)
  templateMRI[mask] = vec
  res = .compute.slices(templateMRI, con@view, con@num.slices)

  #replace the values in the templateMRI with those in partition
  image.slices = .extract.slices(templateMRI, res$slice.idx, res$dim.idx)

  #now plot the partition
  #WARNING ACCEPT USER COLORS
  col.vec = c("black", rainbow(length(lev)))

  res = .compute.plotLayout(con@num.slices)

  par(mfrow = c(res$num.row, res$num.col), mar = rep(0.2, 4), bg = "black")

  for(i in 1:length(image.slices)){
    image(image.slices[[i]], col = col.vec,
     breaks = 0:length(col.vec) - 0.5,
     zlim = c(0, max(lev)), bty = "n", xaxt = "n", yaxt = "n",
     asp = ncol(image.slices[[1]])/nrow(image.slices[[1]]))
  }

  invisible()
} 

.compute.plotLayout <- function(num.plots) {
  num.row = ceiling(sqrt(num.plots/2))
  num.col = ceiling(num.plots / num.row)

  list(num.row = num.row, num.col = num.col)
}


#given a 3D image (img), split img along the dim.idx
#  direction, and output only the slice.indices
.extract.slices <- function(img, slice.idx, dim.idx){
  img.slices = alply(img, dim.idx)
  img.slices[slice.idx]
}


#given an image, find out the dimensional to slice along and the
# slices along that dimension
.compute.slices <- function(img, view, num.slices = 12, offset = 5) {  
  assert_that(dim(img) == 3)
  VIEWS = list.views()

  assert_that(view %in% VIEWS & length(view) == 1)
  
  dim.idx = which(VIEWS %in% view)  
  num.slices = min(num.slices, dim(img)[dim.idx])

  if(offset > dim(img)[dim.idx]) offset = 0

  #determine which dimension indicies have actual "image data" in them
  tmp = apply(img, dim.idx, function(x){sum(abs(x))})
  sort.tmp = sort(which(tmp>0), decreasing = FALSE)

  min.slice = sort.tmp[1] + offset
  max.slice = rev(sort.tmp)[1] - offset
  
  slice.idx = round(seq(min.slice, max.slice, length.out = num.slices))
  
  list(slice.idx = slice.idx, dim.idx = dim.idx)
}

list.views <- function(){
  c("sagittal", "coronal", "axial")
}
