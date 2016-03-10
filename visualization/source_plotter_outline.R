determine.lambda <- function(img, num.changepoint = 22){
  assert_that((is.matrix(img) | class(img)=="array") & is.numeric(img))
  assert_that(length(dim(img))>=3)
  
  mid = floor(dim(img)[1]/2)
  mid2 = floor(dim(img)[3]/2)
  
  slice = img[mid, mid2, ]
  res = fusedlasso1d(slice)
  coef(res, df=22)$lambda
}

compute.outlineSlice <- function(img.slice, lambda){
  assert_that(length(dim(img.slice))==2 & is.matrix(img.slice) 
              & is.numeric(img.slice))
  
  #use fused lasso
  compute.changepoint <- function(x){
    if(sum(abs(x))==0) return(x)
    
    flsa(x, lambda2 = lambda)
  }
  
  #hor.change and ver.change are stored in columns
  hor.change = apply(img.slice, 1, compute.changepoint)
  ver.change = apply(img.slice, 2, compute.changepoint)
  
  #append the row/column idx
  hor.change = rbind(1:ncol(hor.change), hor.change)
  ver.change = rbind(1:ncol(ver.change), ver.change)

  #convert the fused lasso est into a matrix with 2 columns:
  # row (or column) ID and column (or row) position
  convert.fLasso2idx <- function(vec, tol = 1e-5){
    idx = which(abs(diff(vec[-1])) > tol)
    
    if(length(idx)>0){
      cbind(vec[1], idx)
    } else {
      return()
    }
  }

  hor.idx = apply(hor.change, 2, convert.fLasso2idx)
  if(length(hor.idx)>0){
    hor.idx = do.call(rbind, hor.idx)
  } else hor.idx = list()
  
  ver.idx = apply(ver.change, 2, convert.fLasso2idx)
  if(length(ver.idx)>0){
    ver.idx = do.call(rbind, ver.idx)
  } else ver.idx = list()
  
  list(hor.idx = hor.idx, ver.idx = ver.idx)
  
}

compute.outline <- function(img, lambda = NA, num.changepoint = 22,
                            num.slices = 12, view = "sagittal",
                            slice.idx = NA, dim.idx = NA){
  
  assert_that((is.matrix(img) | class(img)=="array")  & is.numeric(img))
  assert_that(length(dim(img))==3 | length(dim(img))==4)
  assert_that(is.na(lambda) | is.numeric(lambda))
  assert_that(is.na(slice.idx[1]) == is.na(dim.idx))
  
  if(is.na(lambda)) lambda = determine.lambda(img, num.changepoint)
  
  if(is.na(slice.idx[1])) {
    res = compute.slices(img, view, num.slices = num.slices)
    slice.idx = res$slice.idx
    dim.idx = res$dim.idx
  }
  
  img.slices = extract.slices(img, slice.idx, dim.idx)
  
  llply(img.slices, compute.outlineSlice, lambda = lambda)
}

compute.slices <- function(img, view, num.slices = 12, offset = 5){
  
  assert_that(view %in% c("sagittal", "coronal", "axial") & length(view)==1)
  
  dim.idx = which(c("sagittal", "coronal", "axial") %in% view)  
  
  #determine which dimension indicies have actual "image data" in them
  tmp = apply(img, dim.idx, function(x){sum(abs(x))})
  min.slice = sort(which(tmp>0), decreasing = FALSE)[1] + offset
  max.slice = sort(which(tmp>0), decreasing = TRUE)[1] - offset
  
  slice.idx = round(seq(min.slice, max.slice, length.out = num.slices))
  
  list(slice.idx = slice.idx, dim.idx = dim.idx)
}

extract.slices <- function(img, slice.idx, dim.idx){
  
  img.slices = alply(img, dim.idx)
  
  img.slices[slice.idx]
}

compute.plotLayout <- function(num.plots){
  num.row = ceiling(sqrt(num.plots/2))
  num.col = ceiling(num.plots / num.row)
  
  list(num.row = num.row, num.col = num.col)
}

plot.overlap <- function(img.base, img.outline, lambda = NA, num.changepoint = 22,
                         num.slices = 12, view = "sagittal", filename = NA,
                         transparency = 0.5, cex = 0.15){
  
  assert_that(all(dim(img.base)==dim(img.outline)))
  assert_that(transparency >= 0 & transparency <= 1)  

  #compute slices
  res = compute.slices(img.outline, view)
  slice.idx = res$slice.idx
  dim.idx = res$dim.idx
  
  if(!is.na(filename)) png(filename, height=4, width=6, units="in", res=600)
  
  #set the graphic layout
  res = compute.plotLayout(num.slices)
  par(mfrow = c(res$num.row, res$num.col), mar = rep(0.2,4), bg="black")
  if(transparency!=0) red = rgb(1,0,0,transparency) else red = rgb(1,0,0)
  
  #compute outline
  outline = compute.outline(img.outline, slice.idx = slice.idx, dim.idx = dim.idx)
  img.slices = extract.slices(img.base, slice.idx, dim.idx)
  
  zlim = c(min(img.base), max(img.base))
  asp = ncol(img.slices[[1]])/nrow(img.slices[[1]])
  
  #plot
  for(i in 1:length(slice.idx)){
    
    image(img.slices[[i]], col = grey(seq(0, 1, length = 256)), asp = asp,
          zlim = zlim, bty = "n", xaxt = "n", yaxt = "n")
    
    #plot hor.change
    changepoints = outline[[i]]$hor.idx
    for(j in 1:nrow(changepoints)){
      points(x = (changepoints[j,1]-.5)/nrow(img.slices[[i]]), 
             y = (changepoints[j,2]-.5)/ncol(img.slices[[i]]), 
             col = red, pch = 16, cex = cex)
    }
    
    #plot ver.change
    changepoints = outline[[i]]$ver.idx
    for(j in 1:nrow(changepoints)){
      points(x = (changepoints[j,2]-.5)/nrow(img.slices[[i]]), 
             y = (changepoints[j,1]-.5)/ncol(img.slices[[i]]), 
             col = red, pch = 16, cex = cex*2)
    }
  }
  
  #save
  if(!is.na(filename)) dev.off()
  invisible() 
}
