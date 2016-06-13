.BCoViewcontrol <- setClass("BCoViewcontrol", representation(window.size = 
  "numeric", transparency = "numeric", direction.panel = "logical"),
  prototype(window.size = NULL, transparency = 1, direction.panel = TRUE))

BCoView.static <- function(obj1, obj2 = NULL, location, controls = 
 list(window.size = NULL, transparency = 1, direction.panel = TRUE)){

  assert_that(length(location)==3)  
  assert_that(class(obj1@data) == "BCoDataFull" & class(obj2@data) == 
   "BCoDataFull")
  #WARNING: All functions should use BCoDataFull to check

  con = .convert.list2control(controls, "BCoViewcontrol")
  assert_that(is.null(con@window.size) || length(con@window.size) == 3)

  if(length(obj1)==1) onlyOne = TRUE else onlyOne = FALSE
  if(class(obj1@data) == "BCoData2D"){
    dat1 = .convert.2Dto4Dmat(obj1@data)
  } else dat1 = obj1@data@mat  
  
  if(!is.null(obj2)){
    if(class(obj2@data) == "BCoData2D"){
      dat2 = .convert.2Dto4Dmat(obj2@data)
    } else dat2 = obj2@data@mat
  }

  #WARNING: This is should probably be an argument? Not sure....?
  par(mar = rep(0.2,4), bg = "black")
  if(onlyOne)  par(mfrow = c(2,2)) else par(mfrow=c(2,3))
  
  .BCoView.helper(dat1, location, con)
  if(!onlyOne) .BCoView.helper(dat2, location, con)
  
  if(!is.na(filename)) dev.off()
  
  invisible()
}

.BCoView.helper <- function(dat, location, con){

  assert_that(con@transparency >= 0 & con@transparency <= 1)
  assert_that(length(location)==3)
  
  red = rgb(1, 0, 0, con@transparency)
  dimen = dim(dat)
  
  if(is.null(con@window.size[1])){
    xrange = 1:dimen[1]
    yrange = 1:dimen[2]
    zrange = 1:dimen[3]
    
    asp.vec = c(dim(dat)[2]/dim(dat)[1], dim(dat)[3]/dim(dat)[1], 
     dim(dat)[3]/dim(dat)[2])
  } else {
    half = floor(window.size/2)
    
    xrange = max(location[1]-half[1],1) : min(location[1]+half[1],dimen[1])
    yrange = max(location[2]-half[2],1) : min(location[2]+half[2],dimen[2])
    zrange = max(location[3]-half[3],1) : min(location[3]+half[3],dimen[3])
    
    asp.vec = rep(1,3)
  }
 
  zlim = c(min(dat), max(dat))

  ##################
  #WARNING: ... there's gotta be a better way to do this...
  view.order = c("coronal", "sagittal", "axial")

  for(i in view.order){  
    if(i == "coronal") {
      dat.slice = dat[xrange, location[2], zrange]
      line.loc = c((location[1]-xrange[1]) / diff(range(xrange)),
       (location[3]-zrange[1]) / diff(range(zrange)))
    } else if(i == "sagittal") {
      dat.slice = dat[location[1], yrange, zrange]
      line.loc = c((location[2]-yrange[1]) / diff(range(yrange)),
       (location[3]-zrange[1]) / diff(range(zrange)))
    } else {
      dat.slice = dat[xrange, yrange, location[3]]
      line.loc = c((location[1]-xrange[1]) / diff(range(xrange)),
       (location[2]-yrange[1]) / diff(range(yrange)))
    }

    image(dat.slice, zlim = zlim,
     col = grey(seq(0, 1, length = 256)), asp = asp.vec[3], bty = "n",
     xaxt = "n", yaxt = "n")

  
    lines(x = rep(line.loc[1], 2), y = c(0,1), col = red, lwd = 2)
    lines(x = c(0,1), y = rep(line.loc[2], 2), col = red, lwd = 2)
  
    if(i == "coronal") {
      text(0.5, 0, "I", col = "white", pos = 3)
      text(0.5, 1, "S", col = "white", pos = 1)
      text(0, 0.5, "R", col = "white", pos = 4)
      text(1, 0.5, "L", col = "white", pos = 2)
    } else if(i == "sagittal") {
      text(0.5, 0, "I", col = "white", pos = 3)
      text(0.5, 1, "S", col = "white", pos = 1)
      text(0, 0.5, "P", col = "white", pos = 4)
      text(1, 0.5, "A", col = "white", pos = 2)
    } else {
      text(0.5, 0, "P", col = "white", pos = 3)
      text(0.5, 1, "A", col = "white", pos = 1)
      text(0, 0.5, "R", col = "white", pos = 4)
      text(1, 0.5, "L", col = "white", pos = 2)
      
    }
  }
 
  #list basic directions
  if(con@direction.panel){
    plot(NA, xlim = c(0,1), ylim = c(0,1), xaxt="n", yaxt="n")
    text(x = 0.5, y = 0.5, labels = "'Left' Key: Towards 'L'\n 
       'Right' Key: Towards 'R'\n
       'Up' Key: Towards 'A'\n 'Down' Key: Towards 'P'\n
       '.' Key: Towards 'I'\n '/' Key: Towards 'S'", col = "white")
  }

  invisible()
}
