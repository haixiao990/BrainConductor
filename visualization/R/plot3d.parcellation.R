.plot3dparcelcontrol <- setClass("plot3dparcelcontrol", representation(view = "character", 
 bot.col = "character", top.col = "character", window.size = "numeric", 
 return.shape = "logical", return.scene = "logical"), 
 prototype(view = "sagittal", 
 bot.col = rgb(252, 245, 192, max = 255), top.col = rgb(186, 109, 26, max = 255),
 window.size = c(0, 23, 1366, 728), return.shape = F, return.scene = F))

plot3D.parcellation <- function(obj, parcel.list, file.name, val = NULL, shape.list = NULL,
  controls = list(view = "sagittal",
  bot.col = rgb(252, 245, 192, max = 255), top.col = rgb(186, 109, 26, max = 255),
  window.size = c(0, 23, 1366, 728), return.shape = F, return.scene = F){

  con = .list2control(controls, "plot3dparcelcontrol")

  if(!is.null(val)) assert_that(all(val >= 0) & all(val <= 1))
  #WARNING: all functions need to make sure it's not BCoData2DReduc
  assert_that(class(parcellation@data) == "BCoData")
  if(class(parcellation@data) == "BCoData2D"){
   dat = .convert.2Dto4Dmat 
  } else if(class(parcellation@data) == "BCoData4D"){
   dat = parcellation@data@mat
  }

  #WARNING: maybe this should also be adjustable
  H.pi = matrix(1, ncol = 3, nrow = 3)
  diag(H.pi) = 3
  quant = 30
  dimen = dim(dat)

  #generate color spectrum
  colfunc = colorRampPalette(c(con@bot.col, con@top.col))
  col.vec = colfunc(50)

  par3d("windowRect" = con@window.size)
  material3d(shininess = 0, specular = "#000000")

  #translate the values if val to colors
  if(!is.null(val)){
    #round to the nearest 0.02
    val.idx = round(val * 50)
  }

  if(is.null(shape.list)){
    shape.list = vector("list", length(parcel.list))

    for(i in 1:length(parcel.list)){
      idx = which(parcellation@data@mat == parcel.list[i], arr.ind = T)
      assert_that(length(idx) > 0)

      #apply the kernel smoothing to smooth the parcel
      shape.list[[i]] = kde(idx, H = H.pi, compute.cont = T)
    }
  
    names(shape.list) = parcel.list

   #WARNING: NEED TO OUTPUT THE SHAPES SOMEHOW
   #WARNING: In general, output the computed shapes from plotting functions
  }

  for(i in 1:length(parcel.list)){
    plot(shape.list[[i]], drawpoints = F, cont = quant, alphavec = 1, 
      colors = col.vec[val.idx[i]], xlab = "",
      ylab = "", zlab = "", xlim = c(1,dimen[1]), ylim = c(1,dimen[2]), 
      zlim = c(1,dimen[3]), add = T,
      axes = F, box = F)
  }

  #WARNING: ALLOW MULTIPLE SHOTS
  mat = .RGLviewfinder(con@view)
  par3d("userMatrix" = mat)

  clear3d(type = "lights") 
  light3d(specular = "black")
  light3d(diffuse = rgb(0.4, 0.4, 0.4), specular = "black")

  rgl.snapshot(file.name)

  scene = scene3d()

  if(con@return.scene) scene else invisible()
}

.RGLviewfinder <- function(view){
  assert_that(view %in% c("top", "bottom", "left", "right", "front", "back",
   "axial", "coronal", "sagittal"))
  assert_that(length(view) == 1)

  mat = diag(4)

  if(view == "top" | view == "axial"){
  } else if(view == "bottom"){
    mat[1,1] = mat[3,3] = -1
  } else if(view == "left" | view == "sagittal"){    
    mat[,c(1:3)] = mat[,c(3,1,2)]
  } else if(view == "right") {
    mat[1,1] = mat[3,3] = -1
    mat[,c(1:3)] = mat[,c(3,1,2)]

    #WARNING NEED TO CHECK WHICH ONE IS AXIAL
  } else if(view == "front"){
    mat[1,1] = -1
    mat[,c(2:3)] = mat[,c(3,2)]
  } else if(view == "back" | view == "coronal"){
    mat[3,3] = -1
    mat[,c(2:3)] = mat[,c(3,2)]
  }

  mat
}
