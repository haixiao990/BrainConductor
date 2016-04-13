library(ks)
library(oro.nifti)
library(rgl)
library(misc3d)
library(assertthat)

#WARNING: make the controls
.plot.parcellation <- function(parcellation, parcel.list, file.name, template = NULL, val = NULL,
  shape.list = NULL, view = "sagittal",
  bot.col = rgb(252, 245, 192, max = 255), top.col = rgb(186, 109, 26, max = 255),
  window.size = c(0, 23, 1366, 728)){
  if(!is.null(val)) assert_that(all(val >= 0) & all(val <= 1))
  #WARNING: do a conversion if not
  assert_that(class(parcellation@data) == "BCoData4D")

  #WARNING: allow template

  #WARNING: maybe this should also be adjustable
  H.pi = matrix(1, ncol = 3, nrow = 3)
  diag(H.pi) = 3
  quant = 30
  dimen = dim(parcellation@data@mat)

  #generate color spectrum
  colfunc = colorRampPalette(c(bot.col, top.col))
  col.vec = colfunc(50)

  par3d("windowRect" = c(0,23,1366,728))
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

    save(shape.list, file = "~/Brainconductor.git/data/AAL_shapes.RData")
  }

  for(i in 1:length(parcel.list)){
    plot(shape.list[[i]], drawpoints = F, cont = quant, alphavec = 1, colors = col.vec[val.idx[i]], xlab = "",
      ylab = "", zlab = "", xlim = c(1,dimen[1]), ylim = c(1,dimen[2]), zlim = c(1,dimen[3]), add = T,
      axes = F, box = F)
  }

  #WARNING: ALLOW MULTIPLE SHOTS
  mat = .RGLviewfinder(view)
  par3d("userMatrix" = mat)

  clear3d(type = "lights") 
  light3d(specular = "black")
  light3d(diffuse = rgb(0.4, 0.4, 0.4), specular = "black")

    rgl.snapshot(file.name)

  scene = scene3d()

  scene
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
