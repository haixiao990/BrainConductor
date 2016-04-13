source("~/Brainconductor.git/case-study/source_all.R")

load("~/Brainconductor.git/Templates/data/AAL_2mm.rda")
abide = BCoRead("~/Brainconductor.git/data/50002_ABIDE_segment.nii.gz")
load("~/Brainconductor.git/data/AAL_shapes.RData")

#WARNING: We need a convinent function for this
#WARNING: We have no print function for templates so far
uniq.val = sort(unique(as.numeric(AAL_2mm@data@mat)))[-1]
val.vec = numeric(length(uniq.val))

for(i in 1:length(uniq.val)){
  idx = which(AAL_2mm@data@mat == uniq.val[i])
  val = abide@data@mat[idx]
  
  val = val[which(val != 0)]
  val.vec[i] = mean(val)
}

val = (val.vec - min(val.vec))/(max(val.vec) - min(val.vec))
val = val^3

#WARNING: I think I need to close rgl after each plot
scene = .plot.parcellation(AAL_2mm, uniq.val, file.name = "~/Brainconductor.git/data/AAL_axial.png", 
  val = val, shape.list = shape.list, view = "axial")
scene = .plot.parcellation(AAL_2mm, uniq.val, file.name = "~/Brainconductor.git/data/AAL_sagittal.png",
  val = val, shape.list = shape.list, view = "sagittal")
scene = .plot.parcellation(AAL_2mm, uniq.val, file.name = "~/Brainconductor.git/data/AAL_coronal.png",
  val = val, shape.list = shape.list, view = "coronal")

#WARNING: we might be able to manipulate it right here
save(scene, file = "~/Brainconductor.git/data/test_scene.RData")
