library(fslr)
options("fsl.path" = Sys.getenv("FSLDIR"))
options("fsl.outputtype" = "NIFTI_GZ")
library(oro.nifti)

setwd("~/Brainconductor.git/data")
fslview("50002_ABIDE_segment.nii.gz") #literally opens fslview in the terminal


