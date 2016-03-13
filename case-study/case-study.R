rm(list = ls())

library(huge)
library(assertthat)
library(plyr)
library(oro.nifti)
library(snow)
library(DoMC)

source("~/Brainconductor.git/Brainbase/R/NIdata.R")
source("~/Brainconductor.git/Brainbase/R/conversion.R")
source("~/Brainconductor.git/Brainbase/R/BCoRead.R")
source("~/Brainconductor.git/Brainbase/R/reduction.R")

base.dir = "~/mridata/CPAC_from_TIGER/ABIDE"
setwd(base.dir)
subj.vec =  list.dirs(full.names = FALSE, recursive = FALSE)
subj.path = list.dirs(path = base.dir, recursive = FALSE)

load("~/Brainconductor.git/Templates/data/AAL_2mm.rda") #load AAL_2mm
load("~/Brainconductor.git/Templates/data/MNI_2mm_brain.rda") #load MNI_2mm_brain

lambda.seq = seq(1, 0.09, length.out = 20)

for(i in 1:length(subj.path)){
  setwd(subj.path[i])
  var.name = BCoReadandAssign("func2mni.nii.gz", "ABIDE", subject.ID = subj.vec[i])
  assign(var.name, convert.4Dto2D(eval(as.name(var.name)), MNI_2mm_brain)) 
  #we need to be able to do all this in ReadandAssign. This is quite complicated

  assert_that(length(dim(eval(as.name(var.name))@data@mat)) == 2)

  assign(var.name, BCoReduce(eval(as.name(var.name)), AAL_2mm))

  res = huge(eval(as.name(var.name))@data@mat, lambda = lambda.seq)

  #need to do something with this... store it somewhere?
}
