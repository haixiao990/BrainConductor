#script to form the Rdata files for the BrainTemplate package

rm(list=ls())
setwd("~/Brainconductor.git/data/")
source("~/Brainconductor.git/Brainbase/R/NIdata.R")

library(oro.nifti)
dat = readNIfTI("MNI152_T1_2mm_brain.nii.gz")
dat = dat@.Data


MNI_2mm_brain = .Template(.BCoBase(data = .BCoData4D(mat = dat), 
  notes = 
  paste0("From MNI152_T1_2mm_brain.nii.gz file in FSL, made on ", Sys.Date())))

save(MNI_2mm_brain, file = "~/Brainconductor.git/Templates/data/MNI_2mm_brain.rda")

#WARNING: need to make this a "parcellation" later. too lazy right now
dat = readNIfTI("AAL_MNI_2mm.nii")
dat = dat@.Data

AAL_2mm = .Template(.BCoBase(data = .BCoData4D(mat = dat), 
  notes = 
  paste0("From AAL_MNI_2mm.nii file, made on ", Sys.Date())))

save(AAL_2mm, file = "~/Brainconductor.git/Templates/data/AAL_2mm.rda")
