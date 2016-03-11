#script to form the Rdata files for the BrainTemplate package

rm(list=ls())
source("~/Brainconductor.git/Brainbase/R/NIdata.R")

library(oro.nifti)
dat = readNIfTI("MNI152_T1_2mm_brain.nii.gz")
dat = dat@.Data


MNI_2mm_brain = .Template(.BCoBase(data = .BCoData4D(mat = dat), 
  notes = 
  paste0("From MNI152_T1_2mm_brain.nii.gz file in FSL, made on ", Sys.Date())))

save(MNI_2mm_brain, file = "~/Brainconductor.git/Templates/data/MNI_2mm_brain.rda")
