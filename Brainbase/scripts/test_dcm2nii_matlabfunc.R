#test script for dcm2nii conversion function 
#using Matlab function
#setwd("./dcm")
#if (!suppressWarnings(require("R.matlab", quietly=TRUE))) {
#  install.packages("R.matlab");
#  suppressPackageStartupMessages (require("R.matlab", quietly=TRUE))
#} else {
#  suppressPackageStartupMessages (require("R.matlab", quietly=TRUE))    
#}   


nifti_object <- read.DICOM.dcm("./dicoms_rfmri")
write.NIFTI.nii(nifti_object,"testDcm2nii_MatlabFunc")
