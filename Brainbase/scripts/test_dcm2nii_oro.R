#test script for dcm2nii conversion function 
#using oro.dicom and oro.nifti
setwd("./dcm/dicoms_rfmri/")
if (!suppressWarnings(require("oro.nifti", quietly=TRUE))) {
  install.packages("oro.nifti");
  suppressPackageStartupMessages (require("oro.nifti", quietly=TRUE))
} else {
  suppressPackageStartupMessages (require("oro.nifti", quietly=TRUE))    
}   

if (!suppressWarnings(require("oro.dicom", quietly=TRUE))) {
  install.packages("oro.dicom");
  suppressPackageStartupMessages (require("oro.dicom", quietly=TRUE))
} else {
  suppressPackageStartupMessages (require("oro.dicom", quietly=TRUE))    
} 

dcm_object <- readDICOM("./",verbose = TRUE)
nifti_object <- dicom2nifti(dcm_object)
writeNIfTI(nim = nifti_object, filename = "testDcm2Nii_image")
