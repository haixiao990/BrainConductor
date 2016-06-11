base.dir = "~/mridata/CPAC_from_TIGER/ABIDE/"
subj.vec = c("0050002", "0050003")

dest.dir = "~/Brainconductor.git/data/"
subj.dest.vec = paste0(c("50002", "50003"), "_ABIDE_segment")

func.name = "/func2mni.nii.gz"
anat.name = "/anat2mni.nii.gz"

library(oro.nifti)

library(assertthat)
source("~/Brainconductor.git/Brainbase/R/NIdata.R")
source("~/Brainconductor.git/Brainbase/R/NIdata_Helper.R")
source("~/Brainconductor.git/Brainbase/R/commonfunc.R")
source("~/Brainconductor.git/Brainbase/R/BCoRead.R")
source("~/Brainconductor.git/Brainbase/R/reduction.R")
source("~/Brainconductor.git/Brainbase/R/conversion.R")
source("~/Brainconductor.git/Brainbase/R/BCoRead_Helper.R")

for(i in 1:length(subj.vec)){
  #grab the data
  dat = readNIfTI(paste0(base.dir, subj.vec[i], func.name))
  #trim so only the first 5 time slices exists
  dat@dim_[3] = 3
  dat@.Data = dat@.Data[,,,1:3]

  #save the nii.gz to /data
  writeNIfTI(dat, filename = paste0(dest.dir, "func_", subj.dest.vec[i]))

  #convert to NIdata and save to Templates
  ni.mat = convert.nifti2nidata(dat)
  ni.mat@notes = paste0("From ABIDE ", subj.vec[i], " preprocessed using CPAC")

  ni.mat = convert.4Dto2D(ni.mat)

  tmp.name = paste0("example_func_", i)
  assign(tmp.name, ni.mat) 
  save(list = as.character(as.name(tmp.name)), file = paste0(tmp.name, ".RData"))

  #grab the anatomical data
  dat = readNIfTI(paste0(base.dir, subj.vec[i], anat.name))
  
  writeNIfTI(dat, filename = paste0(dest.dir, "anat_", subj.dest.vec[i])) 

  ni.mat = convert.nifti2nidata(dat)
  ni.mat@notes = paste0("From ABIDE ", subj.vec[i], " preprocessed using CPAC")


}
