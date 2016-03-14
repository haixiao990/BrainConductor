setwd("..")
source("source_header.R")

setwd("C:/Users/UikosPC/Dropbox/Collaboration and People/Felix_Kevin_Han-seniorthesis2015-16/data")
dat = readNIfTI("MNI152_T1_2mm_brain_symmetric.nii.gz")
dat = dat@.Data

anat = readNIfTI("anat2mni.nii.gz")
anat = anat@.Data

test_that("Outline works",{
  res = compute.slices(dat, "sagittal", num.slices = 12)
  slice.idx = res$slice.idx
  dim.idx = res$dim.idx
  
  img.slices = extract.slices(dat, slice.idx, dim.idx)
  
  for(i in 1:length(slice.idx)){
    expect_true(all(img.slices[[i]] == dat[slice.idx[i],,]))
  }
})
