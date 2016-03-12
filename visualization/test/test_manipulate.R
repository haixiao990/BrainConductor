setwd("..")
source("source_header.R")


setwd("/home/smile/klsix/fmri_script_test/20151201_test/50002")
anat <- readNIfTI("anat2mni.nii.gz")

func <- readNIfTI("func2mni.nii.gz")

dat = readNIfTI("MNI152_T1_2mm_brain_symmetric.nii.gz")
dat = dat@.Data

setwd("/home/smile/klsix/COBRA.git/BrainConductor.plotter")

anat = anat@.Data
func = func@.Data

test_that("Mean Functional is correct", {
  meanFunc = compute.meanFunctional(func)

  expect_true(mean(func[45,45,45,]) == meanFunc[45,45,45])
  expect_true(mean(func[20,30,40,]) == meanFunc[20,30,40])
  expect_true(all(dim(meanFunc) == dim(func)[1:3]))
})
