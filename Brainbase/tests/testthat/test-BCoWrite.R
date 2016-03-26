context("Writing NIdata")

library(assertthat)
library(oro.nifti)
library(testthat)

source("~/Brainconductor.git/Brainbase/R/BCoRead.R")
source("~/Brainconductor.git/Brainbase/R/NIdata.R")
source("~/Brainconductor.git/Brainbase/R/conversion.R")
source("~/Brainconductor.git/Brainbase/R/reduction.R")
source("~/Brainconductor.git/Brainbase/R/BCoRead_Helper.R")
source("~/Brainconductor.git/Brainbase/R/BCoWrite.R")

test_that("Verify that writing works", {
  subj1 = BCoRead("~/Brainconductor.git/data/50002_ABIDE_segment.nii.gz", subject.ID = "50002")

  BCoWrite(subj1, "~/Brainconductor.git/data/BC_saved")
  BCoWrite(subj1, "~/Brainconductor.git/data/BC_saved", controls = list(type = "NIfTI"))
  
  #read the data back in

  subj1.again = BCoRead("~/Brainconductor.git/data/BC_saved.nii.gz", subject.ID = "50002")
  expect_true(all(get.matrix(subj1) == get.matrix(subj1.again)))

  subj1 = BCoRead("~/Brainconductor.git/data/50002_ABIDE_segment.nii.gz", subject.ID = "50002")

  #WARNING: need to see if it still works if it's 2D matrix
})
