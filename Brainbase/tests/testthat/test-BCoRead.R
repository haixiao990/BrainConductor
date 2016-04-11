context("Reading and phenotype linking")

library(assertthat)
library(oro.nifti)
library(testthat)

source("~/Brainconductor.git/Brainbase/R/BCoRead.R")
source("~/Brainconductor.git/Brainbase/R/NIdata.R")
source("~/Brainconductor.git/Brainbase/R/conversion.R")
source("~/Brainconductor.git/Brainbase/R/reduction.R")
source("~/Brainconductor.git/Brainbase/R/BCoRead_Helper.R")


test_that("Verify phenotype linking works", {
  subj1 = BCoRead("~/Brainconductor.git/data/50002_ABIDE_segment.nii.gz", subject.ID = "50002")
  subj2 = BCoRead("~/Brainconductor.git/data/50003_ABIDE_segment.nii.gz", subject.ID = "50003")

  expect_true(class(subj1) == "NIdata")
  expect_true(subj1@ID == "50002")
  expect_true(all(dim(subj1@data@mat) == c(91, 109, 91, 10)))
  expect_true(all(dim(get.matrix(subj1)) == c(91, 109, 91, 10)))

  expect_true(all(subj1@scanner_info@dim_ == c(4, 91, 109, 91, 10, 1, 1, 1)))

  csvfile = read.csv("~/Brainconductor.git/data/ABIDE_pittsburgh.csv")
  csvfile[,2] = as.character(csvfile[,2])

  BCoLink.phenotype(csvfile, 2, c(1, 3, 5, 6, 7))

  expect_true(length(subj1@phenotype) == 5)
  expect_true(length(subj2@phenotype) == 5)
})

test_that("BCoRead is working", {
  subj1 = BCoRead("~/Brainconductor.git/data/50002_ABIDE_segment.nii.gz", subject.ID = "50002")

  mat = get.matrix(subj1) 

  expect_true(length(getSlots(class(subj1@scanner_info))) == 45)
  expect_true(all(dim(mat) == c(91, 109, 91, 10)))
 
  subj1 = BCoRead("~/Brainconductor.git/data/50002_ABIDE_segment.nii.gz", subject.ID = "50002", controls = list(convert2D = T))
 
  mat = get.matrix(subj1)

  expect_true(all(dim(mat) == c(10, 252390)))
})

test_that("Automatic conversion to 2D is working", {
  load("~/Brainconductor.git/Templates/data/MNI_2mm_brain.rda") #loads MNI_2mm_brain
  load("~/Brainconductor.git/Templates/data/AAL_2mm.rda") #loads AAL_2mm

  subj1 = BCoRead("~/Brainconductor.git/data/50002_ABIDE_segment.nii.gz", template = MNI_2mm_brain,
    subject.ID = "50002", controls = list(convert2D = T))
 
  mat = get.matrix(subj1)

  expect_true(length(dim(mat)) == 2)
  expect_true(ncol(mat) == length(which(get.matrix(MNI_2mm_brain) != 0)))

  subj1 = BCoRead("~/Brainconductor.git/data/50002_ABIDE_segment.nii.gz", template = MNI_2mm_brain,
    mask = AAL_2mm, subject.ID = "50002", controls = list(convert2D = T))

  expect_true(class(subj1@data) == "BCoData2DReduc")
  
  mat = get.matrix(subj1)
  expect_true(all(dim(mat) == c(10, 116)))
})
