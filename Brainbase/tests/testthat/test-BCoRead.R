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
