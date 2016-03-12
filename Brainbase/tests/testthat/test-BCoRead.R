context("Reading and phenotype linking")

source("~/Brainconductor.git/Brainbase/R/BCoRead.R")
source("~/Brainconductor.git/Brainbase/R/NIdata.R")

subj1 = BCoRead("~/Brainconductor.git/data/50002_ABIDE_segment.nii.gz", subject.ID = "50002")
