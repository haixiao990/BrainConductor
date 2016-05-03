load("~/Brainconductor.git/Templates/data/AAL_2mm.rda")

library(huge)
library(assertthat)
library(plyr)
library(oro.nifti)
library(snow)
library(DoMC)

source("~/Brainconductor.git/Brainbase/R/NIdata.R")
source("~/Brainconductor.git/Brainbase/R/conversion.R")
source("~/Brainconductor.git/Brainbase/R/BCoRead.R")
source("~/Brainconductor.git/Brainbase/R/reduction.R")
source("~/Brainconductor.git/Brainstat/R/mediangraph.R")
source("~/Brainconductor.git/Brainstat/R/difference.R")
source("~/Brainconductor.git/Brainbase/R/commonfunc.R")
source("~/Brainconductor.git/Brainbase/R/NIdata_Helper.R")

source("~/Brainconductor.git/visualization/plot2d.parcellation.R")

plot2D.parcellation(AAL_2mm)
plot2D.parcellation(AAL_2mm, controls = list(view = "axial"))

