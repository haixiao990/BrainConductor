rm(list = ls())

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


base.dir = "~/mridata/CPAC_from_TIGER/ABIDE"
setwd(base.dir)
subj.vec =  list.dirs(full.names = FALSE, recursive = FALSE)
subj.path = list.dirs(path = base.dir, recursive = FALSE)

load("~/Brainconductor.git/Templates/data/AAL_2mm.rda") #load AAL_2mm
load("~/Brainconductor.git/Templates/data/MNI_2mm_brain.rda") #load MNI_2mm_brain

lambda.seq = seq(1, 0.09, length.out = 20)

for(i in 1:length(subj.path)){
  setwd(subj.path[i])
  var.name = BCoReadandAssign("func2mni.nii.gz", "ABIDE", subject.ID = subj.vec[i])

  if(i %% floor(length(subj.path)/10) == 0) cat('*')
}

csvfile = read.csv("~/Brainconductor.git/data/ABIDE_pittsburgh.csv")
csvfile[,2] = as.character(csvfile[,2])
csvfile[,2] = paste0("00", csvfile[,2])
csvfile = csvfile[,c(1:3, 5:7 , 9:11)]

BCoLink.phenotype(csvfile, 2, c(3, 1, 5, 6, 7))

#save the variables
BCoWrite.all(file.dir = "~/fmri_script_test/20160312_casestudyBC", 
  subj.header = "ABIDE")

med.graph = BCoPopulation.analysis()

idx = which(med.graph != 0)
length(idx)
table(abs(med.graph[idx]))
table(sign(med.graph[idx]))

idx = which(med.graph != 0, arr.ind = T)
tab = table(idx[,1])
mult.node = as.numeric(names(tab)[which(tab > 1)])
graph.row.idx = which(idx[,1] %in% mult.node)
med.graph[idx[graph.row.idx,1], idx[graph.row.idx,2]]
