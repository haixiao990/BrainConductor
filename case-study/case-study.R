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
  assign(var.name, convert.4Dto2D(eval(as.name(var.name)), AAL_2mm, verbose = F)) 
  #we need to be able to do all this in ReadandAssign. This is quite complicated

  assert_that(length(dim(eval(as.name(var.name))@data@mat)) == 2)

  assign(var.name, BCoReduce(eval(as.name(var.name)), AAL_2mm, verbose = F))
  #res = huge(eval(as.name(var.name))@data@mat, lambda = lambda.seq)
  #need to do something with this... store it somewhere?

  if(i %% floor(length(subj.path)/10) == 0) cat('*')
}

csvfile = read.csv("~/Brainconductor.git/data/ABIDE_pittsburgh.csv")
csvfile[,2] = as.character(csvfile[,2])
csvfile[,2] = paste0("00", csvfile[,2])
csvfile = csvfile[,c(1:3,5:7,9:11)]

BCoLink.phenotype(csvfile, 2, c(1, 3, 5, 6, 7))

#save the variables
setwd("~/fmri_script_test/20160312_casestudyBC")
variables = ls()
varClasses = sapply(variables, function(x){class(eval(as.name(x)))})
idx = which(varClasses == "NIdata")
for(i in 1:length(idx)){
  dat = eval(as.name(variables[idx[i]])) #WARNING: pretty inconvinient saving...
  save(dat, file = paste0(variables[idx[i]], ".RData"))
}

for(i in 1:length(idx)){
  colsums = apply(eval(as.name(variables[idx[i]]))@data@mat, 2, function(x){sum(abs(x))})
  idx.zero = which(colsums == 0)
  if(length(idx.zero) > 0){
    print(paste0(variables[idx[i]], " has ", length(idx.zero), " zero-columns."))
    numrow = nrow(eval(as.name(variables[idx[i]]))@data@mat)    

    tmpobj = eval(as.name(variables[idx[i]]))
    tmpmat = tmpobj@data@mat

    for(j in 1:length(idx.zero)){
      set.seed(10*i*j)
      tmpmat[,idx.zero[j]] = rnorm(numrow)
    }
    
    tmpobj@data@mat = tmpmat
    assign(variables[idx[i]], tmpobj)
  }

  res = huge(eval(as.name(variables[idx[i]]))@data@mat, lambda = lambda.seq)

  save(res, file = paste0("graph_", variables[idx[i]], ".RData"))
}


#compute the median graph now
#THIS IS BAD CODING STYLE. This will eventually be moved into Brainstat package

#determine which ones are case and control
subj.id = csvfile$SUB_ID

graph.list = list()

for(i in 1:length(subj.id)){
  load(paste0("graph_ABIDE_", subj.id[i], ".RData"))
  graph.list[[i]] = res
}

#############
#construct the case-control matching
idx = which(csvfile$DX_GROUP == 1)
mapping.mat = matrix(0, ncol = length(idx), nrow = 2)
mapping.mat[1,] = idx
csvfile2 = csvfile
csvfile2$AGE_AT_SCAN = csvfile2$AGE_AT_SCAN/sd(csvfile2$AGE_AT_SCAN)
csvfile2$FIQ = csvfile2$FIQ/sd(csvfile2$FIQ)

for(i in 1:length(idx)){
  idx.control = intersect(which(csvfile$DX_GROUP == 2), which(csvfile$SEX == csvfile[idx[i], "SEX"])) 
  vec = sapply(1:length(idx.control), function(x){
    sum((csvfile[idx.control[x],c("AGE_AT_SCAN", "FIQ")] - csvfile[idx[i],c("AGE_AT_SCAN", "FIQ")])^2)
  })
  mapping.mat[2,i] = idx.control[which.min(vec)]
}

mapping.mat = t(mapping.mat)

##############
#compute graph differences
diff.list = graph.difference(graph.list, mapping.mat)
median.graph = median.graph(diff.list)

idx = which(median.graph != 0)
length(idx)
table(abs(median.graph[idx]))
table(sign(median.graph[idx]))

idx = which(median.graph != 0, arr.ind = T)
tab = table(idx[,1])
mult.node = as.numeric(names(tab)[which(tab > 1)])
graph.row.idx = which(idx[,1] %in% mult.node)
median.graph[idx[graph.row.idx,1], idx[graph.row.idx,2]]
