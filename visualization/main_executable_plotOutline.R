#!/usr/bin/Rscript

source("source_header.R")

parser = ArgumentParser()
parser$add_argument("--func", type = "character",
  help = "Functional file (Preprocessed correctly)")
parser$add_argument("--anat", type = "character",
  help = "Anatomical file (Preprocessed correctly)")
parser$add_argument("--mni", type = "character",
  help = "MNI Template")
parser$add_argument("--filename", type = "character",
  help = "File prefix to append to file names")

args = parser$parse_args()
print("argument done")

anat = readNIfTI(args$anat)
func = readNIfTI(args$func)
mni = readNIfTI(args$mni)

print("loading done")

anat = anat@.Data
func = func@.Data
mni = mni@.Data

meanFunc = compute.meanFunctional(func)

print("mean done")

#anat to mni
plot.overlap(anat, mni, 
 filename = paste0(args$filename, "_anat2mni_sagittal_", DATE, ".png"))
plot.overlap(anat, mni,
 filename = paste0(args$filename, "_anat2mni_axial_", DATE, ".png"),
 view = "axial")

print("plot 1 done")

#func to anat
plot.overlap(meanFunc, anat,
 filename = paste0(args$filename, "_func2anat_sagittal_", DATE, ".png"))
plot.overlap(meanFunc, anat,
 filename = paste0(args$filename, "_func2anat_axial_", DATE, ".png"),
 view = "axial")

print("plot 2 done")

#func to mni
plot.overlap(meanFunc, mni,
 filename = paste0(args$filename, "_func2mni_sagittal_", DATE, ".png"))
plot.overlap(meanFunc, mni,
 filename = paste0(args$filename, "_func2mni_axial_", DATE, ".png"),
 view = "axial")

print("plot 3 done")
