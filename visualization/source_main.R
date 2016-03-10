plot.outlineAll <- function(func.path, anat.path, MNI.path, file.prefix){
  anat = readNIfTI(anat.path)
  func = readNIfTI(func.path)
  dat = readNIfTI(MNI.path)
  
  anat = anat@.Data
  func = func@.Data
  dat = dat@.Data

  meanFunc = compute.meanFunctional(func)
  date = Sys.Date()

  #overlap
  plot.overlap(anat, dat,
   filename = paste0("~/DUMP/", file.prefix, 
   "_anat_to_MNI2mm_sagittal_", date, ".png"))
  plot.overlap(anat, dat,
   filename = paste0("~/DUMP/", file.prefix,
   "_anat_to_MNI2mm_axial_", date, ".png"),
   view = "axial")

  #compute mean functional
  plot.overlap(meanFunc, dat,
   filename = paste0("~/DUMP/", file.prefix, 
   "_meanFunc_to_MNI2mm_sagittal_", date, ".png"))
  plot.overlap(meanFunc, dat,
   filename = paste0("~/DUMP/", file.prefix, 
   "_meanFunc_to_MNI2mm_axial_", date, ".png"),
   view = "axial")

  plot.overlap(meanFunc, anat,
   filename = paste0("~/DUMP/", file.prefix,
   "_meanFunc_to_anat_sagittal_", date, ".png"))
  plot.overlap(meanFunc, anat,
   filename = paste0("~/DUMP/", file.prefix,
   "_meanFunc_to_anat_axial_", date, ".png"),
   view = "axial")

  invisible()
}
