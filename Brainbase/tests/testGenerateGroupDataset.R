#Test the GenerateGroupDataset function
PATH <- system.file("nifti/trtgroup",package = "Brainbase")
SbjList <-  list.files(path = PATH)
SbjList <- SbjList[-grep("mask", SbjList, fixed=T)]
mask <- BCoRead(paste0(PATH,"/mask.nii"))
mask <- new("Template", data = mask@data)
(groupList <- GenerateGroupDataset(PATH, SbjList, mask))

