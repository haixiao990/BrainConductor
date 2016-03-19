#Test the GenerateGroupDataset function
PATH <- system.file("nifti/trtgroup",package = "Brainbase")
SbjList <-  list.files(path = PATH)
SbjList <- SbjList[-grep("mask", SbjList, fixed=T)]


