setGeneric("GenerateGroupDataset", function(Path, SbjList, OutputPath, Mask, lb = NULL, ub = NULL, ...) standardGeneric("GenerateGroupDataset")) 
#Maybe Mask and Template could also be in NIdata class format, with a specific description slot to indicate data, mask, or ROI template

setMethod("GenerateGroupDataset", signature("character", "character", "character","NIData", "numeric", "numeric"), function(Path, SbjList, template, verbose = TRUE){
#  num_Sbj <- length(SbjList)
#  for (i in 1:num_Sbj) {
#    filename[i] <- list.files(Path,SbjList[i])
#  }
  
    
  filename <- paste0(Path,"/",SbjList) 
  for (i in 1:length(filename)) {
      filename[i] <- sub("\\.gz$", "", filename[i])
      parts <- strsplit(filename[i],"\\.")
      nparts <- length(parts[[1]])
      filesuff <- parts[[1]][nparts] 
      filetype  <- switch(filesuff, 
                          Rdata = 'Rdata',
                          rda   = 'Rdata',
                          "NIfile")
      if (filetype == 'Rdata'){
        Objname <- load(filename[i])          #How to return the object saved in Rdata file?
        ####
      } else {
        BCoRead(filename[i])
      }
      
      #write BCod
      outputfile = paste0(outputdir,'/',)
      save( , outputfile)
  }
  save(Mask, output)
  
})