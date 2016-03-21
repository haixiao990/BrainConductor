setGeneric("GenerateGroupDataset", function(Path, SbjList, OutputPath, Mask, lb = NULL, ub = NULL, ...) standardGeneric("GenerateGroupDataset")) 
#Maybe Mask and Template could also be in NIdata class format, with a specific description slot to indicate data, mask, or ROI template

setMethod("GenerateGroupDataset", signature("character", "character", "character","NIData", "numeric", "numeric"), function(Path, SbjList, template, verbose = TRUE){
  num_Sbj <- length(SbjList)
  for (i in 1:num_Sbj) {
    Sbjfilename[i] <- list.files(Path,SbjList[i])
  }
  
    
  filename <- paste0(Path,"/",Sbjfilename) 
  for (i in 1:length(filename)) {
      filename[i] <- sub("\\.gz$", "", filename[i])
      parts <- strsplit(filename[i],"\\.")
      nparts <- length(parts[[1]])
      filesuff <- parts[[1]][nparts] 
      filetype  <- switch(filesuff, 
                          Rdata = 'Rdata',
                          rda = 'Rdata',
                          "NIfile")
      if (filetype == 'Rdata'){
        Objname <- load(filename[i])          
        if (length(Objname)!=1)
          .warning('The Rdata file of ',SbjList[i],'includes not only one objects')
        Obj <- get(Objname)
        if (class(obj)!='NIdata')
          .warning('The Rdata file ')
      } else {
        obj <- BCoRead(filename[i])
      }
      
      #write BCod
      outputfile = paste0(outputdir,'/',)
      save( , outputfile)
  }
  save(Mask, output)
  
})