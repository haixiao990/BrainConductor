setGeneric("GenerateGroupDataset", function(Path, SbjList,  Mask, GroupName = "Group", OutputPath = "./", lb = NULL, ub = NULL, ...) standardGeneric("GenerateGroupDataset")) 
#Maybe Mask and Template could also be in NIdata class format, with a specific description slot to indicate data, mask, or ROI template

setMethod("GenerateGroupDataset", signature("character", "character", "Template", "character", "character","numeric", "numeric"), function(Path, SbjList, template, verbose = TRUE){
  num_Sbj <- length(SbjList)
  for (i in 1:num_Sbj) {
    Sbjfilename[i] <- list.files(Path,SbjList[i])
  }
  Sbjfilename <- sub("\\.gz$", "", Sbjfilename)
  filepath <- paste0(Path,"/",Sbjfilename)
  
  if (!dir.exists(OutputPath))
      dir.create(OutputPath)
  
  Group_Scanner_Info <- new(scanner_info)
  
  for (i in 1:length(filename)) {
      
      outputfilename <- character(0)
      ################## check file type #######################
      dataformat <- .NIfile.type(filepath[i])
      if (is.null(dataformat$filetype))
        .stop(paste(.dQuote(filepath[i]),"is not a file or folder with valid NI format"))
      
      if (dataformat$Datafolder) {
        outputfilename <- paste0(Groupname,"_",Sbjfilename[i],".Rdata")
      }
      else{
       
        outputfilename <- sub(filesuff,"Rdata",Sbjfilename[i])
        outputfilename <- paste0(Groupname,"_",outputfilename)
      }
      ##########################################################
      
      if (dataformat$filetype == 'Rdata'){
        Objname <- load(filename[i])          
        if (length(Objname)!=1){
          .warning(paste('The Rdata file of',SbjList[i], 
                         'includes multiple objects, only the first object will be loaded')
                  )
        }
        Obj <- get(Objname[1])
        remove(list = Objname)
        if (class(obj)!='NIdata'){
          .warning(paste('The Rdata file of',SbjList[i],
                         'includes not an NIdata object')
                  )
          next
        }
      } else {
        obj <- BCoRead(filepath[i])
        if (is.null(obj)){
          .warning(paste('Can\'t read image file of',SbjList[i],
                         ':', filepath[i])
                  )
          next
        }
      }
      
      #write image data within the group-unified mask
      if (i==1)
        Group_Scanner_Info <- obj@scanner_info
      #else if (scanner_info is not matched) .stop("The scanner information is not matched among the subjects")
        
      outputfile <-  paste0(outputdir,'/',outputfilename)
      save(obj@data, outputfile)
  }
  
  MaskDataFile <- paste0(outputdir,"/",Groupname,"_mask.Rdata")
  ScannerInfoFile <- paste0(outputdir,"/",Groupname,"_ScannerInfo.Rdata")
  save(Mask, MaskDataFile)
  save(Group_Scanner_Info,ScannerInfoFile)
  
  GroupFileList <- list.files(path = outputdir)
})

