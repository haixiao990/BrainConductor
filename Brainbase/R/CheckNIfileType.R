.NIfile.type <- function(filename, pattern = NULL){
  
  if (!file.exists(filename))
    .stop(paste0(.dQuote(filename),"  does not exists!"))
  
  
  filetype <- NULL
  singlefile <- TRUE
  gzipped <- FALSE
  
  ####################Check if it is a folder####################
  fileinfo <- file.info(filename)
  if (fileinfo$isdir) {
    singlefile <- FALSE
    filelist <- list.files(path = filename, pattern, full.names = TRUE)
    for (i in 1:length(filelist)){
      .type <- .NIfile.type(filelist[i])
      
      if (.type$singlefile == FALSE)
        .stop(paste(.dQuote(filename),", this folder contains other folders, 
                    This is not allowed. Please check the data folder!"))
      
      if (i == 1) {
        filetype <- .type$filetype
        gzipped <- .type$gzipped
      }
      else if (.type$filetype != filetype || .type$gzipped != gzipped)
        .stop(paste(.dQuote(filename),", this folder contains files in different formats, 
                    This is not allowed. Please check the data folder!"))
    }
  }
  ###############################################################
  else {
    filesuff <- strsplit(filename,"\\.")[[1]]
    if (filesuff[length(filesuff)] == "gz"){
      gzipped <- TRUE
      filename <- sub("\\.gz$", "", filename)
      filesuff <- filesuff[length(filesuff) - 1]
    }
    else {
      filesuff <- filesuff[length(filesuff)]
    }
    
    filetype  <- switch(filesuff, 
                      nii = 'NIFTI',
                      hdr = 'ANALYZE',
                      HEAD = 'AFNI',
                      dcm = 'DICOM')
  }
        
  if (is.null(filetype))
    .stop(paste(.dQuote(filename),"is not a file or folder in valid NI format"))
  
  type <- list(filetype = filetype, gzipped = gzipped, singlefile = singlefile)
}