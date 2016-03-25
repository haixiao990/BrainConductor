############## function check if it is a afni file #################
.is.AFNIfile <- function(filename){
  isAFNIfile <- FALSE
  
  filename <- sub("\\.gz$", "", filename)
  filename <- sub("\\.HEAD$", "", filename)
  filename <- sub("\\.head$", "", filename)
  filename <- sub("\\.BRIK$", "", filename)
  filename <- sub("\\.brik$", "", filename)
  if (  (file.exists(paste(fname, "HEAD", sep=".")) &&
         file.exists(paste(fname, "BRIK", sep=".")))  ||
        (file.exists(paste(fname, "HEAD", sep=".")) &&
         file.exists(paste(fname, "BRIK.gz", sep=".")))) {
    isAFNIfile <- TRUE 
  }
  isAFNIfile
}

########## function check if files are in hdr/img format ##########
.is.HDRandIMG <- function(filename){
  isHDRandIMG <- FALSE
  
  filename <- sub("\\.gz$", "", filename)
  filename <- sub("\\.hdr$", "", filename)
  filename <- sub("\\.img$", "", filename)
  
  if ( (file.exists(paste(filename, "hdr", sep=".")) &&
         file.exists(paste(filename, "img", sep="."))) ||
       (file.exists(paste(filename, "hdr.gz", sep=".")) &&
         file.exists(paste(filename, "img.gz", sep=".")))) {
    isHDRandIMG <- TRUE
  }
  isHDRandIMG
}

############## function check if it is a nifti file #################
.ANALYZEorNIFTI <- function(filename,gzipped = FALSE){
  
  filetype <- NULL
  
  if (gzipped) {
    fid <- gzfile(filename, "rb")
  } else {
    fid <- file(filename, "rb")
  }
  endian <- .Platform$endian
  sizeof.hdr <- readBin(fid, integer(), size=4, endian=endian)
  if (sizeof.hdr != 348) {
    close(fid)
    endian <- "swap"
    if (gzipped) {
      fid <- gzfile(filename, "rb")
    } else {
      fid <- file(filename, "rb")
    }
    sizeof.hdr <- readBin(fid, integer(), size=4, endian=endian)
  }
  
  if (sizeof.hdr == 348){
    invisible(readChar(fid,nchars = 340,useBytes = TRUE))
    magic <- readChar(fid,nchars = 4,TRUE)
    if (magic == "")
      filetype <- "ANALYZE"
    else 
      filetype <- "NIfTI"
  }
  close(fid)
  filetype
}




.NIfile.type <- function(filename, pattern = NULL){
  
  if (!file.exists(filename))
    .stop(paste0(.dQuote(filename),"  does not exists!"))
  
  
  filetype <- NULL
  Datafolder <- FALSE
  gzipped <- FALSE
  
  ####################Check if it is a folder####################
  fileinfo <- file.info(filename)
  if (fileinfo$isdir) {
    Datafolder <- TRUE
    filelist <- list.files(path = filename, pattern, full.names = TRUE)
    for (i in 1:length(filelist)){
      .type <- .NIfile.type(filelist[i])
      
      if (.type$Datafolder == TRUE)
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
      #filename <- sub("\\.gz$", "", filename)
      filesuff <- filesuff[length(filesuff) - 1]
    }
    else {
      gzipped <- FALSE
      filesuff <- filesuff[length(filesuff)]
    }
    
    filetype  <- switch(filesuff, 
                      nii = 'NIFTI',
                      dcm = 'DICOM')
    
    if(is.null(filetype)) 
      if (.is.AFNIfile(filename)){
        filetype <- "AFNI"
      }
      else if (.is.HDRandIMG(filename)){
        filetype <- .ANALYZEorNIFTI(filename,gzipped)
      }
  }
  
  #if (is.null(filetype))
  #  .stop(paste(.dQuote(filename),"is not a file or folder with valid NI format"))
  
  type <- list(filetype = filetype, gzipped = gzipped, Datafolder = Datafolder)
}