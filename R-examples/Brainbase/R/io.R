data.import <- function (filename, filetype = NULL, reorient = F,...)
{
  filename <- sub("\\.gz$", "", filename)
  if(is.null(filetype)){
    parts <- strsplit(filename,"\\.")
    nparts <- length(parts[[1]])
    filesuff <- parts[[1]][nparts] 
    filetype  <- switch(filesuff, 
                        nii = 'NIFTI',
                        hdr = 'ANALYZE',
                        HEAD = 'AFNI',
                        dcm = 'DICOMfile')
    if(nparts == 1 || is.null(filetype))
      .stop('The filename is invalid, please either input the entire filename
             or set up a filetype if the filename is just a prefix')
  }
  allowed_type <- c("NIFTI", "ANALYZE", "AFNI", "DICOMfile", "DICOMdir")
  if(!filetype %in% allowed_type)
    .stop('Invalid filetype')
  if (filetype == 'NIFTI')
    object <- readNIFTI(filename, reorient = reorient)
  if (filetype == 'ANALYZE'){
    object <- readANALYZE(filename,...)
    object <- as(object,'nifti')
  }
  if (filetype == 'DICOMfile'){
    object <- readDICOMFile(filename,...)
    object <- dicom2nifti(object)
  }
  if(filetype == 'DICOMdir'){
    object <- readDICOM(filename,...)
    object <- dicom2nifti(object)
  }
  if (filetype == 'AFNI'){
    object <- read.AFNI(filename,setmask=F,...)
    object <- fmri2oro(object)
  }
  return(object)
}

data.export <- function(object, filename, filetype = NULL,...){
  if(is.null(filetype)){
    parts <- strsplit(filename,"\\.")
    nparts <- length(parts[[1]])
    filesuff <- parts[[1]][nparts] 
    filetype  <- switch(filesuff, 
                        nii = 'NIFTI',
                        hdr = 'ANALYZE',
                        HEAD = 'AFNI',
                        dcm = 'DICOMfile')
    if(nparts == 1 || is.null(filetype))
      .stop('The filename is invalid, please either input the entire filename
             or set up a filetype if the filename is just a prefix')
  }
  allowed_type <- c("NIFTI", "ANALYZE", "AFNI", "DICOMfile", "DICOMdir")
  if(!filetype %in% allowed_type)
    .stop('Invalid filetype')
  if (filetype == 'NIFTI')
    writeNIFTI(filename, reorient = reorient)
  if (filetype == 'ANALYZE'){
    object <- readANALYZE(filename)
    object <- as(object,'nifti')
  }
  if (filetype == 'DICOMfile'){
    object <- readDICOMFile(filename)
    object <- dicom2nifti(object)
  }
  if(filetype == 'DICOMdir'){
    object <- readDICOM(filename)
    object <- dicom2nifti(object)
  }
  if (filetype == 'AFNI'){
    object <- read.AFNI(filename,setmask=F)
    object <- fmri2oro(object)
  }
  
}  
  