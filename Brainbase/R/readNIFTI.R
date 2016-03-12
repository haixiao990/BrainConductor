

#This Method 'read.nii' can read NIFTI S4 class from .nii files

setGeneric("read.nii",function(object, filename)  standardGeneric("read.nii"));

setMethod("read.nii", signature(object = "NIdata"), function(object, filename)
{
  object <- new('NIdata')
  con <- file(filename,'rb');
  endian <- if ((sizeof_hdr <- readBin(con,"int",1,4,endian="little")) == 348) "little" else "big"
  object@sizeof_hdr <- 348
  object@data_type <- readChar(con,10,TRUE)
  object@db_name <- readChar(con,18,TRUE)
  object@extents <- readBin(con,'int',1,4,endian = endian)
  object@session_error <- readBin(con,'int',1,2,endian=endian)
  object@regular <- readChar(con,1,TRUE)
  object@dim_info <- readChar(con,1,TRUE)
  object@dim_ = readBin(con,'int',8,2,endian=endian)
  object@intent_p1 <- readBin(con,'double',1,4,endian=endian)
  object@intent_p2 <- readBin(con,'double',1,4,endian=endian)
  object@intent_p3 <- readBin(con,'double',1,4,endian=endian)
  object@intent_code <- readBin(con,'int',1,2,endian=endian)
  object@datatype <- readBin(con,'int',1,2,endian=endian)
  object@bitpix <- readBin(con,'int',1,2,endian=endian)
  object@slice_start <- readBin(con,'int',1,2,endian=endian)
  object@pixdim <- readBin(con,'double',8,4,endian=endian)
  object@vox_offset <- readBin(con,'double',1,4,endian=endian)
  object@scl_slope <- readBin(con,'double',1,4,endian=endian)
  object@scl_inter <- readBin(con,'double',1,4,endian=endian)
  object@slice_end <- readBin(con,'int',1,2,endian=endian)
  object@slice_code <- readChar(con,1,TRUE)
  object@xyzt_units <- readChar(con,1,TRUE)
  object@cal_min <- readBin(con,'double',1,4,endian=endian)
  object@cal_max <- readBin(con,'double',1,4,endian=endian)
  object@slice_duration <- readBin(con,'double',1,4,endian=endian)
  object@toffset <- readBin(con,'double',1,4,endian=endian)
  object@glmax <- readBin(con,'int',1,4,endian=endian)
  object@glmin <- readBin(con,'int',1,4,endian=endian)

  object@descrip <- readChar(con,80,TRUE)
  object@aux_file <- readChar(con,24,TRUE)

  object@qform_code <- readBin(con,'int',1,2,endian=endian)
  object@sform_code <- readBin(con,'int',1,2,endian=endian)

  object@quatern_b <- readBin(con,'double',1,4,endian=endian)
  object@quatern_c <- readBin(con,'double',1,4,endian=endian)
  object@quatern_d <- readBin(con,'double',1,4,endian=endian)
  object@qoffset_x <- readBin(con,'double',1,4,endian=endian)
  object@qoffset_y <- readBin(con,'double',1,4,endian=endian)
  object@qoffset_z <- readBin(con,'double',1,4,endian=endian)

  object@srow_x <- readBin(con,'double',4,4,endian=endian)
  object@srow_y <- readBin(con,'double',4,4,endian=endian)
  object@srow_z <- readBin(con,'double',4,4,endian=endian)

  object@intent_name <- readChar(con,16,TRUE)
  object@magic <- readChar(con,4,TRUE)
  object@extender <- readChar(con,4,TRUE)
  object@file_type <- "NIFTI"
  bp = 352;
  id = 1;

  if (object@extender != "")
  {
    while (bp < object@vox_offset)
    {
      object@extention$esize[[id]] <- readBin(con,'int',1,4,endian=endian)
      object@extention$ecode[[id]] <- readBin(con,'int',1,4,endian=endian)
      object@extention$edata[[id]] <- readChar(con,object@extention$esize[[id]] - 8,TRUE)
      bp <- bp + object@extention$esize[[id]]
      id <- id + 1
    }
  }

  dx <- object@dim_[2]
  dy <- object@dim_[3]
  dz <- object@dim_[4]
  dt <- object@dim_[5]
  dd <- object@dim_[6]

  if (object@datatype == 1)   #binary
  {
    type <- "raw"
    size <- 1  #may be wrong
    signed <- TRUE
  }

  else if (object@datatype == 2)  #unsigned char
  {
    type <- "int"
    signed <- FALSE
    size <- if (object@bitpix) object@bitpix / dd / 8 else 1
  }
  else if (object@datatype == 4)  #signed short
  {
    type <- "int"
    signed <- TRUE
    size <- if (object@bitpix) object@bitpix / dd / 8 else 2
  }
  else if (object@datatype == 8) #signed int
  {
    type <- "int"
    signed <- TRUE
    size <- if (object@bitpix) object@bitpix / dd / 8 else 4
  }

  else if (object@datatype == 16) #float
  {
    type <- "double"
    signed <- TRUE
    size <- if (object@bitpix) object@bitpix / dd / 8 else 4
  }
  else if (object@datatype == 32) #complex
  {
    type <- "complex"
    signed <- TRUE
    size <- if (object@bitpix) object@bitpix / dd / 8 else 8
  }
  else if (object@datatype == 64) #double
  {
    type <- "double"
    signed <- TRUE
    size <- if (object@bitpix) object@bitpix / dd / 8 else 8
  }

  else if (object@datatype == 128) #RGB
  {
    type <- "int"
    signed <- TRUE
    size <- if (object@bitpix) object@bitpix / dd / 8 else 3
  }
  else
  {
    type <- 'int'
    signed <- TRUE
    size <- 1
  }

  object@image <- array(readBin(con,type,dx * dy * dz * dt * dd, size, signed = signed, endian = endian) ,dim =
                          if(object@dim_[1] == 5) c(dx,dy,dz,dt,dd)
                        else if (object@dim_[1] == 4) c(dx,dy,dz,dt)
                        else if (object@dim_[1] == 3) c(dx,dy,dz)
                        else if (object@dim_[1] == 2) c(dx,dy)
  )

  close(con)
  object
}
)

read.NIFTI.nii <- function(filename)
{
  a <- new("NIdata")
  read.nii(a,filename)

}


setGeneric("read.hdr_img",function(object, filename  )  standardGeneric("read.hdr_img"))

setMethod("read.hdr_img", signature(object = "NIdata"), function(object, filename )
{
  object <- new('NIdata')
  con <- file(paste(filename,'.hdr',sep=""),'rb')
  endian <- if ((sizeof_hdr <- readBin(con,"int",1,4,endian="little")) == 348) "little" else "big"
  object@sizeof_hdr <- 348
  object@data_type <- readChar(con,10,TRUE)
  object@db_name <- readChar(con,18,TRUE)
  object@extents <- readBin(con,'int',1,4,endian = endian)
  object@session_error <- readBin(con,'int',1,2,endian=endian)
  object@regular <- readChar(con,1,TRUE)
  object@dim_info <- readChar(con,1,TRUE)
  object@dim_ = readBin(con,'int',8,2,endian=endian)
  object@intent_p1 <- readBin(con,'double',1,4,endian=endian)
  object@intent_p2 <- readBin(con,'double',1,4,endian=endian)
  object@intent_p3 <- readBin(con,'double',1,4,endian=endian)
  object@intent_code <- readBin(con,'int',1,2,endian=endian)
  object@datatype <- readBin(con,'int',1,2,endian=endian)
  object@bitpix <- readBin(con,'int',1,2,endian=endian)
  object@slice_start <- readBin(con,'int',1,2,endian=endian)
  object@pixdim <- readBin(con,'double',8,4,endian=endian)
  object@vox_offset <- readBin(con,'double',1,4,endian=endian)
  object@scl_slope <- readBin(con,'double',1,4,endian=endian)
  object@scl_inter <- readBin(con,'double',1,4,endian=endian)
  object@slice_end <- readBin(con,'int',1,2,endian=endian)
  object@slice_code <- readChar(con,1,TRUE)
  object@xyzt_units <- readChar(con,1,TRUE)
  object@cal_min <- readBin(con,'double',1,4,endian=endian)
  object@cal_max <- readBin(con,'double',1,4,endian=endian)
  object@slice_duration <- readBin(con,'double',1,4,endian=endian)
  object@toffset <- readBin(con,'double',1,4,endian=endian)
  object@glmax <- readBin(con,'int',1,4,endian=endian)
  object@glmin <- readBin(con,'int',1,4,endian=endian)

  object@descrip <- readChar(con,80,TRUE)
  object@aux_file <- readChar(con,24,TRUE)

  object@qform_code <- readBin(con,'int',1,2,endian=endian)
  object@sform_code <- readBin(con,'int',1,2,endian=endian)

  object@quatern_b <- readBin(con,'double',1,4,endian=endian)
  object@quatern_c <- readBin(con,'double',1,4,endian=endian)
  object@quatern_d <- readBin(con,'double',1,4,endian=endian)
  object@qoffset_x <- readBin(con,'double',1,4,endian=endian)
  object@qoffset_y <- readBin(con,'double',1,4,endian=endian)
  object@qoffset_z <- readBin(con,'double',1,4,endian=endian)

  object@srow_x <- readBin(con,'double',4,4,endian=endian)
  object@srow_y <- readBin(con,'double',4,4,endian=endian)
  object@srow_z <- readBin(con,'double',4,4,endian=endian)

  object@intent_name <- readChar(con,16,TRUE)
  object@magic <- readChar(con,4,TRUE)
  object@extender <- readChar(con,4,TRUE)
  object@file_type <- "NIFTI"
  id <- 1

  if (object@extender != "")
  {
    while (1)
    {
      e_size = readBin(con,'int',1,4,endian=endian);
      if (length(e_size) == 0)
      {
        break;
      }
      object@extention$esize[[id]] <- readBin(con,'int',1,4,endian=endian)
      object@extention$ecode[[id]] <- readBin(con,'int',1,4,endian=endian)
      object@extention$edata[[id]] <- readChar(con,object@extention$esize[[id]] - 8,TRUE)
      bp <- bp + object@extention$esize[[id]]
      id <- id + 1
    }
  }

  dx <- object@dim_[2]
  dy <- object@dim_[3]
  dz <- object@dim_[4]
  dt <- object@dim_[5]
  dd <- object@dim_[6]

  if (object@datatype == 1)   #binary
  {
    type <- "raw"
    size <- 1  #may be wrong
    signed <- TRUE
  }

  else if (object@datatype == 2)  #unsigned char
  {
    type <- "int"
    signed <- FALSE
    size <- if (object@bitpix) object@bitpix / dd / 8 else 1
  }
  else if (object@datatype == 4)  #signed short
  {
    type <- "int"
    signed <- TRUE
    size <- if (object@bitpix) object@bitpix / dd / 8 else 2
  }
  else if (object@datatype == 8) #signed int
  {
    type <- "int"
    signed <- TRUE
    size <- if (object@bitpix) object@bitpix / dd / 8 else 4
  }

  else if (object@datatype == 16) #float
  {
    type <- "double"
    signed <- TRUE
    size <- if (object@bitpix) object@bitpix / dd / 8 else 4
  }
  else if (object@datatype == 32) #complex
  {
    type <- "complex"
    signed <- TRUE
    size <- if (object@bitpix) object@bitpix / dd / 8 else 8
  }
  else if (object@datatype == 64) #double
  {
    type <- "double"
    signed <- TRUE
    size <- if (object@bitpix) object@bitpix / dd / 8 else 8
  }

  else if (object@datatype == 128) #RGB
  {
    type <- "int"
    signed <- TRUE
    size <- if (object@bitpix) object@bitpix / dd / 8 else 3
  }
  else
  {
    type <- 'int'
    signed <- TRUE
    size <- 1
  }

  close(con)
  con <- file(paste(filename,'.img',sep=""),'rb')

  object@image <- array(readBin(con,type,dx * dy * dz * dt * dd, size, signed = signed, endian = endian) ,dim =
                          if(object@dim_[1] == 5) c(dx,dy,dz,dt,dd)
                        else if (object@dim_[1] == 4) c(dx,dy,dz,dt)
                        else if (object@dim_[1] == 3) c(dx,dy,dz)
                        else if (object@dim_[1] == 2) c(dx,dy)
  )

  close(con)
  object
}
)

read.NIFTI.hdr_img <- function(filename) #no .img and .hdr in filename
{
  a <- new("NIdata")
  read.hdr_img(a,filename)

}

