
setGeneric("write.nii",function(object, filename ,endian )  standardGeneric("write.nii"));

setMethod("write.nii", signature(object = "nifti_one"), function(object, filename, endian )
{
  con = file(filename,'wb')
  writeBin(as.integer(348),con,4,endian=endian)
  writeChar(object@data_type,con,10,NULL)
  writeChar(object@db_name,con,18,NULL)
  writeBin(object@extents,con,4,endian=endian)
  writeBin(object@session_error,con,2,endian=endian)
  writeChar(object@regular,con,1,NULL)
  writeChar(object@dim_info,con,1,NULL)
  writeBin(object@dim_,con,2,endian=endian)
  writeBin(object@intent_p1,con,4,endian=endian)
  writeBin(object@intent_p2,con,4,endian=endian)
  writeBin(object@intent_p3,con,4,endian=endian)
  writeBin(object@intent_code,con,2,endian=endian)
  #writeBin(object@intent_code,con,2,endian=endian)
  writeBin(object@datatype,con,2,endian=endian)
  writeBin(object@bitpix,con,2,endian=endian)
  writeBin(object@slice_start,con,2,endian=endian)
  writeBin(object@pixdim,con,4,endian=endian)
  writeBin(object@vox_offset,con,4,endian=endian)
  writeBin(object@scl_slope,con,4,endian=endian)
  writeBin(object@scl_inter,con,4,endian=endian)
  writeBin(object@slice_end,con,2,endian=endian)
  writeChar(object@slice_code,con,1,NULL)
  writeChar(object@xyzt_units,con,1,NULL)
  writeBin(object@cal_min,con,4,endian=endian)
  writeBin(object@cal_max,con,4,endian=endian)
  writeBin(object@slice_duration,con,4,endian=endian)
  writeBin(object@toffset,con,4,endian=endian)
  writeBin(object@glmax,con,4,endian=endian)
  writeBin(object@glmin,con,4,endian=endian)

  writeChar(object@descrip,con,80,NULL)
  writeChar(object@aux_file,con,24,NULL)

  writeBin(object@qform_code,con,2,endian=endian)
  writeBin(object@sform_code,con,2,endian=endian)

  writeBin(object@quatern_b,con,4,endian=endian)
  writeBin(object@quatern_c,con,4,endian=endian)
  writeBin(object@quatern_d,con,4,endian=endian)
  writeBin(object@qoffset_x,con,4,endian=endian)
  writeBin(object@qoffset_y,con,4,endian=endian)
  writeBin(object@qoffset_z,con,4,endian=endian)

  writeBin(object@srow_x,con,4,endian=endian)
  writeBin(object@srow_y,con,4,endian=endian)
  writeBin(object@srow_z,con,4,endian=endian)

  writeChar(object@intent_name,con,16,NULL)
  writeChar(object@magic,con,4,NULL)
  writeChar(object@extender,con,4,NULL)

  if (object@extender != "")
  {
    for (id in 1 : length(object@extention$esize))
    {
      writeBin(object@extention$esize[[id]],con,4,endian=endian)
      writeBin(object@extention$ecode[[id]],con,4,endian=endian)
      writeChar(object@extention$edata[[id]],con,object@extention$esize[[id]]-8,NULL)
    }
  }

  dx <- object@dim_[2]
  dy <- object@dim_[3]
  dz <- object@dim_[4]
  dt <- object@dim_[5]
  dd <- object@dim_[6]

  if (object@datatype == 1)   #binary
  {
    size <- if (object@bitpix) object@bitpix / dd / 8 else 1
    writeBin(as.raw(object@image),con,size,endian=endian)
  }

  else if (object@datatype == 2)  #unsigned char
  {
    size <- if (object@bitpix) object@bitpix / dd / 8 else 1
    writeBin(as.integer(object@image),con,size,endian=endian)
  }
  else if (object@datatype == 4)  #signed short
  {
    size <- if (object@bitpix) object@bitpix / dd / 8 else 2
    writeBin(as.integer(object@image),con,size,endian=endian)
  }
  else if (object@datatype == 8) #signed int
  {
    size <- if (object@bitpix) object@bitpix / dd / 8 else 4
    writeBin(as.integer(object@image),con,size,endian=endian)
  }

  else if (object@datatype == 16) #float
  {
    size <- if (object@bitpix) object@bitpix / dd / 8 else 4
    writeBin(as.double(object@image),con,size,endian=endian)
  }
  else if (object@datatype == 32) #complex
  {
    size <- if (object@bitpix) object@bitpix / dd / 8 else 8
    writeBin(as.complex(object@image),con,size,endian=endian)
  }
  else if (object@datatype == 64) #double
  {
    size <- if (object@bitpix) object@bitpix / dd / 8 else 8
    writeBin(as.double(object@image),con,size,endian=endian)
  }

  else if (object@datatype == 128) #RGB
  {
    size <- if (object@bitpix) object@bitpix / dd / 8 else 3
    writeBin(as.integer(object@image),con,size,endian=endian)
  }
  else
  {
    size <- 1
    writeBin(as.integer(object@image),con,size,endian=endian)
  }
  close(con)
}
)

write.NIFTI.nii <- function(Object,filename,endian='big')
{
  write.nii(Object,filename, endian = endian)
}


