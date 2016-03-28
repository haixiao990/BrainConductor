#DOES THIS NEED TO BE AN S4 FUNCTION?

.NIcontrol <- setClass("NIcontrol", representation(convert2D = "logical",
  method = "function", verbose = "logical"),
  prototype(convert2D = F, method = .reduction.mean, verbose = F))

convert.list2NIcontrol <- function(lis){
  con = .NIcontrol()
  
  if(!is.null(lis$convert2D)) con@convert2D = lis$convert2D
  if(!is.null(lis$verbose)) con@verbose = lis$verbose

  if(!is.null(lis$method)){
    if(class(lis$method) == "function") con@method = lis$method
    if(lis$method == "mean") con@method = .reduction.mean
    if(lis$method == "pca") con@method = .reduction.pca
  }
   
  con
}

convert.nifti2nidata <- function(dat){
  res = .NIdata(.BCoBase(data = .BCoData4D(mat = dat@.Data)),
   scanner_info = .create.scaninfo(dat), ID = subject.ID)
}

convert.nidata2nifti <- function(obj){
  #WARNING: need to check obj has a BCoData2D or BCoData4D

  dat = new('nifti')
  
  #WARNING: NEED A CONVERSION BACK TO 4D IF NEEDED

  dat@.Data = obj@data
  scaninfo = obj@scanner_info
  
  dat@aux_file = scaninfo@aux_file
  dat@bitpix = scaninfo@bitpix
  dat@cal_max = scaninfo@cal_max
  dat@cal_min = scaninfo@cal_min
  dat@data_type = scaninfo@data_type
  dat@datatype = scaninfo@datatype
  dat@db_name = scaninfo@db_name
  dat@descrip = scaninfo@descrip
  dat@dim_ = scaninfo@dim_
  dat@dim_info = scaninfo@dim_info
  dat@extender = scaninfo@extender
  dat@extents = scaninfo@extents
  dat@glmax = scaninfo@glmax
  dat@glmin = scaninfo@glmin
  dat@intent_code = scaninfo@intent_code
  dat@intent_name = scaninfo@intent_name
  dat@intent_p1 = scaninfo@intent_p1
  dat@intent_p2 = scaninfo@intent_p2
  dat@intent_p3 = scaninfo@intent_p3
  dat@magic = scaninfo@magic
  dat@pixdim = scaninfo@pixdim
  dat@qform_code = scaninfo@qform_code
  dat@qoffset_x = scaninfo@qoffset_x
  dat@qoffset_y = scaninfo@qoffset_y
  dat@qoffset_z = scaninfo@qoffset_z
  dat@quatern_b = scaninfo@quatern_b
  dat@quatern_c = scaninfo@quatern_c
  dat@quatern_d = scaninfo@quatern_d
  dat@regular = scaninfo@regular
  dat@reoriented = scaninfo@reoriented
  dat@scl_inter = scaninfo@scl_inter
  dat@scl_slope = scaninfo@scl_slope
  dat@session_error = scaninfo@session_error
  dat@sform_code = scaninfo@sform_code
  dat@sizeof_hdr = scaninfo@sizeof_hdr
  dat@slice_code = scaninfo@slice_code
  dat@slice_duration = scaninfo@slice_duration
  dat@slice_end = scaninfo@slice_end
  dat@slice_start = scaninfo@slice_start
  dat@srow_x = scaninfo@srow_x
  dat@srow_y = scaninfo@srow_y
  dat@srow_z = scaninfo@srow_z
  dat@toffset = scaninfo@toffset
  dat@vox_offset = scaninfo@vox_offset
  dat@xyzt_units = scaninfo@xyzt_units

  dat
}


.create.scaninfo <- function(dat){
  scaninfo = .scanner_info()

  scaninfo@aux_file = dat@aux_file
  scaninfo@bitpix = dat@bitpix
  scaninfo@cal_max = dat@cal_max
  scaninfo@cal_min = dat@cal_min
  scaninfo@data_type = dat@data_type
  scaninfo@datatype = dat@datatype
  scaninfo@db_name = dat@db_name
  scaninfo@descrip = dat@descrip
  scaninfo@dim_ = dat@dim_
  scaninfo@dim_info = dat@dim_info
  scaninfo@extender = dat@extender
  scaninfo@extents = dat@extents
  scaninfo@glmax = dat@glmax
  scaninfo@glmin = dat@glmin
  scaninfo@intent_code = dat@intent_code
  scaninfo@intent_name = dat@intent_name
  scaninfo@intent_p1 = dat@intent_p1
  scaninfo@intent_p2 = dat@intent_p2
  scaninfo@intent_p3 = dat@intent_p3
  scaninfo@magic = dat@magic
  scaninfo@pixdim = dat@pixdim
  scaninfo@qform_code = dat@qform_code
  scaninfo@qoffset_x = dat@qoffset_x
  scaninfo@qoffset_y = dat@qoffset_y
  scaninfo@qoffset_z = dat@qoffset_z
  scaninfo@quatern_b = dat@quatern_b
  scaninfo@quatern_c = dat@quatern_c
  scaninfo@quatern_d = dat@quatern_d
  scaninfo@regular = dat@regular
  scaninfo@reoriented = dat@reoriented
  scaninfo@scl_inter = dat@scl_inter
  scaninfo@scl_slope = dat@scl_slope
  scaninfo@session_error = dat@session_error
  scaninfo@sform_code = dat@sform_code
  scaninfo@sizeof_hdr = dat@sizeof_hdr
  scaninfo@slice_code = dat@slice_code
  scaninfo@slice_duration = dat@slice_duration
  scaninfo@slice_end = dat@slice_end
  scaninfo@slice_start = dat@slice_start
  scaninfo@srow_x = dat@srow_x
  scaninfo@srow_y = dat@srow_y
  scaninfo@srow_z = dat@srow_z
  scaninfo@toffset = dat@toffset
  scaninfo@vox_offset = dat@vox_offset
  scaninfo@xyzt_units = dat@xyzt_units

  scaninfo 
}
