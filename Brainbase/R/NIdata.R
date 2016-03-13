#S4 NIdata class for Neuroimaging data
#For details on Fields in NIFTI header, Refer to: http://nifti.nimh.nih.gov/nifti-1/documentation/nifti1fields

#set the data classes, either 2D or 4D
.BCoData2D <- setClass("BCoData2D", representation(mat = "matrix", mask = "numeric", base.dim =
  "numeric"))

.BCoData4D <- setClass("BCoData4D", representation(mat = "array"))

.BCoData2DReduc <- setClass("BCoData2DReduc", representation(mat = "matrix", col.mapping = "numeric", 
  type = "character"))

setClassUnion("BCoData", c("BCoData2D", "BCoData4D", "BCoData2DReduc"))

#set the base class
.BCoBase <- setClass("BCoBase", representation(data = "BCoData", notes = "character"))

.scanner_info <- setClass("scanner_info",
         representation("sizeof_hdr"="numeric",
                        "data_type"="character",
                        "db_name"="character",
                        "extents"="numeric",
                        "session_error"="numeric",
                        "regular"="character",
                        "dim_info"="character",
                        "dim_"="vector",
                        "intent_p1"="numeric",
                        "intent_p2"="numeric",
                        "intent_p3"="numeric",
                        "intent_code"="numeric",
                        "datatype"="numeric",
                        "bitpix"="numeric",
                        "slice_start"="numeric",
                        "pixdim"="vector",
                        "vox_offset"="numeric",
                        "scl_slope"="numeric",
                        "scl_inter"="numeric",
                        "slice_end"="numeric",
                        "slice_code"="character", # character?
                        "xyzt_units"="character", # character?
                        "cal_max"="numeric",
                        "cal_min"="numeric",
                        "slice_duration"="numeric",
                        "toffset"="numeric",
                        "glmax"="numeric",
                        "glmin"="numeric",
                        "descrip"="character",
                        "aux_file"="character",
                        "qform_code"="numeric",
                        "sform_code"="numeric",
                        "quatern_b"="numeric",
                        "quatern_c"="numeric",
                        "quatern_d"="numeric",
                        "qoffset_x"="numeric",
                        "qoffset_y"="numeric",
                        "qoffset_z"="numeric",
                        "srow_x"="vector",
                        "srow_y"="vector",
                        "srow_z"="vector",
                        "intent_name"="character",
                        "magic"="character",
                        "extender"="character",
                        "extention"="list",
                        "file_type" = "character",
                        "image" = "array"),
         prototype("sizeof_hdr"=348,
                   "data_type"="",
                   "db_name"="",
                   "extents"=numeric(1),
                   "session_error"=numeric(1),
                   "regular"="",
                   "dim_info"="",
                   "dim_"=numeric(8),
                   "intent_p1"=numeric(1),
                   "intent_p2"=numeric(1),
                   "intent_p3"=numeric(1),
                   "intent_code"=numeric(1),
                   "datatype"=2,
                   "bitpix"=8,
                   "slice_start"=numeric(1),
                   "pixdim"=numeric(8),
                   "vox_offset"=352,
                   "scl_slope"=numeric(1),
                   "scl_inter"=numeric(1),
                   "slice_end"=numeric(1),
                   "slice_code"="",
                   "xyzt_units"="",
                   "cal_max"=numeric(1),
                   "cal_min"=numeric(1),
                   "slice_duration"=numeric(1),
                   "toffset"=numeric(1),
                   "glmax"=numeric(1),
                   "glmin"=numeric(1),
                   "descrip"="",
                   "aux_file"="",
                   "qform_code"=numeric(1),
                   "sform_code"=numeric(1),
                   "quatern_b"=numeric(1),
                   "quatern_c"=numeric(1),
                   "quatern_d"=numeric(1),
                   "qoffset_x"=numeric(1),
                   "qoffset_y"=numeric(1),
                   "qoffset_z"=numeric(1),
                   "srow_x"=numeric(4),
                   "srow_y"=numeric(4),
                   "srow_z"=numeric(4),
                   "intent_name"="",
                   "magic"="n+1",
                   "extender"="",
                   "extention"=list(esize=0,ecode=0,edata=""),
                   "image"=array(1:4,dim=c(2,2)))

)

#set the NIdata type

.NIdata <- setClass("NIdata",
 representation(phenotype = "list", scanner_info = "scanner_info", extra = "list", 
 ID = "character"), contains = "BCoBase")

.Template <- setClass("Template", contains = "BCoBase")

#WARNING: Now that I think about it, these three might be just
# effectively all be the same...
setClass("Parcellation", representation(names = "data.frame"), contains = "BCoBase")
setClass("RegionofInterest", contains = "BCoBase")
setClass("TissuePriors", representation(tissue = "character"), 
  contains = "BCoBase", prototype(data = list()))

