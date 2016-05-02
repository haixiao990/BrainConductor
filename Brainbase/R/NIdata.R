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


#WARNING: should give warning if there are no 0's
#WARNING: make sure there is only one row
.Template <- setClass("Template", representation(scanner_info = "scanner_info",
  exra = "list"), contains = "BCoBase")

<<<<<<< HEAD
setGeneric("get.matrix", function(obj, ...) standardGeneric("get.matrix"))

setMethod("get.matrix", signature("NIdata"), function(obj){
  get.matrix(obj@data)
})

setMethod("get.matrix", signature("Template"), function(obj){
  get.matrix(obj@data)
})

setMethod("get.matrix", signature("BCoData"), function(obj, output2D = T){
  if(output2D & class(obj) == "BCoData4D"){
    convert.4Dto2D(obj, verbose = F)
  } else {
    obj@mat
  }
})

<<<<<<< HEAD
<<<<<<< HEAD
=======
setMethod("is.functional", function(obj) standardGeneric("is.functional"))
=======
setGeneric("is.functional", function(obj) standardGeneric("is.functional"))
>>>>>>> kevin

setMethod("is.functional", signature("BCoData"), function(obj){
  if(class(obj) == "BCoData4D"){
    if(dim(obj@mat)[4] > 1) return(TRUE) else return(FALSE)
  } else if(class(obj) == "BCoData2D" | class(obj) == "BCoData2DReduc"){
    if(nrow(obj@mat) > 1) return(TRUE) else return(FALSE)
  } else return(FALSE)
})

setMethod("is.functional", signature("NIdata"), function(obj){
  is.functional(obj@data)
})
>>>>>>> refs/remotes/origin/kevin

setGeneric("get.phenotype", function(obj) standardGeneric("get.phenotype"))

setMethod("get.phenotype", signature("NIdata"), function(obj){
  obj@phenotype
})


setMethod("show", "NIdata", function(object){
  if(length(object@ID) == 0) subj.name = "(Unidentified Subject)" else subj.name = paste0("Subject ", object@ID)
  cat(paste0("NIdata object for ", subj.name, "\n"))

  if(all(dim(object@data@mat) == 0)){
    cat("  No data stored in NIdata.\n")
  } else {
    if(class(object@data) == "BCoData2D"){
      perc = length(which(object@data@mat[1,] != 0))

      cat(paste0("  2-Dimensional data representing a 4D image of dimension ",
        paste0(object@data@base.dim, collapse = ", "), ":\n    ", nrow(object@data@mat), 
        " elements in series, ", perc,
        " voxels active (", round(100*perc/ncol(object@data@mat), 2), 
        "% of all voxels in mask).\n"))
    } else if (class(object@data) == "BCoData4D") {
      cat(paste0("  4-Dimensional data of dimension ", 
        paste0(dim(object@data@mat), collapse = ", "), ": ", dim(object@data@mat)[4],
        " elements in series.\n"))
 
    } else if (class(object@data) == "BCoData2DReduc") {
      cat(paste0("  2-Dimensional data reduced from voxel-level data. ",
       nrow(object@data@mat), " elements in series, ", ncol(object@data@mat),
       " different series.\n"))
    }
  }

  cat(paste0("  Object consumes about ", round(as.numeric(object.size(object))/1048576, 2), " megabytes.\n"))

  cat(paste0("  Object has slots: ", paste0(names(getSlots(class(object))), collapse = ", "), ".\n"))
})
=======
>>>>>>> kevin
