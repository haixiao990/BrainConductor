.Writecontrol <- setClass("Writecontrol", representation(type = "character", 
  onefile = "logical", gzipped = "logical", verbose = "logical", warn = "numeric"),
  prototype(type = "RData", onefile = T, gzipped = T, verbose = F, warn = -1))

.convert.list2Writecontrol <- function(lis){
  con = .Writecontrol()
  
  if(!is.null(lis$onefile)) con@onefile = lis$onefile
  if(!is.null(lis$verbose)) con@verbose = lis$verbose
  if(!is.null(lis$gzipped)) con@gzipped = lis$gzipped  
  if(!is.null(lis$warn)) con@warn = lis$warn

  if(!is.null(lis$type)){
    assert_that(lis$type %in% c("RData", "NIfTI"))
    con@type = lis$type
  }
   
  con
}

setGeneric("BCoWrite", function(obj, ...) standardGeneric("BCoWrite"))

setMethod("BCoWrite", signature("NIdata"), function(obj, filename.prefix,  
  controls = list(type = "RData", onefile = T, gzipped = T, verbose = F, warn = -1)){

  con = .convert.list2Writecontrol(controls)

  if(con@type == "RData"){
    save(obj, file = paste0(filename.prefix, ".RData"))

  } else if(con@type == "NIfTI"){

    assert_that(class(obj@data) == "BCoData2D" | class(obj@data) == "BCoData4D")

    nim = convert.nidata2nifti(obj)
    writeNIfTI(nim, filename.prefix, con@onefile, con@gzipped,
      con@verbose, con@warn)
  }

  invisible()
})


