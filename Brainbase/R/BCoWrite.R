setGeneric("BCoWrite", function(obj, ...) standardGeneric("convert.4Dto2D"))

setMethod("BCoWrite", signature("NIdata"), function(obj, filename.prefix, 
  type = c("NIfTI", "RData"), 
  controls = list(onefile = T, gzipped = T, verbose = F, warn = -1)){

  #need to check type is valid
  type = type[1]  

  if(type == "RData"){
    save(obj, file = paste0(filename.prefix, ".RData"))
  } else if(type == "NIfTI"){

    #warning: make sure the matrix is 4D or 2D but not 2Dreduc

    nim = convert.nidata2nift(obj)
    writeNIfTI(nim, filename.prefix, controls$onefile, controls$gzipped,
      controls$verbose, controls$warn)
  }

  invisible()
})
