setGeneric("BCoWrite", function(obj, ...) standardGeneric("convert.4Dto2D"))

setMethod("BCoWrite", signature("NIdata"), function(obj, filename.prefix, 
  type = c("NIfTI", "RData"), 
  controls = list(onefile = T, gzipped = T, verbose = F, warn = -1)){

  assert_that(type %in% c("NIfTI", "RData") & length(type) == 1)
  type = type[1]  

  if(type == "RData"){
    save(obj, file = paste0(filename.prefix, ".RData"))
  } else if(type == "NIfTI"){

    assert_that(class(obj@data) == "BCoData2D" | class(obj@data) == "BCoData4D")

    nim = convert.nidata2nift(obj)
    writeNIfTI(nim, filename.prefix, controls$onefile, controls$gzipped,
      controls$verbose, controls$warn)
  }

  invisible()
})
