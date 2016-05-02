.Writecontrol <- setClass("Writecontrol", representation(type = "character", 
  onefile = "logical", gzipped = "logical", verbose = "logical", warn = "numeric"),
  prototype(type = "RData", onefile = T, gzipped = T, verbose = F, warn = -1))

setGeneric("BCoWrite", function(obj, ...) standardGeneric("BCoWrite"))

setMethod("BCoWrite", signature("NIdata"), function(obj, filename.prefix,  
  controls = list(type = "RData", onefile = T, gzipped = T, verbose = F, warn = -1)){

  con = .convert.list2control(controls, "Writecontrol")

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

#find all the NIdata objs in the workspace and save them according to their
# variable name
BCoWrite.all <- function(file.dir = NULL, subj.header = NULL, controls = list(type = "RData", onefile = T, gzipped = T, verbose = F, warn = -1)){
  
  #make sure fir.dir is a valid string
  assert_that(substr(file.dir, nchar(file.dir), nchar(file.dir)) == '/')

  subj.vec = BCoSubjectFinder.default()
  if(!is.null(subj.header)) {
    assert_that(is.character(subj.header) & length(subj.header) == 1)
    subj.vec = grep(subj.header, subj.vec, value = T)
  }

  for(i in subj.vec){
    BCoWrite(eval(as.name(i)), filename.prefix = paste0(file.dir,
     deparse(substitute(i))), controls = controls)

    if(is.null(controls$verbose) || controls$verbose){
      paste0("Writing for ", i, " is complete!")
    }
  }

  invisible()
}
