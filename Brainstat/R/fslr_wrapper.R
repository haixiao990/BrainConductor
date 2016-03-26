BCoFSLR <- function(obj, function.name, ...) {
  #first convert obj into oro.nifti
  oro.obj = convert.nidata2nifti(obj)

  #then invoke the function.name on obj and pass in the arguments in ...
  
}

#some dirty test
zz = "mean"
eval(parse(text = paste0(zz, "(c(5,10))")))
