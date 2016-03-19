setGeneric("GenerateGroupDataset", function(Path, SbjList, Mask, ...) standardGeneric("GenerateGroupDataset")) 
#Maybe Mask and Template could also be in NIdata class format, with a specific description slot to indicate data, mask, or ROI template

setMethod("GenerateGroupDataset", signature("character", "character", "NIData"), function(Path, SbjList, template, verbose = TRUE){
  
  
  
})