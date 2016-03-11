compute.meanFunctional <- function(img){
  assert_that(length(dim(img))==4)
  
  apply(img, c(1:3), mean)
}
