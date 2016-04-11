setClass("CustomObj", representation(value = "numeric", env = "environment"),
  prototype(value = 5, env = new.env()))

setGeneric("modify", function(obj) standardGeneric("modify"))
setMethod("modify", signature("CustomObj"), function(obj){
  obj@value = 10
  obj@env$reference = 10

  obj
})

x = new("CustomObj")
x@env$reference = 5
y = modify(x)

x@value
y@value
#we see that since obj@value is passed by value, even when we modify
#  x@value and set the result to y, x@value remains 5

x@env$reference
y@env$reference
#however, since "environments" in R are passed by reference, when
#  we modify x@env$reference even within the function, x@env$reference
#  is changed globally. Thus, currently, x@env$reference and y@env$reference
#  are pointing to the same memory location
x@env
y@env
#we see it points to the same memory location
