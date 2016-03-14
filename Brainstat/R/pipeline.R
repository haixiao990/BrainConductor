BCoClassifier.default <- function(){
}

BCoSubject.analysis.default <- function(){
}

BCoPopulation.aggregate.default <- function(){
  
}

#need to figure out how to do case control pairing
BCoPopulation.analysis <- function(BCoClassifier = BCoClassifier.default, 
  BCoSubject.analysis = BCoSubject.analysis.default, 
  BCoPopulation.aggregate = BCoPopulation.aggregate.default,
  case.control.pairing = NULL,){
}
