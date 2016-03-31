BCoClassifier.default <- function(){
 variables = ls(.GlobalEnv)
 varClasses = sapply(variables, function(x){
   class(eval(as.name(x)))
 })

  #find the NIdata variables
  idx.NIdata = which(varClasses == "NIdata")

  variables[idx.NIdata]
}
#WARNING: let's use this in BCoLink.Phenotype

BCoSubject.analysis.default <- function(){
}

BCoPopulation.aggregate.default <- function(){
  
}

#need to figure out how to do case control pairing
BCoPopulation.analysis <- function(BCoSubjectFinder = BCoSubjectFinder.default, 
  BCoClassifier = BCoClassifier.default, 
  BCoSubject.analysis = BCoSubject.analysis.default, 
  BCoPopulation.aggregate = BCoPopulation.aggregate.default,
  BCoDifference = BCoDifference.default,
  case.control.pairing = NULL,
  controls = list(pairDiff.first = F, verbose = T)){

  #WARNING: need the control function
  #WARNING: need to check case.control makes sense

  #use BCoSubjectFinder to locate all the subjects
  ## it can either be a list of variable names or a function to find the variable names
  if(class(BCoSubjectFinder) == "function"){
    subj.vec = BCoSubjectFinder()

  } else if(class(BCoSubjectFinder) == "character"){
    subj.vec = BCoSubjectFinder
  }

  subj.list = vector("list", length(subj.vec))
  for(i in 1:length(sub.list)){
    subj.list[[i]] = eval(parse(text = subj.vec[i]))
  }


  if(con@verbose) print(paste0("Successfully found and populated all the", length(subj.vec), " subjects."))

  #use BCoClassifier to seperate subjects into the phenotypes
  ##BCoClassifier takes as input a list of NIdata objects and returns a list of control indicies and 
  ## case indicies
  category.list = BCoClassifier(subj.list)

  #use BCoSubject.analysis to perform analysis on each subject
  res.list = BCoSubject.analysis(subj.list)

  #use BCoPopulation.aggregate to aggregate all the subject results somehow
  if(con@pairDiff.first & !is.null(case.control.pairing)){
    diff.list = BCoDifference(res.list, category.list, case.control.pairing)
    return(BCoPopulation.aggregate(diff.list))

  } else {
    pop.list = BCoPopulation.aggregate(res.list, category.list)
    return(BCoDifference(pop.list))    
  }

}
