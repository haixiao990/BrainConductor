BCoClassifier.default <- function(subj.vec, custom.arg){
  pheno.vec = sapply(subj.vec, function(x){
    get.phenotype(eval(as.name(subj.vec[[i]])))[1]
  })
 
  assert_that(all(class(pheno.vec) == "character"))

  pheno.uniq = unique(pheno.vec)
  pheno.list = vector("list", length(pheno.uniq))
  names(pheno.list) = pheno.uniq

  for(i in 1:length(pheno.uniq)){
    pheno.list[[i]] = subj.vec[which(pheno.vec == pheno.uniq[i])]
  }

  pheno.list
}

#input: a list of NIdata
BCoSubject.analysis.default <- function(subj.vec, custom.arg){
  #WARNING: Should allow parallel computing
  #WARNING: need to run checks here

  res.list = vector("list", length(subj.list))
  names(res.list) = subj.vec

  for(i in 1:length(subj.list)){
    obj = perturb.columns(eval(as.name(subj.list[[i]])))
    res.list[[i]] = huge(get.matrix(obj))
  }

  res.list
}

BCoDifference.default <- function(res.list, custom.arg){
  assert_that(length(res.list) == 2)

  graph.difference(res.list)
}

BCoPopulation.aggregate.default <- function(res.list, category.list, custom.arg){
  assert_that(length(category.list) == 2) 

 aggregate.res.list = vector("list", 2)
  names(aggregate.res.list) = names(category.list)

  for(i in 1:2){
    idx = which(names(res.list) %in% category.list[[i]])
    aggregate.res.list[[i]] = median.graph(res.list[idx])
  }
 
  aggregate.res.list
}

.Pipelinecontrol <- setClass("Pipelinecontrol", representation(pairDiff.first =
  "logical", verbose = "logical"), prototype(pairDiff.first = F, verbose = T))

.convert.list2Pipelinecontrol <- function(lis){
  con = .Pipelinecontrol()

  if(!is.null(lis$pairDiff.first)) con@pairDiff.first = lis$pairDiff.first
  if(!is.null(lis$verbose)) con@verbose = lis$verbose

  con
}

#WARNING: allow custom arguments
#need to figure out how to do case control pairing
BCoPopulation.analysis <- function(BCoSubjectFinder = BCoSubjectFinder.default, 
  BCoClassifier = BCoClassifier.default, 
  BCoSubject.analysis = BCoSubject.analysis.default, 
  BCoPopulation.aggregate = BCoPopulation.aggregate.default,
  BCoDifference = BCoDifference.default,
  controls = list(pairDiff.first = F, verbose = T),
  custom.arg = list()){

  con = .convert.list2Pipelinecontrol(controls)

  #use BCoSubjectFinder to locate all the subjects
  ## it can either be a list of variable names or a function to find the variable names
  if(class(BCoSubjectFinder) == "function"){
    subj.vec = BCoSubjectFinder()

  } else if(class(BCoSubjectFinder) == "character"){
    subj.vec = BCoSubjectFinder
  }


  if(con@verbose) print(paste0("Successfully found and populated all the", length(subj.vec), " subjects."))

  #use BCoClassifier to seperate subjects into the phenotypes
  ##BCoClassifier takes as input a list of NIdata objects and returns a list of control indicies and 
  ## case indicies
  ## WARNING: Be aware the BCoClassifier could be a matrix of case-control
  ##  pairings
  if(class(BCoClassifier) == "function"){
    category.list = BCoClassifier(subj.vec, custom.arg)
  } else if(class(category.list) == "matrix"){
    category.list = BCoClassifier
  }

  if(con@verbose) print(paste0("Successfully classified all the", length(subj.vec), " subjects."))

  #use BCoSubject.analysis to perform analysis on each subject
  res.list = BCoSubject.analysis(subj.vec, custom.arg)

  if(con@verbose) print(paste0("Successfully subject-level analysis on all the", length(subj.vec), " subjects."))

  #use BCoPopulation.aggregate to aggregate all the subject results somehow
  if(con@pairDiff.first & !is.null(case.control.pairing)){
    diff.list = BCoDifference(res.list, category.list, custom.arg)
    return(BCoPopulation.aggregate(diff.list, custom.arg))

  } else {
    pop.list = BCoPopulation.aggregate(res.list, category.list, custom.arg)
    return(BCoDifference(pop.list, custom.arg))    
  }

  if(con@verbose) print(paste0("Successfully aggregated and contrasted results."))
}
