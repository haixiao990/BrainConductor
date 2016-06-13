#WARNING: could probably put in "options" a global variable of the template
#tab is a data.frame (supposedly from a csv file already loaded in R)
BCoLink.phenotype <- function(tab, subject.ID.col, kept.column.idx){
  assert_that(class(tab) == "data.frame")
  assert_that(class(tab[,subject.ID.col]) == "character")
  assert_that(all(!duplicated(tab[,subject.ID.col]))) #make sure no duplicates

  obj.vec = BCoSubjectFinder.default()
  subj.vec = sapply(obj.vec, function(x){
    eval(as.name(x))@ID
  })

  colnam = names(tab)[kept.column.idx]

  #now loop over all rows in tab
  link.phenotype <- function(i){
    idx = which(subj.vec == tab[i, subject.ID.col])

    if(length(idx) > 0){
      #create the list for the phenotype
      lis = as.list(tab[i, kept.column.idx])
      names(lis) = colnam

      #loop over all data from that subject (handle multiple scans)
      for(j in 1:length(idx)){
        NIobj = eval(as.name(variables[idx.NIdata[idx[j]]]))
        assert_that(class(NIobj) == "NIdata")

        NIobj@phenotype = lis


        assign(variables[idx.NIdata[idx[j]]], NIobj, envir = .GlobalEnv)
      }
    }
  }

  sapply(1:nrow(tab), link.phenotype)

  invisible()
}


tabulate.phenotype <- function(){
  obj.vec = BCoSubjectFinder.default()

  #WARNING: We should ensure the phenotypes across same IDs are the same?
  #WARNING: We want to remove all subjects which do not have an ID
  subj.vec = sapply(obj.vec, function(x){
    eval(as.name(x))@ID
  })

  #determine the unique phenotypes
  pheno.list = lapply(obj.vec, function(x){
    eval(as.name(x))@phenotype
  })

  assert_that(length(subj.vec) == length(pheno.list))

  pheno.attributes = unique(unlist(lapply(pheno.list, function(x){
    names(x)
  })))

  tab = data.frame(matrix(nrow = length(subj.vec), nrow = 
   length(pheno.attributes) + 1))
  colnames(tab) = c("Subject.ID", pheno.attributes)

  tab[,1] = subj.vec
  for(i in 1:length(pheno.attributes)){
    vec = rep(NA, length(subj.vec))
    for(j in 1:length(subj.vec)){
      vec[j] = pheno.list[[j]][pheno.attributes[i]] 
      #WARNING: Does this work?
    }
  
    tab[,i+1] = vec
  }

  tab
}
