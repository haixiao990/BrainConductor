#Our generic read function
#WARNING: The interface of this function are to be decided. right now I'm just
# going to implement the hash table part

#NOTE: I don't think the hash table is a good idea after all... This is cuz
# a subject could have multiple scans...?

BCoRead <- function(input, controls = NULL, subject.ID = ""){
  #if(!".BCoHashTable_GLOBAL" %in% ls(.GlobalEnv)){
  #  .BCoHashTable_GLOBAL <<- hash()
  #}

  #TEMPORARY LOAD
  dat = readNIfTI(input)
  res = .NIdata(.BCoBase(.BCoData4D(mat = dat@.Data)), ID = subject.ID)

  #.BCoHashTable_GLOBAL[subject.ID] = 1

  res
}

#tab is a data.frame (supposedly from a csv file already loaded in R)
BCoLink.phenotype <- function(tab, subject.ID.col, kept.column.idx){
  assert_that(class(tab) == "data.frame")
  assert_that(class(tab[,subject.ID.col]) == "character")
  assert_that(all(!duplicated(tab[,subject.ID.col])) #make sure no duplicates

  subject.ID = tab[,subject.ID.col]
  variables = ls(.GlobalEnv)
  varClasses = sapply(variables, function(x){
    class(eval(as.name(x)))
  })

  #find the NIdata variables
  idx.NIdata = which(varClasses == "NIdata")
  subj.vec = sapply(variables[idx.NIdata], function(x){
    eval(as.name(x))@ID
  })

  colnam = names(tab)[kept.column.idx]

  #now loop over all rows in tab
  link.phenotype <- function(i){
    idx = which(subj.vec == tab[i, subject.ID.col])
    
    if(length(idx) > 0){
      for(j in 1:length(idx)){
        NIobj = eval(variables[idx.NIdata[idx]])
        assert_that(class(NIobj) == "NIdata")
    
        #create the list for the phenotype
        lis = as.list(tab[i, kept.column.idx])
        names(lis) = colnam
   
        assign(variables[idx.NIdata[idx]], NIobj)
      }
    }
  }

  sapply(1:nrow(tab), link.phenotype)
 
  invisible()
}
