#WE NEED A SPECIAL PRINT STATEMENT

#Our generic read function
#"controls" is a list which contains preprocessing commands
BCoRead <- function(input, controls = NULL, subject.ID = ""){

  #determine if the 'input' file is DICOM or NIfTI
  file.ending = strsplit(input, "\\.")[[1]]
  file.ending = file.ending[length(file.ending)]
  assert_that(file.ending %in% c(".dcm", "gz", "nii"))

  #if dicom, read dicom reader and convert into 
  if(file.ending == ".dcm"){
    
    #WARNING: The following code does not work, but should be something like this:
    dat = read.DICOM.dcm(input)

  #if nifti, use the nifti functions
  } else {
    
    #WARNING: We want to use our readNIfTI.R function?  
    dat = readNIfTI(input)
  }

  #WARNING; This currently doesn't handle the NIfTI handler
  res = .NIdata(.BCoBase(data = .BCoData4D(mat = dat@.Data)), ID = subject.ID)

  #immediately convert to 2D matrix if it is dictated in "controls"
  if(controls$convert2D == TRUE){
    res = convert.4Dto2D(res)
  }

  #immediately apply a mask if one is supplied
  if(!is.null(controsl$template)){
    #WARNING: How to pass in the arguments for BCoReduce?
    res = BCoReduce(res, controls$template)
  }

  res
}

BCoWrite <- function(obj, type = c("NIfTI", "RData")){

}


BCoReadandAssign <- function(input, variable.header, controls = NULL, subject.ID = ""){
  res = BCoRead(input, controls, subject.ID)
  
  resname = paste0(variable.header, "_", subject.ID)
  resname.mod = resname
 
  #determine if "resname" is unique. If not, add integers after it until it is
  current.vars = ls(.GlobalEnv)
  if(resname.mod %in% current.vars){
    iter = 1

    while(TRUE){
      resname.mod = paste0(resname, "_", iter)
      if(!resname.mod %in% current.vars) break()

      iter = iter + 1
    }
  }

  assign(resname.mod, res, envir = .GlobalEnv)  

  resname.mod
}

#tab is a data.frame (supposedly from a csv file already loaded in R)
BCoLink.phenotype <- function(tab, subject.ID.col, kept.column.idx){
  assert_that(class(tab) == "data.frame")
  assert_that(class(tab[,subject.ID.col]) == "character")
  assert_that(all(!duplicated(tab[,subject.ID.col]))) #make sure no duplicates

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
