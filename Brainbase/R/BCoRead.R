#Our generic read function
#"controls" is a list which contains preprocessing commands
BCoRead <- function(input, template = NULL, mask = NULL, subject.ID = "",
  controls = list(convert2D = F, method = "mean", verbose = F)){

  assert_that(is.null(template) || class(template) == "Template")

  con = .convert.list2control(controls, "Readcontrol")

  #determine if the 'input' file is DICOM or NIfTI
  file.ending = strsplit(input, "\\.")[[1]]
  file.ending = file.ending[length(file.ending)]
  assert_that(file.ending %in% c(".dcm", "gz", "nii"))

  #if dicom, read dicom reader and convert into 
  if(file.ending == ".dcm"){
    
    #WARNING: The following code might not work, but should be something like this:
    dat = read.DICOM.dcm(input)

  #if nifti, use the nifti functions
  } else {
    
    dat = readNIfTI(input)
  }

  res = .NIdata(.BCoBase(data = .BCoData4D(mat = dat@.Data)), 
   scanner_info = .create.scaninfo(dat), ID = subject.ID)

  #immediately convert to 2D matrix if it is dictated in "controls"
  if(con@convert2D){
    res = convert.4Dto2D(res, template = template, verbose = con@verbose)
  }

  #immediately apply a mask if one is supplied
  if(!is.null(mask) & con@convert2D){
    res = BCoReduce(res, mask, con@method, con@verbose)
  }

  res
}

BCoReadandAssign <- function(input, variable.header, 
 template = NULL, mask = NULL,
 controls = list(convert2D = F, method = "mean", verbose = F), subject.ID = ""){

  res = BCoRead(input, template = template, mask = mask,
   subject.ID = subject.ID, controls = controls)
  
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
