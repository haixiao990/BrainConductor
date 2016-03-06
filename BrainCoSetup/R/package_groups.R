
##lite_group <- function()
##{
##    c("")    
##}

##MedicalIMG_group <- function()
##{
##    c("")
##}

##task2_group <- function()
##{
##    c("")
##}
.MedicalImaging_group_pkglist <- function(coreOnly = NULL)
{
  if (!suppressWarnings(require("ctv", quietly=TRUE))) {
    install.packages("ctv")
    suppressPackageStartupMessages (require("ctv", quietly=TRUE))
  } else {
    suppressPackageStartupMessages (require("ctv", quietly=TRUE))    
  }   
  pkgs <- ctv:::.get_pkgs_from_ctv_or_repos(views = "MedicalImaging", 
                                          coreOnly = coreOnly, 
                                          repos = NULL)
}
  
MedicalImaging_group <- function(coreOnly = TRUE)
{
    pkgs <- .MedicalImaging_group_pkglist(coreOnly = coreOnly)
    pkgs <- pkgs[[rownames(pkgs)]]
}

all_BCo_group <- function()
{
    ##remember to modify the contribUrl when the website is ready
    contribUrl <- paste(BCoInstallRepos()['BrainCo_soft'], "src/contrib",
                        sep="/")
    
    pkglist = available.packages(contribUrl)
    pkgs = rownames(pkglist)
}


