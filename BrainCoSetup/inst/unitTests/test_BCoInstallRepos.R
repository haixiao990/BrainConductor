repos <- BCoInstallRepos()

#test_BCoInstallRepos_named_repositories <- function()
#{
#
#    allOS <- c("BioCsoft", "CRAN", "BioCann", "BioCexp", "BioCextra")
#    windowsOnly <- "CRANextra"
#
#    checkTrue(all(allOS %in% names(repos)))
#    if (.Platform$OS.type == "windows")
#    {
#        checkTrue(windowsOnly %in% names(repos))
#    } else {
#        checkTrue(!windowsOnly %in% names(repos))
#    }
   
#}

test_BCoInstallRepos_noNA_repositories <- function()
{
    checkTrue(!any(is.na(repos)))
}

#test_BCoInstallRepos_order <- function()
#{
#    checkIdentical("BioCsoft", names(repos)[[1]])
#}
