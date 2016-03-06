## Mirrors: No optional mirrors for BrainConductor right now
## Remember to change the BrainConductor repository when the website is set.

local({
  Rvers <- getRversion()
  REQUIRED_R_VERSION <- "3.1.0"
  BrainCoVers <- tryCatch({
    BrainCoSetup::BrainCoVersion() # The version of local BrainConductor
  }, error=function(...) {         # No Local BrainConductor
    # Manually set to the current version, always change version number when updating this file!
    numeric_version("1.1")         
  })

  if ("package:BiocInstaller" %in% search()) {
    ## startup messages if already attached
    tryCatch(BrainCoSetup:::.startupmsg(), error=function(...) NULL)
  }
  
  if (Rvers >= REQUIRED_R_VERSION ) {
    if (exists("BCoInstall", .GlobalEnv, inherits=FALSE)) {
      txt <- strwrap("An outdated BCoInstall() function exists in the
                     global environment; run 'rm(BCinstall)' and try again.")
      stop("\n", paste(txt, collapse="\n"))
    }
    
    if (!suppressWarnings(require("BrainCoSetup", quietly=TRUE))) {
      
      if (!"package:utils" %in% search()) {
          url <- "http://brainconductor.org/BCinstall.R"
          txt <- sprintf("use 'source(\"%s\")' to update 'BrainCoSetup'
                         after 'utils' package is attached",
                        url)
          message(paste(strwrap(txt), collapse="\n  "))
      } else {
     
        ##remember to change this BrainConductor repository when the website is set.
        BrainCoRepRoot = "http://localhost/packages/BrianCo"
        install.packages("BrainCo", repos=BrainCoRepRoot)
        if (!suppressWarnings(require("BrainCoSetup",
                                      quietly=TRUE))) {
          url0 <- "http://www.brainconductor.org/packages/BrainCo"
      ##    url <- sprintf("%s/%s/",
      ##                   url0, as.character(BrainCoVers))
          txt0 <- "'BCinstall.R' failed to install 'BrainCoSetup',
          use 'install.packages(\"%s\", repos=\"%s\")'"
          txt <- sprintf(txt0, "BrainCoSetup", url0)
          message(paste(strwrap(txt), collapse="\n  "))
        }
      }
    }
  }
  else {
  ## need to change to "3.1" or above version of R
  stop("BrainConductor requires an R version greater than ",
       REQUIRED_R_VERSION,".\n",
       "To install the latest version of R, please see:\n",
       "http://www.r-project.org/\n")
  } 
  
})
