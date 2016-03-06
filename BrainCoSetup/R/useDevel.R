.BCoUpgrade <-
    function()
{
    if (!IS_UPGRADEABLE) {
        .stop("Brainconductor version %s cannot be upgraded with
               R version %s", BrainCoVersion(), R_VERSION)
    }
    
    ##No Devel version is available now 
    
    ##if (IS_UPGRADEABLE && UPGRADE_IS_DEVEL)
    ##    .stop("BrainConductor version %s can be upgraded, but only to 'devel';
    ##           see ?useDevel. Use BCInstall() without any arguments to update
    ##           installed packages", BrainCoVersion())

    ##No upgradeable version for BrainConductor right now.
    ##txt <- sprintf("Upgrade all packages to BrainConductor version %s? [y/n]: ",
    ##               UPGRADE_VERSION)
    ##answer <- .getAnswer(txt, allowed = c("y", "Y", "n", "N"))
    ##if ("y" == answer)
    ##    .update(UPGRADE_VERSION, TRUE)
}


## The following lines will be uncomment in a new version of BrainCoSetup package
## when there is upgradeable released or devel version of BrainConductor

##useDevel <-
##    function(devel=TRUE)
##{
##    if (devel) {
##        if (!IS_USER)
##            .stop("'devel' version already in use")
##        if (IS_END_OF_LIFE)
##            .stop("'devel' version not available")
##        if (!IS_UPGRADEABLE)
##            .stop("'devel' version requires a more recent R")
##        BCoVers <- UPGRADE_VERSION
##  } else {
##        if (IS_USER)
##            .stop("'devel' version not in use")
##        if (!IS_DOWNGRADEABLE)
##            .stop("'devel' version cannot be down-graded with this version of R")
##      BCoVers <- DOWNGRADE_VERSION
##    }
##    .update(BCoVers, FALSE)
##}

##.update <-
##    function(BCoVers, biocLiteAfterUpdate = FALSE)
##{
##    .msg("before, version is %s", packageVersion("BrainCoSetup"))
##    SetupPackageUpdate <-
##        function()
##    {
##        if (nchar(Sys.getenv("BRAINCOSETUP_TEST_REPOS")))
##            contribUrl = Sys.getenv("BRAINCOSETUP_TEST_REPOS")

##        if ("package:BrainCoSetup" %in% search())
##            detach("package:BrainCoSetup", unload=TRUE, force=TRUE)
        ## contribUrl will be in SetupPackageUpdate's environment
##        suppressWarnings(tryCatch({
##            install.packages("BrainCoSetup", contriburl=contribUrl)
##        }, error=function(err) {
##            assign("failed", TRUE, "BCoSetupEnv")
##            NULL
##        }))
##        library(BrainCoSetup)
##        BrainCoSetup:::.updateFinish()
##    }
##    BCoSetupEnv <- new.env()
##    BCoSetupEnv[["contribUrl"]] <- .getContribUrl(BrainCoVersion)
##    BCoSetupEnv[["BCInstallAfterUpdate"]] <- BCInstallAfterUpdate
##    .stepAside(BCoSetupEnv, SetupPackageUpdate)
##}

##.updateFinish <-
##    function()
##{
##    failed <- exists("failed", "BCoSetupEnv")
##    BCInstallAfterUpdate <- get("BCInstallAfterUpdate", "BCoSetupEnv")
##    detach("BCoSetupEnv")
##    .msg("after, version is %s", packageVersion("BrainCoSetup"))
##    vers <- packageVersion("BrainCoSetup")
##    if (!failed) {
##        .msg("'BrainCoSetup' changed to version %s", vers)
##        if (BCInstallAfterUpdate)
##            BCInstall(character(), ask=FALSE)
##    } else
##        .warning("update failed, using BrainCoSetup version %s",
##                 vers, call.=FALSE)
##}
