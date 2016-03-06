## The following values need to be updated with each BrainCondector release; 
BRAINCO_VERSION <- package_version("1.0")  # BrainConductor version for this package
R_VERSION <- package_version("3.1.0")      # R version for this package
##IS_USER <- FALSE                         # TRUE if this version of
                                           # BrainConductor is the
                                           # current release version
##IS_END_OF_LIFE <- FALSE                  # TRUE if this version of
                                           # BrainConductor is no longer
                                           # the release version

IS_UPGRADEABLE <- FALSE                    # TRUE if a more recent
                                           # version (release or
                                           # devel) of BrainConductor is
                                           # available for this
                                           # version of R
##UPGRADE_IS_DEVEL <- TRUE                 # TRUE if UPGRADE_VERSION
                                           # is for devel use only
IS_DOWNGRADEABLE <- FALSE                 # TRUE if an older version
                                           # (release or devel) of
                                           # BrainConductor is available
                                           # for this version of R
UPGRADE_VERSION <- package_version("1.1")  # BrainConductor version for
                                           # upgrade, if
                                           # IS_UPGRADEABLE == TRUE
##DOWNGRADE_VERSION <- package_version("2.14") # BrainConductor verson for
                                           # downgrade, if
                                           # IS_DOWNGRADEABLE == # TRUE

## NEXT_R_DEVEL_VERSION <- "3.2.0" # next (not-yet-supported) version of R


.startupmsg <-
    function() 
{
    .msg("BrainConductor version %s (BrainCoSetup %s), ?BCinstall for help",
             BrainCoVersion(), packageVersion("BrainCoSetup"))
    if (IS_UPGRADEABLE)
             .msg("A newer version of BrainConductor is available for
                        this version of R, ?BCoUpgrade for help")
    ##if (IS_END_OF_LIFE) {
    ##    if (IS_UPGRADEABLE)
    ##        .msg("A newer version of BrainConductor is available for
    ##                  this version of R, ?BCoUpgrade for help")
    ##    else
    ##        .msg("A new version of BrainConductor is available after
    ##                  installing the most recent version of R; see
    ##                  http://BrainConductor.org/install")
    ##}
}
