#test_useDevel <- function()
#{
#    if (!BiocInstaller:::IS_DOWNGRADEABLE)
#        checkException(useDevel(FALSE), silent=TRUE)
#    if (!BiocInstaller:::IS_UPGRADEABLE) {
#        checkException(useDevel(), silent=TRUE)
#        opts <- options(warn=2); on.exit(options(opts))
#        checkException(biocLite("BiocUpgrade"))
#    }
#}

test_getContribUrl_exist <- function()
{
    fun <- BrainCoSetup:::.getContribUrl
    
    vers <- BrainCoSetup:::BRAINCO_VERSION
    checkTrue(grepl(vers, fun(vers)))
    if (BrainCoSetup:::IS_UPGRADEABLE) {
        vers <- BiocInstaller:::UPGRADE_VERSION
        checkTrue(grepl(vers, fun(vers)))
    }
    if (BrainCoSetup:::IS_DOWNGRADEABLE) {
        vers <- BrainCoSetup:::DOWNGRADE_VERSION
        checkTrue(grepl(vers, fun(vers)))
    }
}
