## TODO: modify the functions when the repository is set up

BCoInstallRepos <-
    function(siteRepos=character())
{
    ## siteRepos argument is public, but need the version of BrainConductor internally
    .BCoInstallRepos(siteRepos=siteRepos, braincoversion=BrainCoVersion())
}

.BCoInstallRepos <-
    function(siteRepos=character(), braincoversion)
{
    old.opts <- options("repos")
    on.exit(options(old.opts))

    ## The BrainConductor software is not included in   
    ## the list of available repositories:

    ## CRAN, CRANextra(windows only), 
    ## Omegahat(windows only), 
    ## BioCsoft, BioCann, BioCexp, BioCextra
    ## R-Forge, rforge.net

    ## This function manually add a repository of BrainConductor.

    setRepositories(ind=1:15) # in case more repos are added
    repos <- getOption("repos")

    ## Uncomment these lines when the website is set up.
    ## braincoMirror <- getOption("BrainCo_mirror", "http://bioconductor.org")
    ## braincoPaths <- c(BrainCo_soft="BrainCo")
    ## braincoRepos <- paste(braincoMirror, "packages", braincoversion,
    ##                    braincoPaths, sep="/")
    ## repos[names(braincoPaths)] <- braincoRepos
    
    ## remember to modify these lines when the website is set up.
    repos["BrainCo_soft"]  <- "http://58.96.191.67/packages/BrainCo"
    keepRepos <- if (.Platform$OS.type %in% "windows") {
        c("BrainCo_soft", 
    ##    "R-Forge", 
    ##    "rforge.net", 
          "CRAN", "CRANextra")
    } else {
        c("BrainCo_soft", 
    ##    "R-Forge", 
    ##    "rforge.net",
          "CRAN")
    }
    ########
    
    repos <- repos[keepRepos]
    repos <- subset(repos, !is.na(repos))

    if ("@CRAN@" %in% repos)
        repos["CRAN"] <- "http://cran.fhcrc.org"
    
    c(siteRepos=siteRepos, repos)
}

.BCoInstall <-
    function(pkgs, ask, 
             suppressUpdates, 
             siteRepos=character(),
             lib.loc=.libPaths(), lib=.libPaths()[1], ...)
{

    if (!(is.character(suppressUpdates) || is.logical(suppressUpdates)) ||
        (is.logical(suppressUpdates) && 1L != length(suppressUpdates)))
        .stop("'suppressUpdates' must be character() or logical(1)")

    type <- list(...)[["type"]]
    if (is.null(type))
        type <- getOption("pkgType")

    ## braincoMirror <- getOption("BrainCo_mirror", "http://brainconductor.org")
    ## .msg("BrainCo_mirror: %s", braincoMirror)

    version <- getRversion()
    ##thisRVer <- sprintf("%d.%d", version$major, version$minor)
    .msg("Using Brainconductor version %s (BrainCoSetup %s), R version %s.",
             BrainCoVersion(), packageVersion("BrainCoSetup"), version)

    if (!suppressPackageStartupMessages(require("utils", quietly=TRUE)))
        .stop("cannot load package 'utils'")
    ##if (compareVersion(thisRVer, NEXT_R_DEVEL_VERSION) >= 0)
    ##    .msg("Temporarily using Bioconductor version %s",
    ##             biocVersion())

    repos <- BCoInstallRepos(siteRepos)

    if (length(pkgs)) {
        
        .msg("Installing package(s) '%s'",
                 paste(pkgs, collapse="' '"))
        install.packages(pkgs=pkgs, lib=lib, repos=repos, ...)
    }

    ##early exit if suppressUpdates
    if (is.logical(suppressUpdates) && suppressUpdates)
        return(invisible(pkgs))
    pkgsToUpdate <- old.packages(repos=repos, lib.loc=lib.loc)
    if (is.null(pkgsToUpdate))
        return(invisible(pkgs))

    if (!is.logical(suppressUpdates)) {
        pkgsToUpdate <-
            filterPkgsToUpdate(suppressUpdates, pkgsToUpdate)
        suppressUpdates <- FALSE
    }

    oldPkgs <- getUpdateablePkgs(pkgsToUpdate)
    if (nrow(oldPkgs)) {
        pkgList <- paste(oldPkgs[,"Package"], collapse="', '")
        if (ask==TRUE) {
            .msg("Old packages: '%s'", pkgList)
            
            UpdateSelection <-
                .getUpdateSelection("Update all/some/none? [a/s/n]: ",
                         allowed = c("a", "A", "s", "S", "n", "N"))

            switch(UpdateSelection,
                   a = update.packages(repos=repos, oldPkgs=oldPkgs, ask=FALSE),
                   s = update.packages(repos=repos, oldPkgs=oldPkgs, ask=TRUE),
                   n = invisible(pkgs))
        } else {
            .msg("Updating packages '%s'", pkgList)
            update.packages(repos=repos, oldPkgs=oldPkgs, ask=ask)
        }
    }
    
    invisible(pkgs)
}

.getUpdateSelection <- function (msg, allowed)
{
    if (interactive()) {
        repeat {
            cat(msg)
            ans <- readLines(n = 1)
            if (ans %in% allowed)
              break
        }    
        tolower(ans)
    } else {
        "n"
    }
}

.InstallMedicalImgTV <- function (views = "MedicalImaging",
                           coreOnly = FALSE,
                           ctvrepos = NULL)
{
  if (!suppressWarnings(require("ctv", quietly=TRUE))) {
    install.packages("ctv")
    suppressPackageStartupMessages (require("ctv", quietly=TRUE))
  } else {
    suppressPackageStartupMessages (require("ctv", quietly=TRUE))    
  }   
  ctv::install.views(views = views, coreOnly = coreOnly, repos = ctvrepos)
}

BCoInstall <- function(pkgs=c("Biobase"),
##           No old versions to be updated right now
             suppressUpdates=FALSE,
             suppressAutoUpdate=FALSE,
             siteRepos=character(), ask=TRUE, 
             installmedicalimgTV = NULL, coreOnly = TRUE, ctvrepos = NULL, 
             ...)
{
    if (missing(pkgs))  { # BCoInstall() update w/out installing defaults
        pkgs <- pkgs[!pkgs %in% (.packages(all.available = TRUE))]
    }
    if (is.null(installmedicalimgTV))   {
        MITV_pkglist <- .MedicalImaging_group_pkglist(coreOnly = coreOnly)
        MITV_pkgs <- MITV_pkglist[[rownames(MITV_pkglist)]]
        MITV_pkgs <- MITV_pkgs[!MITV_pkgs %in% (.packages(all.available = TRUE))]
        for (i in seq(along = MITV_pkglist)) install.packages(MITV_pkgs[i], repos = names(MITV_pkglist)[i], ...)
        invisible()
    }  
    else if (installmedicalimgTV)
        .InstallMedicalImgTV(coreOnly = coreOnly, ctvrepos = ctvrepos)
    
    if (!suppressAutoUpdate && !BCoPkgIsCurrent()) {
        on.exit(updateBCoPkgs(pkgs, ask=ask,
                                          suppressUpdates=suppressUpdates,
                                          siteRepos=siteRepos, ...))
    }
    ##else 
    ##if ("BCoUpgrade" %in% pkgs) {
    ##    .BCoUpgrade()
    ##} 
    ##else {
    .BCoInstall(pkgs, ask=ask, siteRepos=siteRepos,
                     suppressUpdates=suppressUpdates, ...)
    ##}
}
