##get ContribUrl of the BrainConductor repository, 
##the package should be available via the Url, 
##or the function will check it for older R version.
.getContribUrl <-
    function(BCoVersion)
{
    .contribUrl <-
        function(repos, replace_oldRepos = FALSE)
    {
        contribUrl <- contrib.url(repos)
        if(replace_oldRepos) {
            version <- getRversion()
            currentVersion <- sprintf("%d.%d", version$major, version$minor)
            lowerVersion <- .lowerRVersionString(version)
            oldcontribUrl <- sub(currentVersion, lowerVersion, contribUrl)
            if (oldcontribUrl == contribUrl)
                .stop("'%s' while trying %s", conditionMessage(err),
                      repos, call.=FALSE)
            contribUrl <- oldcontribUrl
        }
        pkgs <- available.packages(contribUrl)
        if (nrow(pkgs) == 0L)
            .stop("no packages in repository (no internet connection?)",
                  call.=FALSE)
        else if (!"BrainCoSetup" %in% rownames(pkgs))
            .stop("'BrainCoSetup' package not in repository",
                  call.=FALSE)
        contribUrl
    }
    ##remember to modify this line
    repos <- .BCoInstallRepos(braincoversion=BCoVersion)["BrainCo_soft"]
    suppressWarnings(tryCatch({
        .contribUrl(repos)
    }, error=function(err) {
##     version <- getRversion()
##     currentVersion <- sprintf("%d.%d", version$major, version$minor)
##     lowerVersion <- .lowerRVersionString(version)
##     oldRepos <- sub(currentVersion, lowerVersion, repos)
##     if (oldRepos == repos)
##          .stop("'%s' while trying %s", conditionMessage(err),
##                repos, call.=FALSE)
       .msg("'%s' while trying %s, trying older R version package",
            conditionMessage(err), repos)
       .contribUrl(repos, replace_oldRepos = TRUE)
    }))
}

##Check whether the repository of the current BrainConductor version 
##has a new version of BrainCoSetup package 
BCoPkgIsCurrent <-
    function()
{
    installedSentinel <- availableSentinel <- package_version("0.0.0")
    installedVersion <-
        tryCatch(packageVersion("BrainCoSetup"),
                 error = function(err) installedSentinel)
    contribUrl <- .getContribUrl(BrainCoVersion())
    ap <- available.packages(contribUrl)
    availableVersion <-
        if ("BrainCoSetup" %in% rownames(ap))
            package_version(ap["BrainCoSetup", "Version"])
        else
            availableSentinel
    if ((installedVersion == availableVersion) &&
        (installedVersion == installedSentinel))
        .stop("'BrainCoSetup' package not installed, and not available")
    availableVersion <= installedVersion
}

updateBCoPkg <-
    function(pkgs, ask, suppressUpdates, ...)
{
    .msg("before update, version is %s", packageVersion("BrainCoSetup"))
    SetupPackageUpdate <-
        function()
    {
        if ("package:BrainCoSetup" %in% search())
            detach("package:BrainCoSetup", unload=TRUE, force=TRUE)
        ## contribUrl will be in SetupPackageUpdate's environment, i.e. BCoSetupEnv
        suppressWarnings(tryCatch({
            update.packages(contriburl=contribUrl, ask=FALSE,
                            checkBuilt=TRUE, oldPkgs="BrainCoSetup")
        }, error=function(err) {
            assign("failed", TRUE, "BCoSetupEnv")
            NULL
        }))
        library(BrainCoSetup)
        BrainCoSetup:::.updateBCoPkgFinish()
    }
    BCoSetupEnv <- new.env()
    BCoSetupEnv[["pkgs"]] <- pkgs[pkgs != "BrainCoSetup"]
    BCoSetupEnv[["ask"]] <- ask
    BCoSetupEnv[["suppressUpdates"]] <- suppressUpdates
    BCoSetupEnv[["contribUrl"]] <- .getContribUrl(BrainCoVersion())
    BCoSetupEnv[["dotArgs"]] <- list(...)
    
    .stepAside(BCoSetupEnv, SetupPackageUpdate)
}

.updateBCoPkgFinish <-
    function()
{
    args <- c(list(pkgs=get("pkgs", "BCoSetupEnv"),
                   ask=get("ask", "BCoSetupEnv"),
                   suppressUpdates=get("suppressUpdates", "BCoSetupEnv")),
              get("dotArgs", "BCoSetupEnv"))
    failed <- exists("failed", "BCoSetupEnv")
    detach("BCoSetupEnv")
    .msg("after update, version is %s", packageVersion("BrainCoSetup"))
    vers <- packageVersion("BrainCoSetup")
    if (!failed)
        .msg("'BrainCoSetup' updated to version %s", vers)
    else
        .warning("'BrainCoSetup' update failed, using version %s",
                 vers, call.=FALSE)
    if ("BCoUpgrade" %in% args$pkgs) {
        .BCoUpgrade()
    } else {
        do.call(.BCoInstall, args)
    }
}
