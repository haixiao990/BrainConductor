##Update the BrainConductor packages from the BrainConductor repository

BCoUpdatePkgs <-
    function(pkgs, dependencies = NA, repos = BCoInstallRepos(), ...)
{
    allowed <- c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
    if (identical(dependencies, NA))
      dependencies <- c("Depends", "Imports", "LinkingTo")
    avail <- available.packages(contriburl=contrib.url(repos))
    
    dependencies <- dependencies[dependencies %in% allowed]
    if (!length(dependencies)) {
      .stop("not allowed definition of dependencies, 
            ? BCoUpdatePkgs for help")
    }
    
    deps <- avail[pkgs, dependencies, drop=FALSE]
    deps <- unlist(apply(deps, 1, utils:::.clean_up_dependencies))
    deps <- unique(c(pkgs, deps))
    deps <- deps[deps %in% rownames(avail)]
    update.packages(oldPkgs=deps, repos=repos, ...)
} 
