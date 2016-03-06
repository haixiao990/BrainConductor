\name{BCoInstall}
\alias{BCoInstall}
\title{

  Install or update Brainconductor and CRAN packages

}
\description{

  \code{BCoInstall} installs or updates Brainconductor and CRAN packages in
  a Braincoductor release. Upgrading is now unavailable for the test version.

}
\usage{
BCoInstall(pkgs=c("Biobase"),  
    suppressUpdates=FALSE,
    suppressAutoUpdate=FALSE,
    siteRepos=character(),
    ask=TRUE,
    installmedicalimgTV = NULL, coreOnly = TRUE, ctvrepos = NULL,
    ...)
}
\arguments{

  \item{pkgs}{\code{character()} of package names to install or 
    update in the repository of BrainConductor or CRAN.}
    

  \item{suppressUpdates}{\code{logical(1)} indicating whether to
    suppress updating of all installed packages in the library 
    automatically, or \code{character()} describing packages NOT
    to automatically update.}

  \item{suppressAutoUpdate}{\code{logical(1)} indicating whether the
    \code{BrainCoSetup} package updates itself.}

  \item{siteRepos}{\code{character()} appending an additional
    repository in which to search packages to install. The default 
    repositories are set by the function of \code{\link{BCoInstallRepos}}.}

  \item{ask}{\code{logical(1)} indicating whether to ask user for updating
    the installed packages which have new versions, or the character 
    string 'graphics', which brings up a widget for the selection of packages
    to update. More details can be seen with \code{\link{update.packages}}.}
    
  \item{installmedicalimgTV}{\code{logical(1)} indicating whether to install MedicalImaging task from CRAN Task Views. If the argument is NULL as default, the function will automatically install packages in the "MedicalImaging" task but not installed in the local library}
    
  \item{coreOnly}{\code{logical(1)} passed to \code{\link{install.views}} 
    in \code{ctv} package, indicating all packages or only core packages be installed?}
    
  \item{ctvrepos}{\code{character()}, passed to \code{\link{install.views}} 
    in \code{ctv} package. The base URL of the task view CRAN repository.}
  
  \item{...}{Additional arguments. \code{lib.loc} is passed to
    \code{\link{old.packages}} (used to determine the library location
    of installed packages to be updated). \code{lib} is passed to
    \code{\link{install.packages}} (used to determine the library
    location where \code{pkgs} are to be installed).}

}

\value{
  \code{BrainCoSetup()} invisibly returns the \code{pkgs} argument.
}
\seealso{

  \code{\link{BCoInstallRepos}} returns the BrainConductor and CRAN
  repositories used by \code{BCoInstall}.

  \code{\link{install.packages}} downloads and installs the packages.

  \code{\link{update.packages}} updates all installed packages.
  
  \code{\link{install.views}} diretly installs a Task View from CRAN.
  An additional package \code{ctv} is needed.

  \code{\link{chooseCRANmirror}} lets you make a selection from all
  public CRAN mirror URLs.
  
  \code{\link{all_BCo_group}} returns a list of all BrainConductor
  software packages that are available.

}

\examples{
\dontrun{

## If you don't have the BrainCoSetup package installed, you can 
## quickly install and load it as follows:
source("http://BrainConductor.org/biocLite.R")


## The most recent version of the BrainCoSetup package is now loaded.
## No need to load it with library() or require().

## installs default packages (if not already installed) and packages (core)
## in the medicalimaging task view, and updates previously installed packages 
BCoInstall()


## Install default packages, but do not install the task view of MedicalImaging
BCoInstall(installmedicalimgTV = FALSE)

## Now install a CRAN package:
BCoInstall("MASS")

## install a BrainConductor package, but suppress updating all installed
## packages as well:
BCoInstall("BrainCoBase", suppressUpdates=TRUE)

## Install default packages, but do not update any package whose name
## starts with "Analyze." or "mri."
BCoInstall(suppressUpdates=c("^BrainCo\\.", "^mri\\."))

## install a package with source type:
BCoInstall("IRanges", type="source")

## install all Bioconductor software packages
BCoInstall(all_BCo_group())

}
## Show the BrainConductor and CRAN repositories that will be used to
## install/update packages.
BCoInstallRepos()
}
\keyword{environment}
