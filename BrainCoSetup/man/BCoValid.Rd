\name{BCoValid}

\alias{BCoValid}

\title{Validate installed package versions against BCoInstall versions.}

\description{

  Check whether local installed packages are consistent (neither outdated nor
  too new) with the version of R and BrainConductor in the local system, using
  \code{BCoInstall} to reinstall invalid packages.

}

\usage{
BCoValid(pkgs = installed.packages(lib.loc, priority = priority),
    lib.loc = NULL, priority = "NA", type = getOption("pkgType"),
    filters = NULL, silent = FALSE, ..., fix=FALSE)
}

\arguments{
  \item{pkgs}{A character list of package names for checking, or a
    matrix as returned by \code{\link{installed.packages}}.}

  \item{lib.loc}{The library location(s) of packages to be validated;
    see \code{\link{installed.packages}}.}

  \item{priority}{check validity of all, \dQuote{base}, or
    \dQuote{recommended} packages; see
    \code{\link{installed.packages}}.}

  \item{type}{The type of available package (e.g., binary, source) to
    check validity against; see \code{\link{available.packages}}.}

  \item{filters}{Filter available packages to check validity against; see
    \code{\link{available.packages}}.}

  \item{silent}{Report how packages are invalid (\code{silent=FALSE},
    default) and abort execution, or return a logical(1)
    (\code{silent=TRUE}) indicating the overall validity of installed
    packages.}

  \item{\dots}{Additional arguments, passed to \code{\link{BCoInstall}}
    when \code{fix=TRUE}.}

  \item{fix}{When \code{TRUE}, invoke \code{BCoInstall} to reinstall
    (update or downgrade, as appropriate) invalid packages.}
    
}

\details{
  
  This function checks whether the version of installed packages is 
  associated with the version of R and Brainconductor appropriate for 
  the BrainCoSetup package currently in use.

  Packages are reported as \sQuote{out-of-date} if a more recent version
  is available at the repositories appointed by
  \code{BCoInstallRepos()}. Usually, \code{BCoInstall()} is recommended to
  upgrade packages to their most recent version.

  Packages are reported as \sQuote{too new} if the installed version is
  newer than the most recent available in the
  \code{BCoInstallRepos()} repositories. It is possible to down-grade
  by re-installing a too new package \dQuote{PkgA} with
  \code{BCoInstall("PkgA")}. The user should figure out how their 
  installation became too new, and to avoid this in the future.
}

\value{
  \code{logical(1)} indicating overall validity of installed packages.
}

\seealso{\code{\link{BCoInstall}} to reinstall or update installed packages.}

\examples{try(BCoValid())}

\keyword{ environment }
