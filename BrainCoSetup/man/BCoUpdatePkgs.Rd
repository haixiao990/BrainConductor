\name{BCoUpdatePkgs}
\alias{BCoUpdatePkgs}

\title{

  Update packages previously installed from BrainConductor or CRAN and their
  dependencies.

}
\description{

  Update previously installed  BrainConductor and CRAN packages and their
  dependencies. Use \code{\link{BCoInstall}} to install new packages or to
  update all outdated packages. Upgrading to a new BrainConductor
  release requires additional steps; see
  \url{http://BrainConductor.org/downloads}.

}
\usage{
BCoUpdatePkgs(pkgs, dependencies = NA, repos=BCoInstallRepos(), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  
  \item{pkgs}{\code{character()} of package names to install or update.}

  \item{dependencies}{\code{character()} describing outdated
    dependencies that are also updated. Defaults to \code{c("Depends",
    "Imports", "LinkingTo")} but can be a subset of \code{c("Depends",
    "Imports", "LinkingTo", "Suggests", "Enhances")}.}

  \item{repos}{\code{character()} of named repositories in which to look
    for package updates, in the style of \code{BCoInstallRepos()}.}

  \item{\dots}{Additional arguments, passed to
    \code{\link{update.packages}}. For example, \code{ask=FALSE} to
    avoid asking before updating individual packages.}

}

\value{\sQuote{NULL}, invisibly.}

\seealso{\code{\link{BCoInstall}}}

\examples{
\dontrun{
BCoUpdatePkgs("fmri", ask=FALSE)
}
}

\keyword{environment}
