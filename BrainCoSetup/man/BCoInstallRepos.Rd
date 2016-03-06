\name{BCoInstallRepos}
\alias{BCoInstallRepos}
\title{

  Display current BrainConductor and CRAN repositories.

}
\description{

  Displays the URLs of the repositories used by \code{\link{BCoInstall}}
  to install Bioconductor and CRAN packages.

}
\usage{
BCoInstallRepos(siteRepos=character())
}
\arguments{
\item{siteRepos}{\code{character()} adding an additional
  repository in which to look for packages to install. This 
  repository will be appended to the default repositories
  returned by the function.}

}
\value{
  Named \code{character()} of repositories.
}
\seealso{
  \code{\link{BCoInstall}} Installs/updates BrainConductor/CRAN packages.

  \code{\link{install.packages}} downloads and installs the packages.

  \code{\link{chooseCRANmirror}} lets you make a selection from all
  public CRAN mirror URLs.
}
\examples{
## Choose CRAN mirrors
\dontrun{
chooseCRANmirror()
}

BCoInstallRepos()
}
\keyword{environment}
