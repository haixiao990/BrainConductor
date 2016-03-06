\name{Package Groups}
\alias{all_BCo_group}
\alias{MedicalImaging_group}
\title{
  Convenience functions to return package names from a specific subset of 
  BrainConductor.  
  Now only \code{all_BCo_group} and \code{MedicalImaging_group} are available.
}
\description{

  Returns character vectors of packages from a specific subset of BrainConductor,
  which can then be passed to \code{\link{BCoInstall}()}. Pakages from 
  \code{MedicalImaging_group} are not suggestted to be passed to 
  \code{\link{BCoInstall}()} to be installed, because we can directly install these
  packages using default parameters in \code{\link{BCoInstall}()}.

}
\usage{
all_BCo_group()
MedicalImaging_group(coreOnly = TRUE)
}
\arguments{
  \item{coreOnly}{\code{logical(1)} indicating names of all packages or only core packages to be returned}
}
\value{
  \code{character()} of package names.
}
\seealso{
  \code{\link{BCoInstall}} Installs/updates BrainConductor/CRAN packages.

  \code{\link{BCoInstallRepos}} returns the BrainConductor and CRAN
  repositories used by \code{BCoInstall}.

  \code{\link{install.packages}} downloads and installs packages.

  \code{\link{chooseCRANmirror}} lets you make a selection from all
  public CRAN mirror URLs.
}
\examples{
## Get the names of software packages in
## MedicalImaging task veiw from CRAN repositories
\dontrun{
pkgs <- MedicalImaging_group(coreOnly = FALSE)
pkgs
}
## Get the names of all software packages 
## available in BrainConductor
pkgs <- all_BCo_group()
pkgs
}
\keyword{environment}
