\name{data.export}
\alias{data.export}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
data.export(object, filename, filetype = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{filename}{
%%     ~~Describe \code{filename} here~~
}
  \item{filetype}{
%%     ~~Describe \code{filetype} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (object, filename, filetype = NULL, ...) 
{
    if (is.null(filetype)) {
        parts <- strsplit(filename, "\\.")
        nparts <- length(parts[[1]])
        filesuff <- parts[[1]][nparts]
        filetype <- switch(filesuff, nii = "NIFTI", hdr = "ANALYZE", 
            HEAD = "AFNI", dcm = "DICOMfile")
        if (nparts == 1 || is.null(filetype)) 
            .stop("The filename is invalid, please either input the entire filename\n             or set up a filetype if the filename is just a prefix")
    }
    allowed_type <- c("NIFTI", "ANALYZE", "AFNI", "DICOMfile", 
        "DICOMdir")
    if (!filetype \%in\% allowed_type) 
        .stop("Invalid filetype")
    if (filetype == "NIFTI") 
        writeNIFTI(filename, reorient = reorient)
    if (filetype == "ANALYZE") {
        object <- readANALYZE(filename)
        object <- as(object, "nifti")
    }
    if (filetype == "DICOMfile") {
        object <- readDICOMFile(filename)
        object <- dicom2nifti(object)
    }
    if (filetype == "DICOMdir") {
        object <- readDICOM(filename)
        object <- dicom2nifti(object)
    }
    if (filetype == "AFNI") {
        object <- read.AFNI(filename, setmask = F)
        object <- fmri2oro(object)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
