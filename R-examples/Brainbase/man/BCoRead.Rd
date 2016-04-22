\name{BCoRead}
\alias{BCoRead}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BCoRead(input, template = NULL, mask = NULL, subject.ID = "", controls = list(convert2D = F, method = "mean", verbose = F))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
%%     ~~Describe \code{input} here~~
}
  \item{template}{
%%     ~~Describe \code{template} here~~
}
  \item{mask}{
%%     ~~Describe \code{mask} here~~
}
  \item{subject.ID}{
%%     ~~Describe \code{subject.ID} here~~
}
  \item{controls}{
%%     ~~Describe \code{controls} here~~
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
function (input, template = NULL, mask = NULL, subject.ID = "", 
    controls = list(convert2D = F, method = "mean", verbose = F)) 
{
    assert_that(is.null(template) || class(template) == "Template")
    con = .convert.list2Readcontrol(controls)
    file.ending = strsplit(input, "\\.")[[1]]
    file.ending = file.ending[length(file.ending)]
    assert_that(file.ending \%in\% c(".dcm", "gz", "nii"))
    if (file.ending == ".dcm") {
        dat = read.DICOM.dcm(input)
    }
    else {
        dat = readNIfTI(input)
    }
    res = .NIdata(.BCoBase(data = .BCoData4D(mat = dat@.Data)), 
        scanner_info = .create.scaninfo(dat), ID = subject.ID)
    if (con@convert2D) {
        res = convert.4Dto2D(res, template = template, verbose = con@verbose)
    }
    if (!is.null(mask) & con@convert2D) {
        res = BCoReduce(res, mask, con@method, con@verbose)
    }
    res
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
