\name{BCoWrite.all}
\alias{BCoWrite.all}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BCoWrite.all(file.dir = NULL, subj.header = NULL, controls = list(type = "RData", onefile = T, gzipped = T, verbose = F, warn = -1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file.dir}{
%%     ~~Describe \code{file.dir} here~~
}
  \item{subj.header}{
%%     ~~Describe \code{subj.header} here~~
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
function (file.dir = NULL, subj.header = NULL, controls = list(type = "RData", 
    onefile = T, gzipped = T, verbose = F, warn = -1)) 
{
    assert_that(substr(file.dir, nchar(file.dir), nchar(file.dir)) == 
        "/")
    subj.vec = BCoSubjectFinder.default()
    if (!is.null(subj.header)) {
        assert_that(is.character(subj.header) & length(subj.header) == 
            1)
        subj.vec = grep(subj.header, subj.vec, value = T)
    }
    for (i in subj.vec) {
        BCoWrite(eval(as.name(i)), filename.prefix = paste0(file.dir, 
            deparse(substitute(i))), controls = controls)
        if (is.null(controls$verbose) || controls$verbose) {
            paste0("Writing for ", i, " is complete!")
        }
    }
    invisible()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
