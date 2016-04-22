\name{BCoLink.phenotype}
\alias{BCoLink.phenotype}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BCoLink.phenotype(tab, subject.ID.col, kept.column.idx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab}{
%%     ~~Describe \code{tab} here~~
}
  \item{subject.ID.col}{
%%     ~~Describe \code{subject.ID.col} here~~
}
  \item{kept.column.idx}{
%%     ~~Describe \code{kept.column.idx} here~~
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
function (tab, subject.ID.col, kept.column.idx) 
{
    assert_that(class(tab) == "data.frame")
    assert_that(class(tab[, subject.ID.col]) == "character")
    assert_that(all(!duplicated(tab[, subject.ID.col])))
    obj.vec = BCoSubjectFinder.default()
    subj.vec = sapply(obj.vec, function(x) {
        eval(as.name(x))@ID
    })
    colnam = names(tab)[kept.column.idx]
    link.phenotype <- function(i) {
        idx = which(subj.vec == tab[i, subject.ID.col])
        if (length(idx) > 0) {
            lis = as.list(tab[i, kept.column.idx])
            names(lis) = colnam
            for (j in 1:length(idx)) {
                NIobj = eval(as.name(variables[idx.NIdata[idx[j]]]))
                assert_that(class(NIobj) == "NIdata")
                NIobj@phenotype = lis
                assign(variables[idx.NIdata[idx[j]]], NIobj, 
                  envir = .GlobalEnv)
            }
        }
    }
    sapply(1:nrow(tab), link.phenotype)
    invisible()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
