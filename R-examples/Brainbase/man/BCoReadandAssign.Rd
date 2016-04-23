\name{BCoReadandAssign}
\alias{BCoReadandAssign}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BCoReadandAssign(input, variable.header, template = NULL, mask = NULL, controls = list(convert2D = F, method = "mean", verbose = F), subject.ID = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
%%     ~~Describe \code{input} here~~
}
  \item{variable.header}{
%%     ~~Describe \code{variable.header} here~~
}
  \item{template}{
%%     ~~Describe \code{template} here~~
}
  \item{mask}{
%%     ~~Describe \code{mask} here~~
}
  \item{controls}{
%%     ~~Describe \code{controls} here~~
}
  \item{subject.ID}{
%%     ~~Describe \code{subject.ID} here~~
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
function (input, variable.header, template = NULL, mask = NULL, 
    controls = list(convert2D = F, method = "mean", verbose = F), 
    subject.ID = "") 
{
    res = BCoRead(input, template = template, mask = mask, subject.ID = subject.ID, 
        controls = controls)
    resname = paste0(variable.header, "_", subject.ID)
    resname.mod = resname
    current.vars = ls(.GlobalEnv)
    if (resname.mod \%in\% current.vars) {
        iter = 1
        while (TRUE) {
            resname.mod = paste0(resname, "_", iter)
            if (!resname.mod \%in\% current.vars) 
                (break)()
            iter = iter + 1
        }
    }
    assign(resname.mod, res, envir = .GlobalEnv)
    resname.mod
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
