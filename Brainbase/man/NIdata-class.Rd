\name{NIdata-class}
\Rdversion{1.1}
\docType{class}
\alias{NIdata-class}
\alias{BCoReduce,NIdata,numeric-method}
\alias{BCoReduce,NIdata,Template-method}
\alias{BCoWrite,NIdata-method}
\alias{convert.2Dto4D,NIdata-method}
\alias{convert.4Dto2D,NIdata-method}
\alias{get.matrix,NIdata-method}
\alias{get.phenotype,NIdata-method}
\alias{is.functional,NIdata-method}
\alias{show,NIdata-method}

\title{Class \code{"NIdata"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("NIdata", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{phenotype}:}{Object of class \code{"list"} ~~ }
    \item{\code{scanner_info}:}{Object of class \code{"scanner_info"} ~~ }
    \item{\code{extra}:}{Object of class \code{"list"} ~~ }
    \item{\code{ID}:}{Object of class \code{"character"} ~~ }
    \item{\code{data}:}{Object of class \code{"BCoData"} ~~ }
    \item{\code{notes}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{BCoBase}"}, directly.
}
\section{Methods}{
  \describe{
    \item{BCoReduce}{\code{signature(obj = "NIdata", template = "numeric")}: ... }
    \item{BCoReduce}{\code{signature(obj = "NIdata", template = "Template")}: ... }
    \item{BCoWrite}{\code{signature(obj = "NIdata")}: ... }
    \item{convert.2Dto4D}{\code{signature(obj = "NIdata")}: ... }
    \item{convert.4Dto2D}{\code{signature(obj = "NIdata")}: ... }
    \item{get.matrix}{\code{signature(obj = "NIdata")}: ... }
    \item{get.phenotype}{\code{signature(obj = "NIdata")}: ... }
    \item{is.functional}{\code{signature(obj = "NIdata")}: ... }
    \item{show}{\code{signature(object = "NIdata")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("NIdata")
}
\keyword{classes}
