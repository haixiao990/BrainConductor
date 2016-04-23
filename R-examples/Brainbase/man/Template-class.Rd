\name{Template-class}
\Rdversion{1.1}
\docType{class}
\alias{Template-class}
\alias{BCoReduce,NIdata,Template-method}
\alias{get.matrix,Template-method}

\title{Class \code{"Template"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Template", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{scanner_info}:}{Object of class \code{"scanner_info"} ~~ }
    \item{\code{exra}:}{Object of class \code{"list"} ~~ }
    \item{\code{data}:}{Object of class \code{"BCoData"} ~~ }
    \item{\code{notes}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{BCoBase}"}, directly.
}
\section{Methods}{
  \describe{
    \item{BCoReduce}{\code{signature(obj = "NIdata", template = "Template")}: ... }
    \item{get.matrix}{\code{signature(obj = "Template")}: ... }
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
showClass("Template")
}
\keyword{classes}
