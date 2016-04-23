.dQuote <- function(x)
    sprintf('"%s"', as.character(x))

.sQuote <- function(x)
    sprintf("'%s'", as.character(x))

.arrangemsg <-
    function(fmt, ..., width=getOption("width"))
    ## Use this helper to format all error / warning / message text
{
    txt <- strwrap(sprintf(fmt, ...), width=width, exdent=2)
    paste(txt, collapse="\n")
}

.msg <-
    function(..., appendLF=TRUE)
{
    message(.arrangemsg(...), appendLF=appendLF)
}

.stop <-
    function(..., call.=FALSE)
{
    stop(.arrangemsg(...), call.=call.)
}

.warning <-
    function(..., call.=FALSE, immediate.=FALSE)
{
    warning(.arrangemsg(...), call.=call., immediate.=immediate.)
}

.testsuffix <- function(fn, suff){
  parts <- strsplit(fn,"\\.")
  nparts <- length(parts[[1]])
  return(parts[[1]][nparts] == suff)
}

