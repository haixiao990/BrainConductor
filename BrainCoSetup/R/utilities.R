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

.lowerRVersionString <-
    function(version=getRversion())
{
    if (0L == version$minor) {
        major <- version$major - 1L
        minor <- version$minor
    } else {
        major <- version$major
        minor <- version$minor - 1L
    }
    paste(major, minor, sep=".")
}

.stepAside <-
  function(BCoSetupEnv, SetupPackageUpdate) 
  {
    environment(SetupPackageUpdate) <- BCoSetupEnv
    BCoSetupEnv[["SetupPackageUpdate"]] <- SetupPackageUpdate
    attach(BCoSetupEnv)
    on.exit(eval(SetupPackageUpdate(), BCoSetupEnv))
  }