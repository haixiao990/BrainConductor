\name{convert.nidata2nifti}
\alias{convert.nidata2nifti}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
convert.nidata2nifti(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
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
function (obj) 
{
    assert_that(class(obj@data) == "BCoData4D" | class(obj@data) == 
        "BCoData2D")
    dat = new("nifti")
    if (class(obj@data) == "BCoData4D") {
        dat@.Data = obj@data@mat
    }
    else if (class(obj@data) == "BCoData2D") {
        dat@.Data = .convert.4Dto2Dmat(obj@data@mat, obj@data@mask, 
            obj@data@base.dim)
    }
    scaninfo = obj@scanner_info
    dat@aux_file = scaninfo@aux_file
    dat@bitpix = scaninfo@bitpix
    dat@cal_max = scaninfo@cal_max
    dat@cal_min = scaninfo@cal_min
    dat@data_type = scaninfo@data_type
    dat@datatype = scaninfo@datatype
    dat@db_name = scaninfo@db_name
    dat@descrip = scaninfo@descrip
    dat@dim_ = scaninfo@dim_
    dat@dim_info = scaninfo@dim_info
    dat@extender = scaninfo@extender
    dat@extents = scaninfo@extents
    dat@glmax = scaninfo@glmax
    dat@glmin = scaninfo@glmin
    dat@intent_code = scaninfo@intent_code
    dat@intent_name = scaninfo@intent_name
    dat@intent_p1 = scaninfo@intent_p1
    dat@intent_p2 = scaninfo@intent_p2
    dat@intent_p3 = scaninfo@intent_p3
    dat@magic = scaninfo@magic
    dat@pixdim = scaninfo@pixdim
    dat@qform_code = scaninfo@qform_code
    dat@qoffset_x = scaninfo@qoffset_x
    dat@qoffset_y = scaninfo@qoffset_y
    dat@qoffset_z = scaninfo@qoffset_z
    dat@quatern_b = scaninfo@quatern_b
    dat@quatern_c = scaninfo@quatern_c
    dat@quatern_d = scaninfo@quatern_d
    dat@regular = scaninfo@regular
    dat@reoriented = scaninfo@reoriented
    dat@scl_inter = scaninfo@scl_inter
    dat@scl_slope = scaninfo@scl_slope
    dat@session_error = scaninfo@session_error
    dat@sform_code = scaninfo@sform_code
    dat@sizeof_hdr = scaninfo@sizeof_hdr
    dat@slice_code = scaninfo@slice_code
    dat@slice_duration = scaninfo@slice_duration
    dat@slice_end = scaninfo@slice_end
    dat@slice_start = scaninfo@slice_start
    dat@srow_x = scaninfo@srow_x
    dat@srow_y = scaninfo@srow_y
    dat@srow_z = scaninfo@srow_z
    dat@toffset = scaninfo@toffset
    dat@vox_offset = scaninfo@vox_offset
    dat@xyzt_units = scaninfo@xyzt_units
    dat
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
