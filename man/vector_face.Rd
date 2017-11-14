\name{vector_face}
\alias{vector_face}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Vector Face
}
\description{
Takes a vector of length = 14, and creates a face
}
\usage{
vector_face(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
}
\details{
Takes a vector of length = 14. Values for index[1] should be between 0.5-1.5, and values for 2-14 should be between 1-100. Values of 0 will return white for colors, and facial features will disappear (this may be what you want)
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


# draws a standard face
test_face <- c(1,50,50,50,50,50,50,50,50,50,50,50,50,50)
vector_face(test_face)

# draws a random face
vector_face(random_vector())

## The function is currently defined as
function (x)
{

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
