\name{NVL}
\alias{NVL}
\title{
Return the first argument passed (out of any number) that is not \code{NULL}.
}
\description{
This function is inspired by SQL function \code{NVL}, and simply returns
the first argument that is not \code{NULL}, or \code{NULL} if all arguments are NULL.
}
\usage{
NVL(\dots)
}
\arguments{
  \item{\dots}{
    Expressions to be tested.
  }
}
\details{
  Note that an earlier version of this function took only two arguments:
  \code{EXPR}, that was tested and returned if not \code{NULL} and
  \code{NULLV}, which was returned if \code{EXPR} was
  \code{NULL}. The new version produces identical output for the same
  (two-argument) input, but tests any number of expressions
  sequentially.
}
\value{
  The first argument that is not \code{NULL}, or \code{NULL} if all
  are.
}
\seealso{
  \code{\link[base]{is.null}}, \code{\link[base]{if}}
}
\examples{
a <- NULL

print(a) # NULL
print(NVL(a,0)) # 0

b <- 1

print(b) # 1
print(NVL(b,0)) # 1

# Also,
print(NVL(NULL,1,0)) # 1
print(NVL(NULL,0,1)) # 0
print(NVL(NULL,NULL,0)) # 0
print(NVL(NULL,NULL,NULL)) # NULL
}
\keyword{ utilities }
