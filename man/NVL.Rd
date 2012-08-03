\name{NVL}
\alias{NVL}
\title{
Return the second argument if the first argument is \code{NULL}, the
first argument otherwise.
}
\description{
This function is inspired by SQL function \code{NVL}, and is simply
shorthand for \code{if(is.null(EXPR)) NULLV else EXPR}.
}
\usage{
NVL(EXPR, NULLV)
}
\arguments{
  \item{EXPR}{
    An expression to be returned if it is not \code{NULL}.
}
  \item{NULLV}{
    An expression to be returned if \code{EXPR} is \code{NULL}.
}
}
\value{
  \code{NULL} if \code{EXPR} is \code{NULL}, \code{EXPR} otherwise.
}
\seealso{
  is.null, if
}
\examples{
a <- NULL

print(a) # NULL
print(NVL(a,0)) # 0

b <- 1

print(b) # 1
print(NVL(b,0)) # 1
}
\keyword{ utilities }
