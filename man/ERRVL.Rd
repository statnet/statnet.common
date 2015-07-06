\name{ERRVL}
\alias{ERRVL}
\title{
Return the first argument passed (out of any number) that is not
a \code{try-error} (result of \code{\link[base]{try}} encountering an error.
}
\description{
This function is inspired by \code{\link{NVL}}, and simply returns
the first argument that is not a \code{try-error}, raising an error if
all arguments are \code{try-error}s.
}
\usage{
ERRVL(...)
}
\arguments{
  \item{\dots}{
    Expressions to be tested; usually outputs of \code{\link[base]{try}}.
  }
}
\value{
  The first argument that is not a \code{try-error}. Stops with an error
  if all are.
}
\seealso{
  \code{\link[base]{try}}, \code{\link[base]{inherits}}
}
\examples{
print(ERRVL(1,2,3)) # 1
print(ERRVL(try(solve(0)),2,3)) # 2
}
\keyword{ utilities }
