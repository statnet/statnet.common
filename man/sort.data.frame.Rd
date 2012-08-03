\name{sort.data.frame}
\alias{sort.data.frame}
\title{
  Implements a \code{\link{sort}} method for \code{\link{data.frame}},
  sorting it in lexicographic order.
}
\description{
  This function returns a data frame sorted in lexcographic order: first
  by the first column, ties broken by the second, remaining ties by the
  third, etc..
}
\usage{
\method{sort}{data.frame}(x, decreasing = FALSE, ...)
}
\arguments{
  \item{x}{
    A \code{\link{data.frame}} to sort.
}
  \item{decreasing}{
    Whether to sort in decreasing order.
}
  \item{\dots}{
    Ignored. Needed for compatibility with 
}
}
\value{
  A data frame, sorted lexicographically.
}
\seealso{
  data.frame, sort
}
\examples{
data(iris)

head(iris)

head(sort(iris))
}

\keyword{ manip }
