\name{sort.data.frame}
\alias{sort.data.frame}
\alias{order}
\alias{order.default}
\alias{order.data.frame}
\alias{order.matrix}
\title{
  Implement the \code{\link{sort}} and \code{\link{order}} methods for
  \code{\link{data.frame}} and \code{\link{matrix}},
  sorting it in lexicographic order.
}
\description{
  These function return a data frame sorted in lexcographic order or a
  permutation that will rearrange it into lexicographic order: first
  by the first column, ties broken by the second, remaining ties by the
  third, etc..
}
\usage{
\method{sort}{data.frame}(x, decreasing = FALSE, ...)

\method{order}{data.frame}(\dots, na.last = TRUE, decreasing = FALSE)

\method{order}{matrix}(\dots, na.last = TRUE, decreasing = FALSE)
}
\arguments{
  \item{x}{
    A \code{\link{data.frame}} to sort.
  }
  \item{\dots}{
    Ignored for \code{sort}. For \code{order}, first argument is the
    data frame to be ordered. (This is needed for compatibility with \code{\link[base]{order}}.)
  }
  \item{decreasing}{
    Whether to sort in decreasing order.
  }
  \item{na.last}{
    See \code{\link[base]{order}} documentation.
  }
}
\value{
  For \code{sort}, a data frame, sorted lexicographically. For
  \code{order}, a permutation \code{I} (of a vector \code{1:nrow(x)})
  such that \code{x[I,,drop=FALSE]} equals \code{x} ordered
  lexicographically.
}
\seealso{
  \code{\link{data.frame}}, \code{\link{sort}}, \code{\link{order}}, \code{\link{matrix}}
}
\examples{
data(iris)

head(iris)

head(order.data.frame(iris))

head(sort(iris))

stopifnot(identical(sort(iris),iris[order.data.frame(iris),]))
}

\keyword{ manip }
