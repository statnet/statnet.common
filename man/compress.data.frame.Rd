\name{compress.data.frame}
\alias{compress.data.frame}
\title{
  "Compress" a data frame
}
\description{
  This function "compresses" a data frame, returning unique rows and a tally of the
  number of times each row is repeated
}
\usage{
compress.data.frame(x)
}
\arguments{
  \item{x}{
    A data frame.
}
}
\value{
  A \code{\link{list}} with two elements:
  \item{rows }{Unique rows of \code{x}}
  \item{frequencies }{A vector of the same length as the number or rows,
    giving the number of times the corresponding row is repeated }
}
\seealso{
  data.frame
}
\examples{
data(faithful)

head(faithful)

lapply(compress.data.frame(faithful),head)
}
\keyword{ manip }
