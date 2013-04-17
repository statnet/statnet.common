%  File man/compress.data.frame.Rd in package statnet.common, part of the Statnet suite
%  of packages for network analysis, http://statnet.org .
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) at
%  http://statnet.org/attribution
%
%  Copyright 2003-2013 Statnet Commons
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
