%  File man/paste.and.Rd in package statnet.common, part of the Statnet suite
%  of packages for network analysis, http://statnet.org .
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) at
%  http://statnet.org/attribution
%
%  Copyright 2003-2013 Statnet Commons
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{paste.and}
\alias{paste.and}
\title{
  Concatenates the elements of a vector (optionaly enclosing them in
  quotation marks or parentheses) adding appropriate punctuation and
  unions.
}
\description{
  A vector \code{x} becomes "\code{x[1]}", "\code{x[1]} and
  \code{x[2]}", or "\code{x[1]},
  \code{x[2]}, and \code{x[3]}", depending on the langth of \code{x}.
}
\usage{
paste.and(x, oq = "", cq = "")
}
\arguments{
  \item{x}{
    A vector.
}
  \item{oq}{
    Opening quotation symbol. (Defaults to none.)
}
  \item{cq}{
    Closing quotation symbol. (Defaults to none.)
}
}
\value{
  A string with the output.
}
\seealso{
  paste, cat
}
\examples{
print(paste.and(c()))

print(paste.and(1))

print(paste.and(1:2))

print(paste.and(1:3))

print(paste.and(1:4))
}
\keyword{ utilities }
