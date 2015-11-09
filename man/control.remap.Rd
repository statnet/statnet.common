\name{control.remap}
\alias{control.remap}

\title{
  Overwrite control parameters of one configuration with another.
}
\description{
  Given a \code{control.list}, and two prefixes, \code{from} and
  \code{to}, overwrite the elements starting with \code{to} with
  the corresponding elements starting with \code{from}.
}
\usage{
control.remap(control, from, to)
}
\arguments{
  \item{control}{
    An object of class \code{control.list}.
}
  \item{from}{
    Prefix of the source of control parameters.
}
  \item{to}{
    Prefix of the destination of control parameters.
}
}
\value{
  An \code{control.list} object.
}
\author{
  Pavel N. Krivitsky
}
\seealso{
  \code{\link{print.control.list}}
}
\examples{
(l <- set.control.class("test", list(a.x=1, a.y=2)))
control.remap(l, "a", "b")
}
\keyword{ utilities }
