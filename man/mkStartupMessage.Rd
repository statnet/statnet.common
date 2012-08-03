\name{mkStartupMessage}
\alias{mkStartupMessage}
\title{
  Construct a "standard" startup message to be printed when the package
  is loaded.
}
\description{
    This function uses information returned by \code{\link{packageDescription}}
  to list package copyrights and miscellaneous information. To determine
  institutional affiliation, it uses a look up table that maps domain
  names to institutions. (E.g., *.uw.edu or *.washington.edu maps to
  University of Washington.)
  }
\usage{
mkStartupMessage(pkgname)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pkgname}{
    Name of the package whose information is used.
}
}
\value{
  A string containing the information.
}
\seealso{
  packageDescription
}
\examples{
cat(mkStartupMessage("statnet.common"))
}
\keyword{ utilities }
