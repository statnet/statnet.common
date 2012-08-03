\name{statnet.cite}
\alias{statnet.cite.pkg}
\alias{statnet.cite.head}
\alias{statnet.cite.foot}

\title{
  \code{CITATION} file utilities for Statnet packages
}
\description{
  These functions automate citation generation for Statnet Project packages.
}
\usage{
statnet.cite.pkg(pkg)

statnet.cite.head(pkg)

statnet.cite.foot(pkg)
}
\arguments{
  \item{pkg}{
    Name of the package whose citation is being generated.
}
}
\value{
  For \code{statnet.cite.head} and \code{statnet.cite.foot}, an object
  of type \code{citationHeader} and \code{citationFooter}, respectively,
  understood by the \code{\link{citation}} function, with package name
  substituted into the template.

  For \code{statnet.cite.pkg}, an object of class \code{\link{bibentry}}
  containing the citation for the package constructed from
  \code{DESCRIPTION} and a template. 
}
\seealso{
  citation, citHeader, citFooter, bibentry
}

\examples{
statnet.cite.head("statnet.common")

statnet.cite.pkg("statnet.common")

statnet.cite.foot("statnet.common")
}
\keyword{ utilities }
