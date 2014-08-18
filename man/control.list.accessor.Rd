\name{$.control.list}
\alias{$.control.list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Named element accessor for ergm control lists
}
\description{
Utility method that overrides the standard `$' list accessor to disable partial matching for ergm \code{control.list} objects
}
\usage{
\method{$}{control.list}(object, name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
list-coearceable object with elements to be searched
}
  \item{name}{
literal character name of list element to search for and return
}
}
\details{
Executes \code{\link[base]{getElement}} instead of \code{\link[base]{$}} so that element names must match exactly to be returned and partially matching names will not return the wrong object. 
}
\value{
Returns the named list element exactly matching \code{name}, or \code{NULL} if no matching elements found
}

\author{
pavel
}


\seealso{
see \code{\link{getElement}}
}
\examples{

## The function is currently defined as
function (object, name) 
{
    if (isS4(object)) 
        slot(object, name)
    else object[[name, exact = TRUE]]
  }
}

