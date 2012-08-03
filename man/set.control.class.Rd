\name{set.control.class}
\alias{set.control.class}
\title{
  Set the class of the control list
}
\description{
  This function sets the class of the control list, with the default
  being the name of the calling function.
}
\usage{
set.control.class(myname = {
    sc <- sys.calls()
    as.character(sc[[length(sc) - 1]][[1]])
}, control = get("control", pos = parent.frame()))
}
\arguments{
  \item{myname}{
    Name of the class to set. Defaults to the name of the calling function.
}
  \item{control}{
    Control list. Defaults to the \code{control} variable in the calling function.
}
}
\value{
  The control list with class set.
}
\seealso{
  check.control.class, print.control.list
}
\keyword{ utilities }
