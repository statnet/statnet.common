%  File man/set.control.class.Rd in package statnet.common, part of the Statnet suite
%  of packages for network analysis, http://statnet.org .
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) at
%  http://statnet.org/attribution
%
%  Copyright 2003-2013 Statnet Commons
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
