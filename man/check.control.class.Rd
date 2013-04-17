%  File man/check.control.class.Rd in package statnet.common, part of the Statnet suite
%  of packages for network analysis, http://statnet.org .
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) at
%  http://statnet.org/attribution
%
%  Copyright 2003-2013 Statnet Commons
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{check.control.class}
\alias{check.control.class}
\title{
  Check if the class of the control list is one of those that can be
  used by the calling function
}
\description{
  This function can be called to check that the control list passed is
  appropriate for the function to be controlled. It does so by looking up
  the class of the \code{control} argument (defaulting to the
  \code{control} variable in the calling function) and checking if it
  matches a list of acceptable classes (defaulting to the name of the
  calling function with "control." prepended).  
}
\usage{
check.control.class(OKnames = {
    sc <- sys.calls()
    as.character(sc[[length(sc) - 1]][[1]])
}, myname = {
    sc <- sys.calls()
    as.character(sc[[length(sc) - 1]][[1]])
}, control = get("control", pos = parent.frame()))
}

\arguments{
  \item{OKnames}{
    List of control function names which are acceptable.
}
  \item{myname}{
    Name of the calling function (used in the error message).
}
  \item{control}{
    The control list. Defaults to the \code{control} variable in the
    calling function.
}
}
\seealso{
  set.control.class, print.control.list
}

\keyword{ utilities }
