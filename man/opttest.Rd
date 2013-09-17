%  File man/opttest.Rd in package statnet.common, part of the Statnet suite
%  of packages for network analysis, http://statnet.org .
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) at
%  http://statnet.org/attribution
%
%  Copyright 2003-2013 Statnet Commons
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{opttest}
\alias{opttest}
\title{
  Optionally test code depending on environment variable.
}
\description{
A convenience wrapper to run code based on whether an environment
variable is defined.
}
\usage{
opttest(expr, testname = NULL, testvar = "ENABLE_statnet_TESTS",
        yesvals=c("y","yes","t","true","1"), lowercase=TRUE)
}
\arguments{
  \item{expr}{
    An expression to be evaluated only if \code{testvar} is set to a
    non-empty value.
  }
  \item{testname}{
    Optional name of the test. If given, and the test is skipped, will
    print a message to that end, including the name of the test, and
    instructions on how to enable it.
  }
  \item{testvar}{
    Environment variable name. If set to one of the \code{yesvals},
    \code{expr} is run. Otherwise, an optional message is printed.
  }
  \item{yesvals}{
    A character vector of strings considered affirmative values for
    \code{testvar}.
  }
  \item{lowercase}{
    Whether to convert the value of \code{testvar} to lower case before
    comparing it to \code{yesvals}.
  }
}

\keyword{ utilities }
\keyword{ environment }
\keyword{ debugging }
