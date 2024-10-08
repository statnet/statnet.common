% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mcmc-utils.R
\name{mcmc-utilities}
\alias{mcmc-utilities}
\alias{colMeans.mcmc.list}
\alias{var.mcmc.list}
\alias{sweep.mcmc.list}
\alias{lapply.mcmc.list}
\title{Utility operations for \code{\link[coda:mcmc.list]{mcmc.list}} objects}
\usage{
colMeans.mcmc.list(x, ...)

var.mcmc.list(x, ...)

sweep.mcmc.list(x, STATS, FUN = "-", check.margin = TRUE, ...)

lapply.mcmc.list(X, FUN, ...)
}
\arguments{
\item{x}{a \code{\link[coda:mcmc.list]{mcmc.list}} object.}

\item{\dots}{additional arguments to the functions evaluated on each chain.}

\item{STATS, FUN, check.margin}{See help for \code{\link[=sweep]{sweep()}}.}

\item{X}{An \code{\link[coda:mcmc.list]{mcmc.list}} object.}
}
\value{
\code{colMeans.mcmc} returns a vector with length equal to
the number of mcmc chains in \code{x} with the mean value for
each chain.

\code{sweep.mcmc.list} returns an appropriately modified
version of \code{x}

\code{lapply.mcmc.list} returns an \code{\link[coda:mcmc.list]{mcmc.list}} each of
whose chains had been passed through \code{FUN}.
}
\description{
\code{colMeans.mcmc.list} is a "method" for (non-generic) \code{\link[=colMeans]{colMeans()}} applicable to \code{\link[coda:mcmc.list]{mcmc.list}} objects.

\code{var.mcmc.list} is a "method" for (non-generic)
\code{\link[=var]{var()}} applicable to \code{\link[coda:mcmc.list]{mcmc.list}} objects. Since MCMC chains
are assumed to all be sampling from the same underlying
distribution, their pooled mean is used.

\code{sweep.mcmc.list} is a "method" for (non-generic)
\code{\link[=sweep]{sweep()}} applicable to \code{\link[coda:mcmc.list]{mcmc.list}} objects.

\code{lapply.mcmc.list} is a "method" for (non-generic)
\code{\link[=lapply]{lapply()}} applicable to \code{\link[coda:mcmc.list]{mcmc.list}} objects.
}
\details{
These implementations should be equivalent (within
numerical error) to the same function being called on
\code{as.matrix(x)}, while avoiding construction of the large matrix.
}
\examples{
data(line, package="coda")
colMeans(as.matrix(line)) # also coda
colMeans.mcmc.list(line) # "Method"
\dontshow{
stopifnot(isTRUE(all.equal(colMeans(as.matrix(line)),colMeans.mcmc.list(line))))
}
data(line, package="coda")
var(as.matrix(line)) # coda
var.mcmc.list(line) # "Method"
\dontshow{
stopifnot(isTRUE(all.equal(var.mcmc.list(line), var(as.matrix(line)))))
}
data(line, package="coda")
colMeans.mcmc.list(line)-1:3
colMeans.mcmc.list(sweep.mcmc.list(line, 1:3))
\dontshow{
stopifnot(isTRUE(all.equal(colMeans.mcmc.list(sweep.mcmc.list(line, 1:3)), colMeans.mcmc.list(line)-1:3)))
}
data(line, package="coda")
colMeans.mcmc.list(line)[c(2,3,1)]
colMeans.mcmc.list(lapply.mcmc.list(line, `[`,,c(2,3,1)))
\dontshow{
stopifnot(isTRUE(all.equal(colMeans.mcmc.list(line)[c(2,3,1)],colMeans.mcmc.list(lapply.mcmc.list(line, `[`,,c(2,3,1))))))
}
}
\seealso{
\code{\link[coda:mcmc.list]{mcmc.list}}

\code{\link[=colMeans]{colMeans()}}

\code{\link[=var]{var()}}

\code{\link[=sweep]{sweep()}}

\code{\link[=lapply]{lapply()}}
}
