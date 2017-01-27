\name{logspace.utils}
\alias{log_mean_exp}
\alias{log_sum_exp}
\alias{lweighted.mean}
\title{
  Utilities for performing calculations on logarithmic scale.
}
\description{
  A small suite of functions to compute sums, means, and weighted means
  on logarithmic scale, minimizing loss of precision.
}
\usage{
log_mean_exp(logx, use_ldouble = FALSE)
log_sum_exp(logx, use_ldouble = FALSE)
lweighted.mean(x, logw)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{logx}{
    Numeric vector of \eqn{\log(x)}, the natural logarithms of the values
    to be summed or averaged.
  }
  \item{x}{
    Numeric vector of \eqn{x}, the (raw) values
    to be summed or averaged.
  }
  \item{logw}{
    Numeric vector of \eqn{\log(w)}, the natural logarithms of the weights.
  }
  \item{use_ldouble}{
    Whether to use \code{long double} precision in the calculation. If
    \code{TRUE}, \R's C built-in \code{logspace_sum()} is used. If
    \code{FALSE}, the package's own implementation based on it is used,
    using \code{double} precision, which is (on most systems) several
    times faster, at the cost of precision.
  }
}
\value{
  The functions return the equivalents of the following R expressions,
  but faster and with less loss of precision:
  \describe{
    \item{\code{log_sum_exp(logx)}}{\code{log(sum(exp(logx)))}}
    \item{\code{log_mean_exp(logx)}}{\code{log(mean(exp(logx)))}}
    \item{\code{lweighted.mean(x, logw)}}{\code{sum(x*exp(logw))/sum(exp(logw))}}
  }
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Pavel N. Krivitsky
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
logx <- rnorm(1000)
stopifnot(all.equal(log(sum(exp(logx))),log_sum_exp(logx)))
stopifnot(all.equal(log(mean(exp(logx))),log_mean_exp(logx)))

x <- rnorm(1000)
logw <- rnorm(1000)
stopifnot(all.equal(sum(x*exp(logw))/sum(exp(logw)),lweighted.mean(x, logw)))
}
\keyword{arith}