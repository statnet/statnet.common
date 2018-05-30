#  File R/logspace.utils.R in package statnet.common, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2007-2018 Statnet Commons
#######################################################################
#' Utilities for performing calculations on logarithmic scale.
#' 
#' A small suite of functions to compute sums, means, and weighted means on
#' logarithmic scale, minimizing loss of precision.
#' 
#' 
#' @aliases log_mean_exp log_sum_exp lweighted.mean lweighted.var
#' @param logx Numeric vector of \eqn{\log(x)}, the natural logarithms of the
#' values to be summed or averaged.
#' @param x Numeric vector of \eqn{x}, the (raw) values to be summed or
#' averaged. For \code{lweighted.mean}, \code{x} may also be a matrix, in which
#' case the weighted mean will be computed for each column of \code{x}.
#' @param logw Numeric vector of \eqn{\log(w)}, the natural logarithms of the
#' weights.
#' @param use_ldouble Whether to use \code{long double} precision in the
#' calculation. If \code{TRUE}, 's C built-in \code{logspace_sum()} is used. If
#' \code{FALSE}, the package's own implementation based on it is used, using
#' \code{double} precision, which is (on most systems) several times faster, at
#' the cost of precision.
#' @return The functions return the equivalents of the following R expressions,
#' but faster and with less loss of precision: \describe{
#' \item{`log_sum_exp(logx)`}{\code{log(sum(exp(logx)))}}
#' \item{`log_mean_exp(logx)`}{\code{log(mean(exp(logx)))}}
#' \item{`lweighted.mean(x,logw)`}{\code{sum(x*exp(logw))/sum(exp(logw))} for \code{x} scalar and
#' \code{colSums(x*exp(logw))/sum(exp(logw))} for \code{x} matrix}
#' \item{`lweighted.var(x,logw)`}{\code{crossprod(x*exp(logw/2))/sum(exp(logw))}} }
#' @author Pavel N. Krivitsky
#' @keywords arith
#' @examples
#' 
#' logx <- rnorm(1000)
#' stopifnot(all.equal(log(sum(exp(logx))), log_sum_exp(logx)))
#' stopifnot(all.equal(log(mean(exp(logx))), log_mean_exp(logx)))
#' 
#' x <- rnorm(1000)
#' logw <- rnorm(1000)
#' stopifnot(all.equal(m <- sum(x*exp(logw))/sum(exp(logw)),lweighted.mean(x, logw)))
#' stopifnot(all.equal(sum((x-m)^2*exp(logw))/sum(exp(logw)),
#'                     lweighted.var(x, logw), check.attributes=FALSE))
#' 
#' x <- cbind(x, rnorm(1000))
#' stopifnot(all.equal(m <- colSums(x*exp(logw))/sum(exp(logw)),
#'                     lweighted.mean(x, logw), check.attributes=FALSE))
#' stopifnot(all.equal(crossprod(t(t(x)-m)*exp(logw/2))/sum(exp(logw)),
#'                     lweighted.var(x, logw), check.attributes=FALSE))
#' @name logspace.utils
#' @useDynLib statnet.common
#' @export
log_sum_exp <- function(logx, use_ldouble=FALSE){
  if(length(logx)==0) -Inf
  else .Call("log_sum_exp_wrapper", logx, use_ldouble, PACKAGE="statnet.common")
}

#' @rdname logspace.utils
#' @export
log_mean_exp <- function(logx, use_ldouble=FALSE){
  if(length(logx)==0) NaN
  else .Call("log_sum_exp_wrapper", logx, use_ldouble, PACKAGE="statnet.common") - log(length(logx))
}

#' @rdname logspace.utils
#' @export
lweighted.mean <- function(x, logw){
  d <- dim(x)
  if(is.null(d)){ # Vector
    if(length(x)==0) NaN
    else if(length(x)!=length(logw)) stop("x and logw must have the same length")
    else .Call("logspace_wmean_wrapper", x, logw, PACKAGE="statnet.common")
  }else if(length(d)>2){
    stop("Arrays of 3 or more dimensions are not supported at this time.")
  }else{ # Matrix
    if(d[1]==0) rep(NaN, d[2])
    else if(d[1]!=length(logw)) stop("logw must have the same length as the number of rows in x")
    else .Call("logspace_wmeans_wrapper", x, logw, PACKAGE="statnet.common")
  }
}

#' @rdname logspace.utils
#' @export
lweighted.var <- function(x, logw){
  E <- lweighted.mean(x, logw)
  if(is.null(dim(x))){
    if(length(x)<2) return(NA)
    x <- x - E
    lweighted.mean(x*x, logw)
  }else{
    if(nrow(x)<2) return(matrix(NA, 1, ncol(x)))
    .Call("logspace_wmean2_wrapper", sweep_cols.matrix(x, E), logw, PACKAGE="statnet.common")
  }
}



#' Suptract a elements of a vector from respective columns of a matrix
#' 
#' An optimized function equivalent to \code{sweep(x, 2, STATS)} for a matrix
#' \code{x}.
#' 
#' 
#' @param x a numeric matrix;
#' @param STATS a numeric vector whose length equals to the number of columns
#' of \code{x}.
#' @param disable_checks if \code{TRUE}, do not check that \code{x} is a
#' numeric matrix and its number of columns matches the length of \code{STATS};
#' set in production code for a significant speed-up.
#' @return A matrix of the same attributes as \code{x}.
#' @examples
#' 
#' x <- matrix(runif(1000), ncol=4)
#' s <- 1:4
#' 
#' stopifnot(all.equal(sweep_cols.matrix(x, s), sweep(x, 2, s)))
#' 
#' @export 
sweep_cols.matrix <- function(x, STATS, disable_checks=FALSE){
  if(!disable_checks)
    if(!is.matrix(x) || mode(x)!="numeric" || ncol(x)!=length(STATS)) stop("Argument ",sQuote("x")," must be a numeric matrix variable (not an expression that evaluates to a numeric matrix).")
  o <- .Call("sweep2m", x, STATS, PACKAGE="statnet.common")
  attributes(o) <- attributes(x)
  o
}
