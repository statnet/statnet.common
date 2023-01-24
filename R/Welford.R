#  File R/Welford.R in package statnet.common, part of the
#  Statnet suite of packages for network analysis, https://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  https://statnet.org/attribution .
#
#  Copyright 2007-2023 Statnet Commons
################################################################################
#' A Welford accumulator for sample mean and variance
#'
#' A simple class for keeping track of the running mean and the sum of squared deviations from the mean for a vector.
#'
#' @param dn,means,vars initialization of the Welford object: if `means`
#'   and `vars` are given, they are treated as the running means and
#'   variances, and `dn` is their associated sample size, and if not,
#'   `dn` is the dimension of the vector (with sample size 0).
#'
#' @return an object of type `Welford`: a list with four elements:
#' 
#' 1. `n`: Running number of observations
#' 2. `means`: Running mean for each variable
#' 3. `SSDs`: Running sum of squared deviations from the mean for each variable
#' 4. `vars`: Running variance of each variable
#'
#' @examples
#'
#' X <- matrix(rnorm(200), 20, 10)
#' w0 <- Welford(10)
#'
#' w <- update(w0, X)
#' stopifnot(isTRUE(all.equal(w$means, colMeans(X))))
#' stopifnot(isTRUE(all.equal(w$vars, apply(X,2,var))))
#'
#' w <- update(w0, X[1:12,])
#' w <- update(w, X[13:20,])
#' stopifnot(isTRUE(all.equal(w$means, colMeans(X))))
#' stopifnot(isTRUE(all.equal(w$vars, apply(X,2,var))))
#' 
#' w <- Welford(12, colMeans(X[1:12,]), apply(X[1:12,], 2, var))
#' w <- update(w, X[13:20,])
#' stopifnot(isTRUE(all.equal(w$means, colMeans(X))))
#' stopifnot(isTRUE(all.equal(w$vars, apply(X,2,var))))
#' 
#' @export
Welford <- function(dn, means, vars){
  switch(1 + missing(means) + missing(vars),
         structure(list(n = dn, means = means, SSDs = vars*(dn-1), vars = vars), class = "Welford"), # Both means and vars -> dn is sample size.
         stop("Either both ", sQuote("mean"), " and ", sQuote("var"), " should be passed or neither."), # One of the two -> error.
         structure(list(n = 0L, means = numeric(dn), SSDs = numeric(dn), vars = numeric(dn)), class = "Welford") # Neither means nor vars -> dn is dimension.
         )
}

#' @describeIn Welford Update a `Welford` object with new
#'   data.
#'
#' @param object a `Welford` object.
#' @param newdata either a numeric vector of length `d`, a numeric
#'   matrix with `d` columns for a group update, or another `Welford`
#'   object with the same `d`.
#' @param ... additional arguments to methods.
#'
#' @export
update.Welford <- function(object, newdata, ...){
  l <- object
  x <- newdata

  if(is(x, "Welford")){ # Multielement update: newdata is a Welford object.

    if(length(l[[2]]) != length(x[[2]]))
      stop(sQuote("newdata"), " must have the same dimension as ", sQuote("object"))

    l.n <- l[[1]]; x.n <- x[[1]]; l[[1]] <- n.new <- l.n + x.n
    l.m <- l[[2]]; x.m <- x[[2]]
    d <- x.m - l.m
    # In our application, n for x and n for l are going to be similar, so we use weighted average.
    l[[2]] <- (l.n*l.m + x.n*x.m)/n.new
    l[[3]] <- l[[3]] + x[[3]] + d*d*l.n*x.n/n.new

  }else if(is.numeric(x)){ # Either a vector or a matrix with statistics in rows.
    xm <- rbind(x)

    if(length(l[[2]]) != ncol(xm))
      stop(sQuote("newdata"), " must have the same dimension as ", sQuote("object"))

    for(r in seq_len(nrow(xm))){
      x <- xm[r,]
      n.prev <- l[[1]]
      l[[1]] <- n.new <- n.prev + 1
      m.prev <- l[[2]]
      l[[2]] <- m.new <- m.prev + (x-m.prev)/n.new
      l[[3]] <- l[[3]] + (x-m.prev)*(x-m.new)
    }

  }else stop(sQuote("newdata"), " must be either another Welford object, a scalar of correct length, or a matrix with statistics in rows.")

  l[[4]] <- l[[3]]/(l[[1]]-1)

  l
}
