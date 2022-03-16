#' A Welford accumulator for sample mean and variance
#'
#' A simple class for keeping track of the running mean and the sum of squared deviations from the mean.
#'
#' @param d the dimension of the data vector
#'
#' @return an object of type `Welford`: a list with three elements:
#' 
#' 1. Running number of observations
#' 2. Running mean for each variable
#' 3. Running sum of squared deviations from the mean for each variable
#'
#' @examples
#' X <- matrix(rnorm(200), 20, 10)
#' w0 <- Welford(10)
#'
#' w <- update(w0, X)
#' stopifnot(isTRUE(all.equal(mean(w), colMeans(X))))
#' stopifnot(isTRUE(all.equal(var(w), apply(X,2,var))))
#'
#' w <- w0 %>% update(X[1:12,]) %>% update(X[13:20,])
#' stopifnot(isTRUE(all.equal(mean(w), colMeans(X))))
#' stopifnot(isTRUE(all.equal(var(w), apply(X,2,var))))
#' 
#' @export
Welford <- function(d){
  structure(list(0L, numeric(d), numeric(d)), class = "Welford")
}

#' @describeIn Welford Update a `Welford` object with new
#'   data.
#'
#' @param object,x a `Welford` object.
#' @param newdata either a numeric vector of length `d`, a numeric
#'   matrix with `d` columns for a group update, or another `Welford`
#'   object with the same `d`.
#'
#' @export
update.Welford <- function(object, newdata){
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

    if(length(l[[2]]) != ncol(x[[2]]))
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

  l
}

#' @describeIn Welford Return the running mean vector.
#' @export
mean.Welford <- function(x, ...){
  if(...length()) warning(sQuote("mean"), " method for Welford objects only uses the first argument and ignores the others at this time")
  x[[2]]
}

#' @export
var <- function(x, ...){
  UseMethod("var")
}

#' @export
var.default <- stats::var

#' @describeIn Welford Return the running variance vector.
#' @export
var.Welford <- function(x, ...){
  if(...length()) warning(sQuote("var"), " method for Welford objects only uses the first argument and ignores the others at this time")
  x[[3]]/(x[[1]]-1)
}
