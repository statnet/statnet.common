#  File R/matrix.utils.R in package statnet.common, part of the
#  Statnet suite of packages for network analysis, https://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  https://statnet.org/attribution .
#
#  Copyright 2007-2023 Statnet Commons
################################################################################
#' Test if the object is a matrix that is symmetric and positive definite
#'
#' @param x the object to be tested.
#' @param tol the tolerance for the reciprocal condition number.
#'
#' @export
is.SPD <- function(x, tol = .Machine$double.eps) {
  is.matrix(x) &&
    nrow(x) == ncol(x) &&
    all(x == t(x)) &&
    rcond(x) >= tol &&
    all(eigen(x, symmetric=TRUE, only.values=TRUE)$values > 0)
}


#' Common quadratic forms
#'
#' @name xTAx
#'
#' @details These are somewhat inspired by emulator::quad.form.inv()
#'   and others.
NULL

#' @describeIn xTAx Evaluate \eqn{x'Ax} for vector \eqn{x} and square
#'   matrix \eqn{A}.
#'
#' @param x a vector
#' @param A a square matrix
#'
#' @export
xTAx <- function(x, A) {
  drop(crossprod(crossprod(A, x), x))
}

#' @describeIn xTAx Evaluate \eqn{xAx'} for vector \eqn{x} and square
#'   matrix \eqn{A}.
#'
#' @export
xAxT <- function(x, A) {
  drop(x %*% tcrossprod(A, x))
}

#' @describeIn xTAx Evaluate \eqn{x'A^{-1}x} for vector \eqn{x} and
#'   invertible matrix \eqn{A} using [solve()].
#'
#' @export
xTAx_solve <- function(x, A, ...) {
  drop(crossprod(x, solve(A, x, ...)))
}

#' @describeIn xTAx Evaluate \eqn{x'A^{-1}x} for vector \eqn{x} and
#'   matrix \eqn{A} using QR decomposition and confirming that \eqn{x}
#'   is in the span of \eqn{A} if \eqn{A} is singular; returns `rank`
#'   and `nullity` as attributes just in case subsequent calculations
#'   (e.g., hypothesis test degrees of freedom) are affected.
#'
#' @param tol tolerance argument passed to the relevant subroutine
#'
#' @export
xTAx_qrsolve <- function(x, A, tol = 1e-07, ...) {
  Aqr <- qr(A, tol=tol, ...)
  nullity <- NCOL(A) - Aqr$rank
  if(nullity && !all(abs(crossprod(qr.Q(Aqr)[,-seq_len(Aqr$rank), drop=FALSE], x))<tol))
    stop("x is not in the span of A")
  structure(sum(x*qr.coef(Aqr, x), na.rm=TRUE), rank=Aqr$rank, nullity=nullity)
}

#' @describeIn xTAx Evaluate \eqn{A^{-1}B(A')^{-1}} for \eqn{B} a
#'   square matrix and \eqn{A} invertible.
#'
#' @param B a square matrix
#' @param ... additional arguments to subroutines
#'
#' @export
sandwich_solve <- function(A, B, ...) {
  solve(A, t(solve(A, B, ...)), ...)
}


#' Wrappers around matrix algebra functions that pre-*s*cale their
#' arguments
#'
#' Covariance matrices of variables with very different orders of
#' magnitude can have very large ratios between their greatest and
#' their least eigenvalues, causing them to appear to the algorithms
#' to be near-singular when they are actually very much SPD. These
#' functions first scale the matrix's rows and/or columns by its
#' diagonal elements and then undo the scaling on the result.
#'
#' `ssolve()`, `sginv()`, and `snearPD()` wrap [solve()],
#' [MASS::ginv()], and [Matrix::nearPD()], respectively. `srcond()`
#' returns the reciprocal condition number of [rcond()] net of the
#' above scaling. `xTAx_ssolve`, `xTAx_qrssolve`, and
#' `sandwich_ssolve` wrap the corresponding \pkg{statnet.common}
#' functions.
#'
#' @param snnd assume that the matrix is symmetric non-negative
#'   definite (SNND). If it's "obvious" that it's not (e.g., negative
#'   diagonal elements), an error is raised.
#'
#' @param x,a,b,X,A,B,tol,... corresponding arguments of the wrapped functions.
#'
#' @export
ssolve <- function(a, b, ..., snnd = TRUE) {
  if(missing(b)) {
    b <- diag(1, nrow(a))
    colnames(b) <- rownames(a)
  }

  d <- diag(as.matrix(a))
  d <- ifelse(d==0, 1, 1/d)

  if(snnd) {
    d <- sqrt(d)
    if(anyNA(d)) stop("Matrix a has negative elements on the diagonal, and snnd=TRUE.")
    a <- a * d * rep(d, each = length(d))
    solve(a, b*d, ...) * d
  } else {
    ## NB: In R, vector * matrix and matrix * vector always scales
    ## corresponding rows.
    solve(a*d, b*d, ...)
  }
}


#' @rdname ssolve
#'
#' @export
sginv <- function(X, ..., snnd = TRUE) {
  d <- diag(as.matrix(X))
  d <- ifelse(d==0, 1, 1/d)

  if(snnd) {
    d <- sqrt(d)
    if(anyNA(d)) stop("Matrix a has negative elements on the diagonal, and snnd=TRUE.")
    dd <- rep(d, each = length(d)) * d
    X <- X * dd
    MASS::ginv(X, ...) * dd
  } else {
    dd <- rep(d, each = length(d))
    MASS::ginv(X*d, ...) * dd
  }
}

#' @rdname ssolve
#'
#' @export
srcond <- function(x, ..., snnd = TRUE) {
  d <- diag(as.matrix(x))
  d <- ifelse(d==0, 1, 1/d)

  if(snnd) {
    d <- sqrt(d)
    if(anyNA(d)) stop("Matrix a has negative elements on the diagonal, and snnd=TRUE.")
    dd <- rep(d, each = length(d)) * d
    rcond(x*dd)
  } else {
    rcond(x*d, ...)
  }
}

#' @rdname ssolve
#'
#' @export
snearPD <- function(x, ...) {
  d <- abs(diag(as.matrix(x)))
  d[d==0] <- 1
  d <- sqrt(d)
  if(anyNA(d)) stop("Matrix x has negative elements on the diagonal.")
  dd <- rep(d, each = length(d)) * d
  x <- Matrix::nearPD(x / dd, ...)
  x$mat <- x$mat * dd
  x
}

#' @rdname ssolve
#'
#' @export
xTAx_ssolve <- function(x, A, ...) {
  drop(crossprod(x, ssolve(A, x, ...)))
}

#' @rdname ssolve
#'
#' @examples
#' x <- rnorm(2, sd=c(1,1e12))
#' x <- c(x, sum(x))
#' A <- matrix(c(1, 0, 1,
#'               0, 1e24, 1e24,
#'               1, 1e24, 1e24), 3, 3)
#' stopifnot(all.equal(
#'   xTAx_qrssolve(x,A),
#'   structure(drop(x%*%sginv(A)%*%x), rank = 2L, nullity = 1L)
#' ))
#'
#' x <- rnorm(2, sd=c(1,1e12))
#' x <- c(x, rnorm(1, sd=1e12))
#' A <- matrix(c(1, 0, 1,
#'               0, 1e24, 1e24,
#'               1, 1e24, 1e24), 3, 3)
#'
#' stopifnot(try(xTAx_qrssolve(x,A), silent=TRUE) ==
#'   "Error in xTAx_qrssolve(x, A) : x is not in the span of A\n")
#'
#' @export
xTAx_qrssolve <- function(x, A, tol = 1e-07, ...) {
  d <- diag(as.matrix(A))
  d <- ifelse(d==0, 1, 1/d) |> sqrt()

  if(anyNA(d)) stop("Matrix x has negative elements on the diagonal.")
  dd <- rep(d, each = length(d)) * d

  Aqr <- qr(A*dd, tol=tol, ...)
  nullity <- NCOL(A) - Aqr$rank
  if(nullity && !all(abs(crossprod(qr.Q(Aqr)[,-seq_len(Aqr$rank), drop=FALSE], x*d))<tol))
    stop("x is not in the span of A")
  structure(sum(x*d*qr.coef(Aqr, x*d), na.rm=TRUE), rank=Aqr$rank, nullity=nullity)
}

#' @rdname ssolve
#'
#' @export
sandwich_ssolve <- function(A, B, ...) {
  ssolve(A, t(ssolve(A, B, ...)), ...)
}
