#  File R/matrix.utils.R in package statnet.common, part of the Statnet suite
#  of packages for network analysis, https://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free, open
#  source, and has the attribution requirements (GPL Section 7) at
#  https://statnet.org/attribution .
#
#  Copyright 2007-2025 Statnet Commons
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

#' @describeIn xTAx Evaluate \eqn{x' A^{-1} x} for vector \eqn{x} and
#'   matrix \eqn{A} (symmetric, nonnegative-definite) via
#'   eigendecomposition and confirming that \eqn{x} is in the span of
#'   \eqn{A} if \eqn{A} is singular; returns `rank` and `nullity` as
#'   attributes just in case subsequent calculations (e.g., hypothesis
#'   test degrees of freedom) are affected.
#'
#'   Decompose \eqn{A = P L P'} for \eqn{L} diagonal matrix of
#'   eigenvalues and \eqn{P} orthogonal. Then \eqn{A^{-1} = P L^{-1}
#'   P'}.
#'
#'   Substituting, \deqn{x' A^{-1} x = x' P L^{-1} P' x
#'   = h' L^{-1} h} for \eqn{h = P' x}.
#'
#' @export
xTAx_eigen <- function(x, A, tol=sqrt(.Machine$double.eps), ...) {
  e <- eigen(A, symmetric=TRUE)
  keep <- e$values > max(tol * e$values[1L], 0)
  h <- crossprod(e$vectors, x)
  if(!all(keep) && !all(abs(h[!keep,])<tol))
    stop("x is not in the span of A")

  h <- h[keep, , drop=FALSE]
  structure(drop(crossprod(h, h/e$values[keep])), rank = sum(keep), nullity = sum(!keep))
}

.inv_diag <- function(X, zero = .Machine$double.xmax/(1 + .Machine$double.eps)){
  d <- diag(as.matrix(X))
  ifelse(d==0, zero, 1/d)
}

.sqrt_inv_diag <- function(X){
  Xname <- deparse1(substitute(X))
  d <- .inv_diag(X)
  d <- suppressWarnings(sqrt(d))
  if(anyNA(d)) stop("Matrix ", sQuote(Xname), " assumed symmetric and non-negative-definite has negative elements on the diagonal.")
  d
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
#' `ginv_eigen()` reimplements [MASS::ginv()] but using
#' eigendecomposition rather than SVD; this means that it is only
#' suitable for symmetric matrices, but that detection of negative
#' eigenvalues is more robust.
#'
#' `ssolve()`, `sginv()`, `sginv_eigen()`, and `snearPD()` wrap
#' [solve()], [MASS::ginv()], `ginv_eigen()`, and [Matrix::nearPD()],
#' respectively. `srcond()` returns the reciprocal condition number of
#' [rcond()] net of the above scaling. `xTAx_ssolve()`,
#' `xTAx_qrssolve()`, `xTAx_seigen()`, and `sandwich_ssolve()` wrap
#' the corresponding \pkg{statnet.common} functions. `qrssolve()`
#' solves the linear system via QR decomposition after scaling by
#' diagonal.
#'
#' @param snnd assume that the matrix is symmetric non-negative
#'   definite (SNND). This typically entails scaling that converts
#'   covariance to correlation and use of eigendecomposition rather
#'   than singular-value decomposition. If it's "obvious" that the matrix is
#'   not SSND (e.g., negative diagonal elements), an error is raised.
#'
#' @param x,a,b,X,A,B,tol,... corresponding arguments of the wrapped functions.
#'
#' @export
ssolve <- function(a, b, ..., snnd = TRUE) {
  if(missing(b)) {
    b <- diag(1, nrow(a))
    colnames(b) <- rownames(a)
  }

  if(snnd) {
    d <- .sqrt_inv_diag(a)
    a <- a * d * rep(d, each = length(d))
    solve(a, b*d, ...) * d
  } else {
    d <- .inv_diag(a)
    ## NB: In R, vector * matrix and matrix * vector always scales
    ## corresponding rows.
    solve(a*d, b*d, ...)
  }
}


#' @rdname ssolve
#'
#' @export
sginv <- function(X, ..., snnd = TRUE){
  if(snnd) {
    d <- .sqrt_inv_diag(X)
    dd <- rep(d, each = length(d)) * d
    ginv_eigen(X * dd, ...) * dd
  } else {
    d <- .inv_diag(X)
    dd <- rep(d, each = length(d))
    MASS::ginv(X * dd, ...) * t(dd)
  }
}

#' @rdname ssolve
#' @export
ginv_eigen <- function(X, tol = sqrt(.Machine$double.eps), ...){
  e <- eigen(X, symmetric=TRUE)
  keep <- e$values > max(tol * e$values[1L], 0)
  tcrossprod(e$vectors[, keep, drop=FALSE] / rep(e$values[keep],each=ncol(X)), e$vectors[, keep, drop=FALSE])
}


#' @rdname ssolve
#'
#' @export
xTAx_seigen <- function(x, A, tol=sqrt(.Machine$double.eps), ...) {
  d <- .sqrt_inv_diag(A)
  dd <- rep(d, each = length(d)) * d

  A <- A * dd
  x <- x * d

  xTAx_eigen(x, A, tol=tol, ...)
}

#' @rdname ssolve
#'
#' @export
srcond <- function(x, ..., snnd = TRUE) {
  if(snnd) {
    d <- .sqrt_inv_diag(x)
    dd <- rep(d, each = length(d)) * d
    rcond(x*dd)
  } else {
    d <- .inv_diag(x)
    rcond(x*d, ...)
  }
}

#' @rdname ssolve
#'
#' @export
snearPD <- function(x, ...) {
  d <- abs(diag(as.matrix(x)))
  d[d==0] <- 1
  d <- suppressWarnings(sqrt(d))
  if(anyNA(d)) stop("Matrix ", sQuote("x"), " has negative elements on the diagonal.")
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
#' stopifnot(isTRUE(all.equal(
#'   xTAx_qrssolve(x,A),
#'   structure(drop(x%*%sginv(A)%*%x), rank = 2L, nullity = 1L)
#' )))
#'
#' stopifnot(isTRUE(all.equal(c(A %*% qrssolve(A, x)), x)))
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
  d <- .sqrt_inv_diag(A)
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

#' @rdname ssolve
#' @export
qrssolve <- function(a, b, tol = 1e-07, ..., snnd = TRUE) {
  if(missing(b)) {
    b <- diag(1, nrow(a))
    colnames(b) <- rownames(a)
  }

  if(snnd) {
    d <- .sqrt_inv_diag(a)
    dd <- d * rep(d, each = length(d))

    aqr <- qr(a*dd, tol = tol, ...)
    nullity <- min(nrow(a), ncol(a)) - aqr$rank
    if(nullity && !all(abs(crossprod(qr.Q(aqr)[,-seq_len(aqr$rank), drop=FALSE], b*d))<tol))
      stop("b is not in the span of a")
    x <- qr.coef(aqr, b*d)*d
  } else {
    d <- .inv_diag(a)
    ## NB: In R, vector * matrix and matrix * vector always scales
    ## corresponding rows.
    aqr <- qr(a*d, tol = tol, ...)
    nullity <- min(nrow(a), ncol(a)) - aqr$rank
    if(nullity && !all(abs(crossprod(qr.Q(aqr)[,-seq_len(aqr$rank), drop=FALSE], b*d))<tol))
      stop("b is not in the span of a")
    x <- qr.coef(aqr, b*d)
  }
  structure(replace(x, is.na(x), 0), rank=aqr$rank, nullity=nullity)
}

#' @rdname ssolve
#' @export
qrsolve <- function(a, b, tol = 1e-07, ...) {
  if(missing(b)) {
    b <- diag(1, nrow(a))
    colnames(b) <- rownames(a)
  }

  aqr <- qr(a, tol = tol, ...)
  nullity <- min(nrow(a), ncol(a)) - aqr$rank
  if(nullity && !all(abs(crossprod(qr.Q(aqr)[,-seq_len(aqr$rank), drop=FALSE], b))<tol))
      stop("b is not in the span of a")
    x <- qr.coef(aqr, b)

  structure(replace(x, is.na(x), 0), rank=aqr$rank, nullity=nullity)
}

#' @rdname ssolve
#'
#' @export
sandwich_qrssolve <- function(A, B, ...) {
  qrssolve(A, t(qrssolve(A, B, ...)), ...)
}

#' @rdname ssolve
#'
#' @export
sandwich_qrsolve <- function(A, B, ...) {
  qrsolve(A, t(qrsolve(A, B, ...)), ...)
}

#' @rdname xTAx
#'
#' @export
sandwich_sginv <- function(A, B, ...) {
  Ai <- sginv(A, ...)
  Ai %*% B %*% t(Ai)
}

#' @rdname xTAx
#'
#' @export
sandwich_ginv <- function(A, B, ...) {
  Ai <- MASS::ginv(A, ...)
  Ai %*% B %*% t(Ai)
}

#' Conveniently covert between coordinate-value and array representations
#'
#' These function similarly to \CRANpkg{Matrix}'s utilities but is
#' simpler and allows arbitrary baseline and handling of missing
#' values. (It is also almost certainly much slower.) Also, since it
#' is likely that operations will be performed on the elements of the
#' array, their argument is first for easier piping.
#'
#' If `x0` is `NA`, non-`NA` elements are returned; if `x0` is `NULL`,
#' all elements are.
#'
#' @param x values of elements differing from the default.
#' @param coord an integer matrix of their indices.
#' @param dim dimension vector; recycled to `ncol(coord)`; if not
#'   given, inferred from `dimnames`.
#' @param x0 the default value.
#' @param dimnames dimension name list.
#' @param X an array.
#' @param na.rm whether the `NA` elements of the array should be
#'   omitted from the list.
#'
#' @return `coo_to_arr()` returns a matrix or an array.
#'
#' @examples
#' m <- matrix(rpois(25, 1), 5, 5)
#' arr_to_coo(m, 0L)
#' stopifnot(identical(do.call(arr_from_coo, arr_to_coo(m, 0L)), m))
#'
#' stopifnot(length(arr_to_coo(m, NULL)$x) == 25) # No baseline
#'
#' m[sample.int(25L, 2L)] <- NA
#' m
#' arr_to_coo(m, 0L) # Return NAs
#'
#' arr_to_coo(m, 0L, na.rm = TRUE) # Drop NAs
#' @export
arr_from_coo <- function(x, coord, dim = lengths(dimnames), x0 = NA, dimnames = NULL) {
  coord <- as.matrix(coord)
  dim <- rep_len(dim, ncol(coord))
  if (anyNA(dim)) stop("array dimensions not specified")
  replace(array(NVL(x0, NA), dim, dimnames), coord, x)
}

#' @rdname arr_from_coo
#' @return `arr_to_coo()` returns a list with the following elements:
#'
#' \item{`x`}{the values distinct from `x0`}
#'
#' \item{`coord`}{a matrix with a column for each dimension containing
#' indexes of values distinct from `x0`}
#'
#' \item{`dim`}{the dimension vector of the matrix}
#'
#' \item{`dimnames`}{the dimension name list of the matrix}
#'
#' @export
arr_to_coo <- function(X, x0, na.rm = FALSE) {
  nz  <- if (is.null(x0)) seq_along(X)
         else if (is.na(x0)) which(!is.na(X))
         else if (na.rm) which(X != x0)
         else which(is.na(X) | X != x0)
  coord <- arrayInd(nz, d <- dim(X), dn <- dimnames(X), TRUE)
  list(x = X[nz], coord = coord, dim = d, x0 = x0, dimnames = dn)
}

#' Return the matrix with diagonal set to a specified value
#'
#' This function simply assigns `value` to diagonal of `x` and returns
#' `x`.
#'
#' @param x a square matrix.
#' @param value a value or a vector (recycled to the required length).
#'
#' @export
set_diag <- function(x, value) {
  diag(x) <- value
  x
}
