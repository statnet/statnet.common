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

#' @describeIn xTAx Evaluate \eqn{x'Ax} for vector or matrix \eqn{x}
#'   and square matrix \eqn{A}.
#'
#' @param x a vector
#' @param A a square matrix
#'
#' @export
xTAx <- function(x, A) {
  drop(crossprod(crossprod(A, x), x))
}

#' @describeIn xTAx Evaluate \eqn{xAx'} for vector or matrix \eqn{x}
#'   and square matrix \eqn{A}.
#'
#' @export
xAxT <- function(x, A) {
  drop(x %*% tcrossprod(A, x))
}

#' @describeIn xTAx Evaluate \eqn{x'A^{-1}x} for vector or matrix
#'   \eqn{x} and invertible matrix \eqn{A} using [solve()].
#'
#' @export
xTAx_solve <- function(x, A, ...) {
  drop(crossprod(x, solve(A, x, ...)))
}

## TODO: The xAxT versions may benefit from optimization.

#' @describeIn xTAx As the corresponding `xTAx_*()` function, but with
#'   \eqn{x} transposed.
#' @export
xAxT_solve <- function(x, A, ...) {
  xTAx_solve(t(x), A, ...)
}

#' Check that matrix or vector `b` is in the span of matrix `a`. If so,
#' evaluate `f(a^-1 b)` via QR decomposition and wrap the output to
#' include rank and nullity.
#'
#' @param f a function that takes a vector or a matrix
#' @param a,b,tol passed to [qr()]; `tol` is also used for span
#'   checking.
#' @param nm a vector of names for `a` and `b` to use; defaults to the
#'   corresponding expressions in the parent frame.
#'
#' @return Results of `f` with additional attributes `"rank"` and
#'   `"nullity"`.
#' @noRd
with_qrcoef <- function(f, a, b, tol,
                        nm = c(deparse1(substitute(a)),
                               deparse1(substitute(b)))) {
  qr <- qr(a)
  rank <- qr$rank
  q <- qr.Q(qr)

  if (nullity <- min(dim(q)) - rank) {
    if (rank > 0L) q <- q[, -seq_len(rank), drop = FALSE]
    if (!all(abs(crossprod(q, b)) < tol))
      stop(simpleError(paste(nm[[2]], "is not in the span of", nm[[1]]),
                       call = sys.call(-1L)))
  }

  structure(f(qr.coef(qr, b)), rank = rank, nullity = nullity)
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
  with_qrcoef(\(z) sum(x * z, na.rm = TRUE), A, x, tol)
}

#' @describeIn xTAx As the corresponding `xTAx_*()` function, but with
#'   \eqn{x} transposed.
#' @export
xAxT_qrsolve <- function(x, A, tol = 1e-07, ...) {
  xTAx_qrsolve(t(x), A, tol, ...)
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

#' @describeIn xTAx Evaluate \eqn{x' A^{-1} x} for vector or matrix
#'   \eqn{x} and matrix \eqn{A} (symmetric, nonnegative-definite) via
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

#' @describeIn xTAx As the corresponding `xTAx_*()` function, but with
#'   \eqn{x} transposed.
#' @export
xAxT_eigen <- function(x, A, tol=sqrt(.Machine$double.eps), ...) {
  xTAx_eigen(t(x), A, tol, ...)
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
#'   than singular-value decomposition. If it's "obvious" that the
#'   matrix is not SSND (e.g., negative diagonal elements), an error
#'   is raised. It defaults to `TRUE` for eigenvalue-based methods and
#'   `FALSE` for QR-based methods.
#'
#' @param x,a,b,X,A,B,tol,... corresponding arguments of the wrapped functions.
#'
#' @export
ssolve <- function(a, b, ..., snnd = FALSE) {
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
xAxT_seigen <- function(x, A, tol=sqrt(.Machine$double.eps), ...) {
  xTAx_seigen(t(x), A, tol, ...)
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
#' @export
xAxT_ssolve <- function(x, A, ...) {
  xTAx_ssolve(t(x), A, ...)
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

  with_qrcoef(function(z) sum(x * d * z, na.rm = TRUE),
              A * dd, x * d, tol, c("A", "x"))
}

#' @rdname ssolve
#'
#' @export
xAxT_qrssolve <- function(x, A, tol = 1e-07, ...) {
  xTAx_qrssolve(t(x), A, tol, ...)
}

#' @rdname ssolve
#'
#' @export
sandwich_ssolve <- function(A, B, ...) {
  ssolve(A, t(ssolve(A, B, ...)), ...)
}

#' @rdname ssolve
#' @export
qrssolve <- function(a, b, tol = 1e-07, ..., snnd = FALSE) {
  if(missing(b)) {
    b <- diag(1, nrow(a))
    colnames(b) <- rownames(a)
  }

  if(snnd) {
    d <- .sqrt_inv_diag(a)
    dd <- d * rep(d, each = length(d))
    x <- with_qrcoef(function(z) z * d,
                     a * dd, b * d, tol, c("a", "b"))
  } else {
    d <- .inv_diag(a)
    ## NB: In R, vector * matrix and matrix * vector always scales
    ## corresponding rows.
    x <- with_qrcoef(identity,
                     a * d, b * d, tol, c("a", "b"))
  }
  replace(x, is.na, 0)
}

#' @rdname ssolve
#' @export
qrsolve <- function(a, b, tol = 1e-07, ...) {
  if(missing(b)) {
    b <- diag(1, nrow(a))
    colnames(b) <- rownames(a)
  }

  with_qrcoef(identity, a, b, tol) |> replace(is.na, 0)
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


#' (Pseudo-)Determinant of the ratio of two matrices
#'
#' @param num,denom numerator and denominator matrices.
#' @param log whether to return log-pseudo-determinant.
#' @param root whether to return \eqn{p}th root of the ratio, where
#'   \eqn{p} is the effective rank.
#' @param ... additional arguments to [qrssolve()], particularly `tol`
#'   for determining the cut-off for estimating the rank and `snnd`
#'   for whether `denom` can be assumed to be symmetric and positive
#'   non-negative definite.
#'
#' @note Kernel of `denom` must be contained in the kernel of `num`,
#'   or equivalently, the span of `num` must be contained in the span
#'   of `denom`.
#'
#' @return The pseudo-determinant, with an additional attribute
#'   `"rank"` giving the number of eigenvalues used.
#'
#' @export
pdet_rat <- function(num, denom, log = FALSE, root = FALSE, ...) {
  r <- qrssolve(denom, num, ...)
  rank <- attr(r, "rank")
  eigen(r, only.values = TRUE)$values[seq_len(rank)] |>
    log() |>
    (if (root) mean else sum)() |>
    (if (log) identity else exp)() |>
    structure(rank = rank)
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


#' Specialization of Common Functions to Matrices
#'
#' Being able to assume two dimensions reduces overhead.
#'
#' @param x a matrix.
#' @param MARGIN,FUN,simplify,check.margin,STATS,... See help for corresponding function.
#' @name matrix_helpers
#' @examples
#'
#' x <- matrix(runif(1000), ncol = 4)
NULL


#' @describeIn matrix_helpers Highly optimized [`sweep`]`(x, 2, STATS,
#'   "-")` specifically for numeric matrices.
#'
#' @param disable_checks if \code{TRUE}, do not check that \code{x} is a
#' numeric matrix and its number of columns matches the length of \code{STATS};
#' set in production code for a significant speed-up.
#' @examples
#'
#' s <- 1:4
#'
#' stopifnot(all.equal(sweep_cols.matrix(x, s), sweep(x, 2, s)))
#'
#' @export
sweep_cols.matrix <- function(x, STATS, disable_checks=FALSE){
  if (!disable_checks)
    if (!is.matrix(x) || mode(x) != "numeric" || ncol(x) != length(STATS)) warning("x is not a numeric matrix")
  o <- .Call("sweep2m", x, STATS, PACKAGE = "statnet.common")
  attributes(o) <- attributes(x)
  o
}

#' @describeIn matrix_helpers [`sweep`]`(x, 2L, STATS, FUN, ...)`,
#'   though for convenience, STATS can also be a function that is
#'   applied to each column; this also disables the margin check by default.
#' @examples
#' stopifnot(all.equal(sweep(x, 2, colMeans(x)), sweep.matrix(x, 2, colMeans(x))))
#' stopifnot(all.equal(sweep(x, 2, colMeans(x)), sweep.matrix(x, 2, mean)))
#' @export
sweep.matrix <- function(x, MARGIN, STATS, FUN = "-", check.margin = TRUE, ...) {
  FUN <- match.fun(FUN)
  if (is.function(STATS)) {
    STATS <- apply.matrix(x, MARGIN, STATS, ...)
    check.margin <- FALSE
  }

  if (MARGIN == 1L) {
    if (check.margin) stopifnot(nrow(x) == length(STATS))
    for (i in seq_len(nrow(x))) x[i, ] <- FUN(x[i, ], STATS[i], ...)
  } else if (MARGIN == 2L) {
    if (check.margin) stopifnot(ncol(x) == length(STATS))
    for (i in seq_len(ncol(x))) x[, i] <- FUN(x[, i], STATS[i], ...)
  } else {
    stop("MARGIN is not 1 or 2")
  }
  x
}

#' @describeIn matrix_helpers [`apply`]`(x, 2L, FUN, ...)`.
#'
#' @examples
#' stopifnot(all.equal(apply.matrix(x, 2, min), apply(x, 2, min)))
#' @export
apply.matrix <- function(x, MARGIN, FUN, ..., simplify = TRUE) {
  FUN <- match.fun(FUN)
  if (MARGIN == 1L) {
    l <- seq_len(nrow(x))
    f <- function(i) FUN(x[i, ], ...)
  } else if (MARGIN == 2L) {
    l <- seq_len(ncol(x))
    f <- function(i) FUN(x[, i], ...)
  } else {
    stop("MARGIN is not 1 or 2")
  }
  sapply(l, f, simplify = simplify)
}


#' Identify Matrix Columns with a Small Range
#'
#' @param x a [`matrix`] or a list of matrices, such as an
#'   [`mcmc.list`].
#' @param tol a vector (recycled as needed) giving maximum half-ranges
#'   allowed.
#'
#' @return A logical vector.
#'
#' @examples
#' M <- cbind(rnorm(5), rnorm(10, 1, 1e-16))
#' M
#' sweep(M, 2L, M[1L, ])
#' (sweep(M, 2L, M[1L, ]) == 0) |> apply(2, all)
#' cols_constant(M)
#'
#' @export
cols_constant <- function(x, tol = .Machine$double.eps * 2) {
  x <- x |>
    unclass() |>
    enlist() |>
    lapply(unclass)
  mins <- lapply(x, apply.matrix, 2L, min) |> Reduce(x = _, pmin)
  maxs <- lapply(x, apply.matrix, 2L, max) |> Reduce(x = _, pmax)
  maxs - mins <= tol * 2
}
