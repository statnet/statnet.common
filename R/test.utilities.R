
#' Skip a test if not called via `R CMD` [`check`]
#'
#' A \CRANpkg{testthat} predicate to skip tests if not run as a part
#' of a package [check].
#'
#' @export
skip_if_not_checking <- function() {
  testthat::skip_if_not(testthat::is_checking(), "not inside R CMD check")
}
