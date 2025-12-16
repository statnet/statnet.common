#  File R/test.utilities.R in package statnet.common, part of the Statnet suite
#  of packages for network analysis, https://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free, open
#  source, and has the attribution requirements (GPL Section 7) at
#  https://statnet.org/attribution .
#
#  Copyright 2007-2025 Statnet Commons
################################################################################

#' Skip a test if not called via `R CMD` [`check`]
#'
#' A \CRANpkg{testthat} predicate to skip tests if not run as a part
#' of a package [check].
#'
#' @export
skip_if_not_checking <- function() {
  testthat::skip_if_not(testthat::is_checking(), "not inside R CMD check")
}
