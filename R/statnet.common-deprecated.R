#' @name statnet.common-deprecated
#' @title Deprecated functions from `statnet.common`
#' @param ... arguments to deprecated functions.
NULL

#' @rdname statnet.common-deprecated
#' @export
compact.rle <- function(...){
  .Deprecate_once("compress")
  compress(...)
}
