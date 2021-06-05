#  File R/deprecation_utils.R in package statnet.common, part of the Statnet suite
#  of packages for network analysis, https://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  https://statnet.org/attribution
#
#  Copyright 2007-2021 Statnet Commons
#######################################################################

#' @name deprecation-utilities
#' @rdname deprecation-utilities
#' @title Utilities to help with deprecating functions.
#'
NULL

#' @rdname deprecation-utilities
#'
#' @description `.Deprecate_once` calls [.Deprecated()], passing all its arguments
#' through, but only the first time it's called.
#'
#' @param ... arguments passed to [.Deprecated()].
#'
#' @examples
#' \dontrun{
#' options(warn=1) # Print warning immediately after the call.
#' f <- function(){
#'    .Deprecate_once("new_f")
#' }
#' f() # Deprecation warning
#' f() # No deprecation warning
#' }
#' @importFrom utils modifyList
#' @export
.Deprecate_once <- local({
  warned <- c()
  function(...){
    me <- sys.call(-1)
    myname <- format(me[[1L]])
    if(! myname%in%warned){
      do.call(".Deprecated", modifyList(list(old=myname),list(...)))
      warned <<- c(warned, myname)
    }
  }
})

#' @rdname deprecation-utilities
#' @description `.Deprecate_method` calls
#'   [.Deprecated()], but only if a method has been called by name,
#'   i.e., \code{\var{METHOD}.\var{CLASS}}. Like `.Deprecate_once` it
#'   only issues a warning the first time.
#'
#' @param generic,class strings giving the generic function name and
#'   class name of the function to be deprecated.
#'
#' @examples
#' \dontrun{
#' options(warn=1) # Print warning immediately after the call.
#' summary.packageDescription <- function(object, ...){
#'    .Deprecate_method("summary", "packageDescription")
#'    invisible(object)
#' }
#' 
#' summary(packageDescription("statnet.common")) # No warning.
#' summary.packageDescription(packageDescription("statnet.common")) # Warning.
#' summary.packageDescription(packageDescription("statnet.common")) # No warning.
#' }
#' @export
.Deprecate_method <- local({
  warned <- c()
  function(generic, class){
    fullname <- paste(generic,class,sep=".")
    if(! fullname%in%warned){
      me <- sys.call(-1)[[1L]]
      if(length(me)>1 && me[[1L]]=="::") me <- me[[3L]]
      parent <- sys.call(-2)[[1L]]
      if(length(parent)>1 && parent[[1L]]=="::") parent <- parent[[3L]]
      if(me==fullname && NVL(parent,"")!=generic){
        do.call(".Deprecated", list(msg=paste0("You appear to be calling ", fullname,"() directly. ", fullname,"() is a method, and will not be exported in a future version of ", sQuote("ergm"),". Use ", generic, "() instead, or getS3method() if absolutely necessary."), old=fullname))
        warned <<- c(warned, fullname)
      }
    }
  }
})

