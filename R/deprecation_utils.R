
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
#' @examples
#' \donttest{
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
    myname <- as.character(me[[1]])
    if(length(myname)>1 && myname[[1]]=="::") myname <- myname[[3]]
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
#' @examples
#' \donttest{
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
      me <- sys.call(-1)[[1]]
      if(length(me)>1 && me[[1]]=="::") me <- me[[3]]
      parent <- sys.call(-2)[[1]]
      if(length(parent)>1 && parent[[1]]=="::") parent <- parent[[3]]
      if(me==fullname && NVL(parent,"")!=generic){
        do.call(".Deprecated", list(msg=paste0("You appear to be calling ", fullname,"() directly. ", fullname,"() is a method, and will not be exported in a future version of ", sQuote("ergm"),". Use ", generic, "() instead, or getS3method() if absolutely necessary."), old=fullname))
        warned <<- c(warned, fullname)
      }
    }
  }
})
