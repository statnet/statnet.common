#  File R/startup.utilities.R in package statnet.common, part of the
#  Statnet suite of packages for network analysis, https://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  https://statnet.org/attribution .
#
#  Copyright 2007-2024 Statnet Commons
################################################################################
## .who.loaded.me <- function(){
##   top.call <- sys.calls()[[1L]] # Grab the top-level call.
##   top.fn <- as.character(top.call[[1L]])
  
##   if(length(top.fn)!=1 || !(top.fn %in% c("library","require"))) return(NULL)

##   top.call <- match.call(get(as.character(top.call[[1L]]),baseenv(),mode="function"),top.call) # Expand the arguments.
##   top.call <- as.list(top.call) # Turn the call into a list.
  
##   top.pkg <- top.call$package
  
##   if(!NVL(top.call$character.only,FALSE))
##     as.character(top.pkg)
##   else top.pkg
## }



#' Construct a "standard" startup message to be printed when the package is
#' loaded.
#' 
#' This function uses information returned by [packageDescription()]
#' to construct a standard package startup message according to the
#' policy of the Statnet Project.
#'
#' @param pkgname Name of the package whose information is used.
#' @param friends,nofriends No longer used.
#' 
#'
#' @return A string containing the startup message, to be passed to the
#' [packageStartupMessage()] call or `NULL`, if policy
#' prescribes printing default startup message. (Thus, if
#' [statnetStartupMessage()] returns `NULL`, the calling package should
#' not call [packageStartupMessage()] at all.)
#' 
#' @note Earlier versions of this function printed a more expansive
#'   message. This may change again as the Statnet Project policy
#'   evolves.
#' @seealso [packageDescription()], [packageStartupMessage()]
#' @keywords utilities
#' @examples
#' 
#' \dontrun{
#' .onAttach <- function(lib, pkg){
#'   sm <- statnetStartupMessage("ergm")
#'   if(!is.null(sm)) packageStartupMessage(sm)
#' }
#' }
#' @export
statnetStartupMessage <- function(pkgname, friends = c(), nofriends = c()) {
  desc <- utils::packageDescription(pkgname)

  paste0("\n", sQuote(desc$Package), " ", desc$Version, " (", desc$Date, "), part of the Statnet Project\n",
         "* ", sQuote(paste0("news(package=\"", desc$Package, "\")")), " for changes since last version\n",
         "* ", sQuote(paste0("citation(\"", desc$Package, "\")"))," for citation information\n",
         "* ", sQuote("https://statnet.org"), " for help, support, and other information\n")
}

#' Set [options()] according to a named list, skipping those already
#' set.
#'
#' This function can be useful for setting default options, which do
#' not override options set elsewhere.
#'
#' @param ... see [options()]: either a list of `name=value` pairs or
#'   a single unnamed argument giving a named list of options to set.
#'
#' @return The return value is same as that of [options()] (omitting
#'   options already set).
#'
#' @examples
#' options(onesetting=1)
#'
#' default_options(onesetting=2, anothersetting=3)
#' stopifnot(getOption("onesetting")==1) # Still 1.
#' stopifnot(getOption("anothersetting")==3)
#'
#' default_options(list(yetanothersetting=5, anothersetting=4))
#' stopifnot(getOption("anothersetting")==3) # Still 3.
#' stopifnot(getOption("yetanothersetting")==5)
#' @export
default_options <- function(...){
  x <- list(...)

  if(is.null(names(x))){
    if(length(x)==1) x <- x[[1]]
    else stop("invalid argument")
  }
  if(all(names(x)=="")) stop("list argument has no valid names")
  if(any(names(x)=="")) stop("invalid argument")

  toset <- setdiff(names(x), names(options()))
  do.call(options, x[toset])
}
