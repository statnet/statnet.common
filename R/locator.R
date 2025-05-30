#  File R/locator.R in package statnet.common, part of the Statnet suite of
#  packages for network analysis, https://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free, open
#  source, and has the attribution requirements (GPL Section 7) at
#  https://statnet.org/attribution .
#
#  Copyright 2007-2025 Statnet Commons
################################################################################
#' A simple dictionary to cache recent InitFunction lookups.
#'
#' @param name function name.
#' @param env the environment name for the function; if `NULL`, look
#'   up in cache, otherwise insert or overwrite.
#'
#' @return A character string giving the name of the environment
#'   containing the function, or `NULL` if not in cache.
#' @noRd
locate_function_cache <- local({
  cache <- list()
  watchlist <- character(0) # Packages being watched for unloading.
  pkglist <- character(0) # Current list of packages.
  # Reset the cache and update the list of watched packages.
  reset <- function(...){
    pkglist <<- .packages()
    new <- setdiff(pkglist, watchlist)
    for(pkg in new){
      setHook(packageEvent(pkg, "detach"), reset)
      setHook(packageEvent(pkg, "onUnload"), reset)
    }
    watchlist <<- c(watchlist, new)
    cache <<- list()
  }
  # Check if new namespaces have been added.
  checknew <- function(){
    if(!setequal(.packages(), pkglist)) reset()
  }
  function(name, env=NULL){
    checknew()
    if(is.null(env)){
      cache[[name]]
    }else{
      cache[[name]] <<- env
    }
  }
})

#' Locate a function with a given name and return it and its environment.
#'
#' These functions first search the given environment, then search all
#' loaded environments, including those where the function is not
#' exported. If found, they return an unambiguous reference to the
#' function.
#' 
#' @name locate_function
NULL

#' @describeIn locate_function a low-level function returning the
#'   reference to the function named `name`, or `NULL` if not found.
#'
#' @param name a character string giving the function's name.
#' @param env an [`environment`] where it should search first.
#' @param ... additional arguments to the warning and error warning messages. See Details.
#'
#' @return If the function is found, an unevaluated call of the form
#'   `ENVNAME:::FUNNAME`, which can then be used to call the function
#'   even if it is unexported. If the environment does not have a
#'   name, or is `GlobalEnv`, only `FUNNAME` is returned. Otherwise,
#'   `NULL` is returned.
#'
#' @details If the initial search fails, a search using
#'   [getAnywhere()] is attempted, with exported ("visible") functions
#'   with the specified name preferred over those that are not. When
#'   multiple equally qualified functions are available, a warning is
#'   printed and an arbitrary one is returned.
#'
#'   Because [getAnywhere()] can be slow, past searches are cached.
#'
#' @examples
#'
#' # Locate a random function in base.
#' locate_function(".row_names_info")
#' 
#' @export
locate_function <- function(name, env = globalenv(), ...){
  if(is.call(name)) name <- name[[1]]
  name <- as.character(name)
  
  # Try the given environment...
  if(!is.null(obj<-get0(name, mode='function', envir=env))){
    env <- environment(obj)
    envname <- environmentName(env)
    # Check that environment name is not blank or globalenv(), and
    # that the detected environment actually contains the object.
    if(! NVL(envname,"") %in% c("", "R_GlobalEnv") && exists(name, mode='function', envir=env, inherits=FALSE)) return(call(":::",as.name(envname),as.name(name)))
    else return(as.name(name))
  }

  # Try the cache...
  envname <- locate_function_cache(name)
  if(!is.null(envname)) return(call(":::",as.name(envname),as.name(name)))

  # Use getAnywhere()...
  #' @importFrom utils getAnywhere
  m <- getAnywhere(name)
  if(length(m$objs)){
    ## Prioritise visible over not:
    if(any(m$visible)){
      m <- lapply(unclass(m)[-1], "[", m$visible)
    }
    if(length(m$objs)>1) warning("Name ",name," matched by multiple objects; using the first one on the list.", ...)
    envname <- environmentName(environment(m$objs[[1]]))
    locate_function_cache(name, envname)
    return(call(":::",as.name(envname),as.name(name)))
  }
  NULL
}

#' @describeIn locate_function a helper function that searches for a
#'   function of the form `prefix.name` and produces an informative
#'   error message if not found.
#'
#' @param prefix a character string giving the prefix, so the
#'   searched-for function is `prefix.name`.
#' @param errname a character string; if given, if the function is not
#'   found an error is raised, with `errname` prepended to the error
#'   message.
#' @param call. a logical, whether the call
#'   (`locate_prefixed_function`) should be a part of the error
#'   message; defaults to `FALSE` (which is different from [stop()]'s
#'   default).
#'
#' @export
locate_prefixed_function <- function(name, prefix, errname, env = globalenv(), ..., call.=FALSE){
  if(is.call(name)) name <- name[[1]]
  name <- as.character(name)
  fname <- paste(prefix,name,sep=".")
  f <- locate_function(fname, env, ...)
  if(is.null(f) && !is.null(errname)) stop(errname,' ', sQuote(name), " function ", sQuote(fname), " not found.", ..., call.=call.)
  else f
}
