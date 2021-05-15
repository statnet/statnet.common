#  File R/control.utilities.R in package statnet.common, part of the Statnet suite
#  of packages for network analysis, https://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  https://statnet.org/attribution
#
#  Copyright 2007-2020 Statnet Commons
#######################################################################
.autodetect_dep_warn <- local({
  warned <- c()
  function(caller = as.character(ult(sys.calls(),3)[[1L]])){
    if(!caller %in% warned)
      warning("In ",sQuote(caller),": Autodetection of acceptable control parameter generators and of the calling function name has been deprecated and will be removed in a future version. They must be set explicitly.", call.=FALSE)
    warned <<- c(warned, caller)
  }
})

#' Ensure that the class of the control list is one of those that can
#' be used by the calling function
#'
#' This function converts an ordinary `list` into a control list (if
#' needed) and checks that the control list passed is appropriate for
#' the function to be controlled.
#' 
#' @param OKnames List of control function names which are acceptable.
#' @param myname Name of the calling function (used in the error
#'   message).
#' @param control The control list or a list to be converted to a
#'   control list using `control.myname()`. Defaults to the
#'   \code{control} variable in the calling function. See Details for
#'   detailed behavior.
#'
#' @note In earlier versions, `OKnames` and `myname` were
#'   autodetected. This capability has been deprecated and results in
#'   a warning issued once per session. They now need to be set
#'   explicitly.
#'
#' @details `check.control.class()` performs the check by looking up
#'   the class of the `control` argument (defaulting to the `control`
#'   variable in the calling function) and checking if it matches a
#'   list of acceptable given by `OKnames`.
#'
#'   Before performing any checks, the `control` argument (including
#'   the default) will be converted to a control list by calling
#'   [as.control.list()] on it with the first element of `OKnames` to
#'   construct the control function.
#'
#'   If `control` is missing, it will be assumed that the user wants
#'   to modify it in place, and a variable with that name in the
#'   parent environment will be overwritten.
#'
#' @return A valid control list for the function in which it is to be
#'   used. If `control` argument is missing, it will also overwrite
#'   the variable `control` in the calling environment with it.
#'
#' @seealso [set.control.class()], [print.control.list()], [as.control.list()]
#' @keywords utilities
#' @export
check.control.class <- function(OKnames=as.character(ult(sys.calls(),2)[[1L]]), myname=as.character(ult(sys.calls(),2)[[1L]]), control=get("control",pos=parent.frame())){
  overwrite_control <- missing(control)
  control <- as.control.list(control, OKnames[1])

  if(missing(OKnames) || missing(myname)) .autodetect_dep_warn() 
  funs <- paste("control", OKnames, sep=".")

  # Control missing: overwrite default name in parent.
  if(overwrite_control) assign("control", control, pos=parent.frame())
  
  if(inherits(control, funs[1L])) return(control)
  
  for(fun in funs[-1]) # If there is only one, that's a null vector, so it just terminates.
    if(inherits(control, fun)){
      warning("Using ", fun,"(...) as the control parameter of ",myname,"(...) is suboptimal and may overwrite some settings that should be preserved. Use ",funs[1L],"(...) instead.")
      return(control)
    }
  
  stop("Invalid control parameters for ",myname,"(...): ",class(control)[1L],"(...). Use ",funs[1L],"(...) to construct them instead.", call.=FALSE)
}



#' Set the class of the control list
#' 
#' This function sets the class of the control list, with the default being the
#' name of the calling function.
#' 
#' 
#' @param myname Name of the class to set.
#' @param control Control list. Defaults to the \code{control} variable in the
#' calling function.
#' @return The control list with class set.
#' @note In earlier versions, `OKnames` and `myname` were autodetected. This capability has been deprecated and results in a warning issued once per session. They now need to be set explicitly.
#' @seealso [check.control.class()], [print.control.list()]
#' @keywords utilities
#' @export
set.control.class <- function(myname=as.character(ult(sys.calls(),2)[[1L]]), control=get("control",pos=parent.frame())){
  if(missing(myname)) .autodetect_dep_warn()
  class(control) <- c(myname, "control.list", "list")
  control
}

#' Handle standard `control.*()` function semantics.
#'
#' This function takes the arguments of its caller (whose name should
#' be passed explicitly), plus any `...` arguments and produces a
#' control list based on the standard semantics of `control.*()`
#' functions, including handling deprecated arguments, identifying
#' undefined arguments, and handling arguments that should be passed
#' through [match.arg()].
#'
#' @param myname the name of the calling function.
#' @param ... the `...` argument of the control function, if present.
#'
#' @details The function behaves based on the information it acquires from the calling function. Specifically,
#'
#' * The values of formal arguments (except `...`, if present) are
#'   taken from the environment of the calling function and stored in
#'   the list.
#'
#' * If the calling function has a `...` argument *and* defines an
#'   `old.controls` variable in its environment, then it remaps the
#'   names in `...` to their new names based on `old.controls`. In
#'   addition, if the value is a list with two elements, `action` and
#'   `message`, the standard deprecation message will have `message`
#'   appended to it and then be called with `action()`.
#'
#' * If the calling function has a `match.arg.pars` in its
#'   environment, the arguments in that list are processed through
#'   [match.arg()].
#'
#' @return a list with formal arguments of the calling function.
#' @export
handle.controls <- function(myname, ...){
  formal.args <- formals(sys.function(-1))
  if(has.dots <- "..." %in% names(formal.args)) formal.args[["..."]] <- NULL

  control <- list()
  for(arg in names(formal.args))
    control[arg] <- list(get(arg, parent.frame()))

  if(has.dots){
    old.controls <- if(exists("old.controls", parent.frame())) get("old.controls", parent.frame()) else list()

    for(arg in names(list(...))){
      if(is.null(newarg <- old.controls[[arg]])){
        stop("Unrecognized control parameter for ", sQuote(paste0(myname, "()")), ": ", sQuote(arg), ".", call.=FALSE)
      }else if(is.list(newarg)){
        newarg$action("Control parameter ",sQuote(paste0(arg,"=..."))," to ", sQuote(paste0(myname, "()")), " is no longer used.", newarg$message, call.=FALSE)
      }else{
        warning("Passing ",sQuote(paste0(arg,"=..."))," to ", sQuote(paste0(myname, "()")), " is deprecated and may be removed in a future version. Specify it as ", sQuote(paste0(myname, "(", old.controls[[arg]], "=...)")), " instead.", call.=FALSE)
        control[old.controls[[arg]]]<-list(list(...)[[arg]])
      }
    }
  }

  if(exists("match.arg.pars", parent.frame()))
    for(arg in get("match.arg.pars", parent.frame()))
      control[arg] <- list(match.arg(control[[arg]][1], eval(formal.args[[arg]])))

  control
}


#' Pretty print the control list
#' 
#' This function prints the control list, including what it can control and the
#' elements.
#' 
#' 
#' @param x A list generated by a \code{control.*} function.
#' @param \dots Additional argument to print methods for individual settings.
#' @param indent an argument for recursive calls, to facilitate indentation of nested lists.
#' @seealso \code{\link{check.control.class}}, \code{\link{set.control.class}}
#' @keywords utilities
#' @export
print.control.list <- function(x, ..., indent=""){
  cat("Control parameter list generated by", sQuote(class(x)[1L]), "or equivalent. Non-empty parameters:\n")
  for(name in names(x)){
    if(length(x[[name]])){
      cat(indent, name,": ",sep="")
      if(is.list(x[[name]])) {print(x[[name]], ..., indent=paste0(indent,"   "))} else cat(paste(deparse(x[[name]]), collapse=" "),"\n", sep="")
    }
  }
}

#' Named element accessor for ergm control lists
#' 
#' Utility method that overrides the standard `$' list accessor to disable
#' partial matching for ergm \code{control.list} objects
#' 
#' Executes \code{\link[base]{getElement}} instead of \code{\link[base]{$}} so
#' that element names must match exactly to be returned and partially matching
#' names will not return the wrong object.
#' 
#' @param object list-coearceable object with elements to be searched
#' @param name literal character name of list element to search for and return
#' @return Returns the named list element exactly matching \code{name}, or
#' \code{NULL} if no matching elements found
#' @author Pavel N. Krivitsky
#' @seealso see \code{\link{getElement}}
#' @name control.list.accessor
#' @export
`$.control.list` <- function(object, name) object[[name, exact = TRUE]]



#' Overwrite control parameters of one configuration with another.
#' 
#' Given a \code{control.list}, and two prefixes, \code{from} and \code{to},
#' overwrite the elements starting with \code{to} with the corresponding
#' elements starting with \code{from}.
#' 
#' 
#' @param control An object of class \code{control.list}.
#' @param from Prefix of the source of control parameters.
#' @param to Prefix of the destination of control parameters.
#' @return An \code{control.list} object.
#' @author Pavel N. Krivitsky
#' @seealso \code{\link{print.control.list}}
#' @keywords utilities
#' @examples
#' 
#' (l <- set.control.class("test", list(a.x=1, a.y=2)))
#' control.remap(l, "a", "b")
#' 
#' @export
control.remap <- function(control, from, to){
  from <- paste0("^",from,"\\.")
  to <- paste0(to,"\\.")
  nfrom <- grep(from, names(control), value=TRUE)
  nto <- sub(from, to, nfrom)
  for(i in seq_along(nfrom)) control[[nto[i]]] <- control[[nfrom[i]]]
  control
}


#' Identify and the differences between two control lists.
#' @param x a `control.list`
#' @param y a reference `control.list`; defaults to the default
#'   settings for `x`.
#' @param ignore.environment whether environment for
#'   environment-bearing parameters (such as formulas and functions)
#'   should be considered when comparing.
#' @param ... Additional arguments to methods.
#'
#' @return An object of class `diff.control.list`: a named list with
#'   an element for each non-identical setting. The element is either
#'   itself a `diff.control.list` (if the setting is a control list)
#'   or a named list with elements `x` and `y`, containing `x`'s and
#'   `y`'s values of the parameter for that setting.
#' @export
diff.control.list <- function(x, y=eval(call(class(x)[[1L]])), ignore.environment=TRUE, ...){
  d <- list()
  for(name in union(names(x),names(y))){
    d[[name]] <-
      if(is(x[[name]], "control.list") && is(y[[name]], "control.list")) EVL(diff(x[[name]], y[[name]]))
      else if(!identical(x[[name]],y[[name]],ignore.environment=ignore.environment)) list(x=x[[name]], y=y[[name]])
  }
  structure(d, class=c("diff.control.list", "list"), xclass=c(class(x)[1L]))
}

#' @describeIn diff.control.list A print method.
#' @param indent an argument for recursive calls, to facilitate
#'   indentation of nested lists.
#' @export
print.diff.control.list <- function(x, ..., indent = ""){
  if(length(x)==0) cat("No difference between parameter lists generated by", sQuote(attr(x,"xclass")), "or equivalent.\n")
  else{
    cat("Difference between parameter lists generated by", sQuote(attr(x,"xclass")), "or equivalent. Differences:\n")
    for(name in names(x)){
      if(length(x[[name]])){
        cat(indent, name,": ",sep="")
        if(is(x[[name]],"diff.control.list")) print(x[[name]], ..., indent=paste0(indent,"   "))
        else if(is.list(x[[name]]$x)){
          print(x[[name]]$x, ..., indent=paste0(indent,"   "))
          cat("versus")
          print(x[[name]]$y, ..., indent=paste0(indent,"   "))
        }else{
          cat(paste(deparse(x[[name]]$x), collapse=" "), " versus ",
              paste(deparse(x[[name]]$y), collapse=" "), "\n", sep="")
        }
      }
    }
  }
}

#' Convert to a control list.
#'
#' @param x An object, usually a [`list`], to be converted to a
#'   control list.
#' @param ... Additional arguments to methods.
#' @return a `control.list` object.
#'
#' @examples
#' myfun <- function(..., control=control.myfun()){
#'   as.control.list(control)
#' }
#' control.myfun <- function(a=1, b=a+1){
#'   list(a=a,b=b)
#' }
#'
#' myfun()
#' myfun(control = list(a=2))
#' @export
as.control.list <- function(x, ...) UseMethod("as.control.list")

#' @describeIn as.control.list Idempotent method for control lists.
#' @export
as.control.list.control.list <- function(x, ...) x

#' @describeIn as.control.list The method for plain lists, which runs
#'   them through `FUN`.
#' @param FUN Either a `control.*()` function or its name or suffix
#'   (to which `"control."` will be prepended); defaults to taking the
#'   nearest (in the call traceback) function that does not begin with
#'   `"as.control.list"`, and prepending `"control."` to it. (This is
#'   typically the function that called `as.control.list()` in the
#'   first place.)
#' @param unflat Logical, indicating whether an attempt should be made
#'   to detect whether some of the arguments are appropriate for a
#'   lower-level control function and pass them down.
#' @examples
#' myfun2 <- function(..., control=control.myfun2()){
#'   as.control.list(control)
#' }
#' control.myfun2 <- function(c=3, d=c+2, myfun=control.myfun()){
#'   list(c=c,d=d,myfun=myfun)
#' }
#'
#' myfun2()
#' # Argument to control.myfun() (i.e., a) gets passed to it, and a
#' # warning is issued for unused argument e.
#' myfun2(control = list(c=3, a=2, e=3))
#' @export
as.control.list.list <- function(x, FUN=NULL, unflat=TRUE, ...){
  if(is.null(FUN)){
    FUN <-
      ult(
        Filter(function(x) !startsWith(x, "as.control.list"),
             vapply(
               lapply(sys.calls(), # Obtain the traceback.
                      `[[`, 1L), # Extract the function names as names.
               as.character, character(1)) # Convert to character vectors.
             ) # Drop those that begin with "as.control.list".
      ) # Take the last one.
  }
  if(is.character(FUN) && !startsWith(FUN, "control.")) FUN <- paste0("control.", FUN)

  FUN <- match.fun(FUN)

  if(unflat){
    xnames_unused <- names(x)
    unflat <- function(f){
      args <- formals(f)
      anames <- setdiff(names(args), "...")

      l <- list()
      for(aname in names(args))
        if(aname %in% names(x)){ # Present in the input list: copy.
          l[aname] <- list(x[[aname]])
          xnames_unused <<- setdiff(xnames_unused, aname)
        }else if(is.call(aval <- args[[aname]]) && startsWith(as.character(aval[[1]]), "control.")){ # A control list not supplied directly: process recursively.
          l[aname] <- list(unflat(get(as.character(aval[[1]]), pos=environment(f), mode="function")))
        }
      # Otherwise, leave blank.

      l
    }
    x <- unflat(FUN)
    if(length(xnames_unused)) warning("Control arguments ", paste.and(sQuote(xnames_unused)), " not used in any of the control functions.", call.=FALSE, immediate.=TRUE)
  }

  do.call(FUN, x, envir=parent.frame())
}

#' Statnet Control
#'
#' A utility to facilitate argument completion of control lists.
#'
#' In and of itself, `snctrl` copies its named arguments into a
#' list. However, its argument list is updated dynamically as packages
#' are loaded, as are those of its reexports from other packages. This
#' is done using an API provided by helper functions. (See `API?snctrl`.)
#'
#' @param ... The parameter list is updated dynamically as packages
#'   are loaded and unloaded. Their current list is given below.
#'
#' @section Currently recognised control parameters:
#' This list is updated as packages are loaded and unloaded.
#'
#' \Sexpr[results=rd,stage=render]{statnet.common::snctrl_names()}
#'
#' @note You may see messages along the lines of
#' ```
#' The following object is masked from 'package:PKG':
#' snctrl
#' ```
#' when loading packages. They are benign.
#' 
#' @export
snctrl <- function(...){
  control <- list(...)
  if(length(control)){
    if(any(names(control)=="")) stop("All arguments to ",sQuote("snctrl")," must be named.", call.=FALSE)
    warning("The following arguments to ",sQuote("snctrl")," are not recognised: ", paste.and(sQuote(names(control))), call.=FALSE, immediate.=TRUE)
  }

  formal.args<-formals(sys.function())
  formal.args[["..."]] <- NULL
  for(arg in names(formal.args)){
    if(arg=="") stop("All arguments to ",sQuote("snctrl")," must be named.", call.=FALSE)
    if(!do.call(missing, list(arg)))
      control[arg] <- list(get(arg))
  }
  control
}

#' @describeIn snctrl-API Typeset the currently defined list of
#'   argument names by package and control function.
#'
#' @export
snctrl_names <- function(){
  a <- argnames()
  pkgs <- sapply(names(a), function(pkg){
    funs <- lapply(names(a[[pkg]]), function(ctrl){
      ctrll <- nchar(ctrl)
      args <- names(a[[pkg]][[ctrl]])
      paste0("\\item{\\code{\\link[",pkg,":",ctrl,"]{",ctrl,"}}}{\\code{", paste0(strwrap(paste0(args,collapse=", "),simplify=TRUE,exdent=ctrll+1),collapse="\n"), "}}")
    })
    paste0("\\subsection{Package \\pkg{",pkg,"}}{\\describe{", paste0(funs,collapse="\n"),"}}")
  })
  paste0(pkgs,collapse="\n")
}

argnames <- local({
  cache <- list()

  delpkg <- function(pkgname,pkgpath){
    cache[[pkgname]] <<- NULL
    update_snctrl()
  }

  function(pkg, arglists){
    if(missing(pkg)) cache
    else{
      cache[[pkg]] <<- arglists
      setHook(packageEvent(pkg, "onUnload"), delpkg)
    }
  }
})


callbacks <- local({
  cache <- list()

  delpkg <- function(pkgname,pkgpath){
    cache[[pkgname]] <<- NULL
  }

  function(pkg, callback){
    if(missing(pkg)) cache
    else{
      cache[[pkg]] <<- callback
      setHook(packageEvent(pkg, "onUnload"), delpkg)
    }
  }
})

#' @name snctrl-API
#' @title Helper functions used by packages to facilitate [`snctrl`] updating.
#'
NULL

#' @describeIn snctrl-API Typically called from [.onLoad()], Update the
#'   argument list of [snctrl()] to include additional argument names
#'   associated with the package, and set a callback for the package
#'   to update its own copy.
#'
#' @param myname Name of the package defining the arguments.
#' @param arglists A named list of argument name-default pairs. If the
#'   list is not named, it is first passed through
#'   [collate_controls()].
#' @param callback A function with no arguments that updates the
#'   packages own copy of [snctrl()].
#'
#' @return `update_snctrl()` has no return value and is used for its side-effects.
#' @export
update_snctrl <- function(myname, arglists=NULL, callback=NULL){
  if(length(arglists) && all(names(arglists)=="")) arglists <- do.call(collate_controls, arglists)
  
  # Make a copy and replace the arglist.
  tmp <- snctrl

  if(!missing(myname)){
    argnames(myname, arglists)
    if(!is.null(callback)) callbacks(myname, callback)
  }

  arglists <- c(argnames(),list(list(formals(tmp)[1])))
  arglist <- unlist(unlist(lapply(unname(arglists), unname), recursive=FALSE), recursive=FALSE)
  argnames <- sort(unique(names(arglist)))
  arglist <- structure(rep(list(substitute()), length(argnames)), # For now, leave all default arguments blank.
                       names = argnames)
  formals(tmp) <- arglist
  # Replace the original function with the copy.
  unlockBinding("snctrl", environment(snctrl))
  snctrl <<- tmp
  lockBinding("snctrl", environment(snctrl))

  for(callback in callbacks()) callback()

  invisible()
}

#' @describeIn snctrl-API Obtain and concatenate the argument lists of
#'   specified functions or all functions starting with dQuote(`control.`) in
#'   the environment.
#'
#' @param x Either a function, a list of functions, or an
#'   environment. If `x` is an environment, all functions starting
#'   with dQuote(`control.`) are obtained.
#' @param ... Additional functions or lists of functions.
#'
#' @return `collate_controls()` returns the combined list of name-default pairs of each function.
#' @export
collate_controls <- function(x=NULL, ...){
  l <- if(is.environment(x)) lapply(grep("^control\\.*", ls(pos=x), value=TRUE), mget, x, mode="function", ifnotfound=list(NULL)) else list(x)
  l <- unlist(c(list(...), l))

  arglists <- lapply(l, formals)
}

#' @describeIn snctrl-API A stored expression that, if evaluated, will
#'   create a callback function `update_my_snctrl()` that will update
#'   the client package's copy of [snctrl()].
#' @format `UPDATE_MY_SCTRL_EXPR` is a quoted expression meant to be passed directly to [eval()].
#' @examples
#' \dontrun{
#' # In the client package (outside any function):
#' eval(UPDATE_MY_SCTRL_EXPR)
#' }
#' @export
UPDATE_MY_SCTRL_EXPR <- quote(
  update_my_snctrl <- function(){
    unlockBinding("snctrl", environment(update_my_snctrl))
    snctrl <<- statnet.common::snctrl
    lockBinding("snctrl", environment(update_my_snctrl))
  }
)

#' @describeIn snctrl-API A stored expression that, if evaluated on
#'   loading, will add arguments of the package's `control.*()`
#'   functions to [snctrl()] and set the callback.
#' @format `COLLATE_ALL_MY_CONTROLS_EXPR` is a quoted expression meant to be passed directly to [eval()].
#' @examples
#' \dontrun{
#' # In the client package:
#' .onLoad <- function(libame, pkgname){
#'   # ... other code ...
#'   eval(statnet.common::COLLATE_ALL_MY_CONTROLS_EXPR)
#'   # ... other code ...
#' }
#' }
#' @export
COLLATE_ALL_MY_CONTROLS_EXPR <- quote(
  statnet.common::update_snctrl(pkgname,
                     list(environment(.onLoad)),
                     update_my_snctrl)
)
