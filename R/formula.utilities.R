#  File R/formula.utilities.R in package statnet.common, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2007-2017 Statnet Commons
#######################################################################
###################################################################
## This file has utilities whose primary purpose is examining or ##
## manipulating ERGM formulas.                                   ##
###################################################################

#' Functions for Querying, Validating and Extracting from ERGM Formulas
#' 
#' \code{append.rhs.formula} appends a list of terms to the RHS of a
#' formula. If the formula is one-sided, the RHS becomes the LHS, if
#' \code{keep.onesided==FALSE} (the default).
#' 
#' @param object formula object to be updated
#' @param newterms list of terms (names) to append to the formula, or a formula
#' whose RHS terms will be used
#' @param keep.onesided if the initial formula is one-sided, keep it whether to
#' keep it one-sided or whether to make the initial formula the new LHS
#' @param \dots Additional arguments. Currently unused.
#' @return 
#' \code{append.rhs.formula} each return an
#' updated formula object
#' @examples
#' 
#' ## append.rhs.formula
#' 
#' (f1 <- append.rhs.formula(y~x,list(as.name("z1"),as.name("z2"))))
#' (f2 <- append.rhs.formula(~y,list(as.name("z"))))
#' (f3 <- append.rhs.formula(~y+x,list(as.name("z"))))
#' (f4 <- append.rhs.formula(~y,list(as.name("z")),TRUE))
#' (f5 <- append.rhs.formula(y~x,~z1+z2))
#' 
#' \dontshow{
#' stopifnot(f1 == (y~x+z1+z2))
#' stopifnot(f2 == (y~z))
#' stopifnot(f3 == (y+x~z))
#' stopifnot(f4 == (~y+z))
#' stopifnot(f5 == (y~x+z1+z2))
#' }
#'
#' @export
#' @rdname formula.utilities
append.rhs.formula<-function(object,newterms,keep.onesided=FALSE){
  if(inherits(newterms,"formula")) newterms <- term.list.formula(newterms[[length(newterms)]])
  for(newterm in newterms){
    if(length(object)==3) object[[3]]<-call("+",object[[3]],newterm)
    else if(keep.onesided) object[[2]]<-call("+",object[[2]],newterm)
    else object[[3]]<-newterm
  }
  object
}

#' @describeIn formula.utilities
#'
#' \code{nonsimp.update.formula} is a reimplementation of
#' \code{\link{update.formula}} that does not simplify.  Note that the
#' resulting formula's environment is set as follows. If
#' \code{from.new==FALSE}, it is set to that of object. Otherwise, a new
#' sub-environment of object, containing, in addition, variables in new listed
#' in from.new (if a character vector) or all of new (if TRUE).
#' 
#' @param new new formula to be used in updating
#' @param from.new logical or character vector of variable names. controls how
#' environment of formula gets updated.
#' @return 
#' \code{nonsimp.update.formula} each return an
#' updated formula object
#' @importFrom stats as.formula
#' @export
nonsimp.update.formula<-function (object, new, ..., from.new=FALSE){
  old.lhs <- if(length(object)==2) NULL else object[[2]]
  old.rhs <- if(length(object)==2) object[[2]] else object[[3]]
  
  new.lhs <- if(length(new)==2) NULL else new[[2]]
  new.rhs <- if(length(new)==2) new[[2]] else new[[3]]
  
  sub.dot <- function(c, dot){
    if(is.null(dot)) c # If nothing to substitute with, just return it.
    else if(is.call(c)) as.call(c(list(c[[1]]), lapply(c[-1], sub.dot, dot))) # If it's a call, construct a call consisting of the call and each of the arguments with the substitution performed, recursively.
    else if(is.name(c) && c==".")  dot # If it's a dot, return substitute.
    else c # If it's anything else, just return it.
  }
  
  deparen<- function(c, ops = c("+","*")){
    if(is.call(c)){
      if(as.character(c[[1]]) %in% ops){
        op <- as.character(c[[1]])
        if(length(c)==2 && is.call(c[[2]]) && c[[2]][[1]]==op)
          return(deparen(c[[2]], ops))
        else if(length(c)==3 && is.call(c[[3]]) && c[[3]][[1]]==op){
          if(length(c[[3]])==3)
            return(call(op, call(op, deparen(c[[2]],ops), deparen(c[[3]][[2]],ops)), deparen(c[[3]][[3]],ops)))
          else
            return(call(op, deparen(c[[2]],ops), deparen(c[[3]][[2]],ops)))
        }
      }
      return(as.call(c(list(c[[1]]), lapply(c[-1], deparen, ops)))) # If it's a non-reducible call, construct a call consisting of the call and each of the arguments with the substitution performed, recursively.
    }else return(c)
  }
  
  out <- if(length(new)==2) call("~", deparen(sub.dot(new.rhs, old.rhs))) else call("~", deparen(sub.dot(new.lhs, old.lhs)), deparen(sub.dot(new.rhs, old.rhs)))

  #  a new sub-environment for the formula, containing both
  # the variables from the old formula and the new.
  
  if(identical(from.new,FALSE)){ # The new formula will use the environment of the original formula (the default).
    e <- environment(object)
  }else{
    # Create a sub-environment also containing variables from environment of new.
    e <- new.env(parent=environment(object))
    
    if(identical(from.new,TRUE)) from.new <- ls(pos=environment(new)) # If TRUE, copy all of them (dangerous!).
    
    for(name in from.new)
      assign(name, get(name, pos=environment(new)), pos=e)
  }

  as.formula(out, env = e)
}

#' @describeIn formula.utilities
#'
#' \code{term.list.formula} returns a list containing terms in a given
#' formula, handling \code{+} and \code{-} operators and parentheses, and
#' keeping track of whether a term has a plus or a minus sign.
#' 
#' @param rhs a formula-style call containing the right hand side of formula,
#' obtained by \code{fmla[[3]]} for a two-sided formula and \code{fmla[[2]]}
#' for a one-sided formula.
#' @param sign an internal parameter used by \code{term.list.formula} when
#' calling itself recursively.
#' @return
#' \code{terms.list.formula} returns a list of formula terms, with an additional numerical vector attribute \code{"sign"} with of the same length, giving the corresponding term's sign as \code{+1} or \code{-1}.
#' @export
term.list.formula<-function(rhs, sign=+1){
  if(length(rhs)==1) {out <- list(rhs); attr(out,"sign")<-sign; out}
  else if(length(rhs)==2 && rhs[[1]]=="+") term.list.formula(rhs[[2]],sign)
  else if(length(rhs)==2 && rhs[[1]]=="-") term.list.formula(rhs[[2]],-sign)
  else if(length(rhs)==3 && rhs[[1]]=="+") {
    l1 <- term.list.formula(rhs[[2]],sign)
    l2 <- term.list.formula(rhs[[3]],sign)
    out <- c(l1, l2)
    attr(out,"sign") <- c(attr(l1,"sign"), attr(l2,"sign"))
    out
  }
  else if(length(rhs)==3 && rhs[[1]]=="-"){
    l1 <- term.list.formula(rhs[[2]],sign)
    l2 <- term.list.formula(rhs[[3]],-sign)
    out <- c(l1, l2)
    attr(out,"sign") <- c(attr(l1,"sign"), attr(l2,"sign"))
    out    
  }
  else if(rhs[[1]]=="(") term.list.formula(rhs[[2]], sign)
  else {out <- list(rhs); attr(out,"sign")<-sign; out}
}

