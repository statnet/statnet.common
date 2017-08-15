#  File R/string.utilities.R in package statnet.common, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2007-2017 Statnet Commons
#######################################################################
## Concatenate a character list with commas and ands in the right places.


#' Concatenates the elements of a vector (optionaly enclosing them in quotation
#' marks or parentheses) adding appropriate punctuation and unions.
#' 
#' A vector \code{x} becomes "\code{x[1]}", "\code{x[1]} and \code{x[2]}", or
#' "\code{x[1]}, \code{x[2]}, and \code{x[3]}", depending on the langth of
#' \code{x}.
#' 
#' 
#' @param x A vector.
#' @param oq Opening quotation symbol. (Defaults to none.)
#' @param cq Closing quotation symbol. (Defaults to none.)
#' @return A string with the output.
#' @seealso paste, cat
#' @keywords utilities
#' @examples
#' 
#' print(paste.and(c()))
#' 
#' print(paste.and(1))
#' 
#' print(paste.and(1:2))
#' 
#' print(paste.and(1:3))
#' 
#' print(paste.and(1:4))
#' @export
paste.and <- function(x, oq='', cq=''){
  x <- paste(oq, x, cq, sep='')
  if(length(x)==0) return('')
  if(length(x)==1) return(x)
  if(length(x)==2) return(paste(x[1],'and',x[2]))
  if(length(x)>=3) return(paste(paste(x[-length(x)], collapse=", "),', and ',x[length(x)],sep=''))
}

