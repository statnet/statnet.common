#  File R/string.utilities.R in package statnet.common, part of the Statnet suite
#  of packages for network analysis, https://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  https://statnet.org/attribution
#
#  Copyright 2007-2019 Statnet Commons
#######################################################################
## Concatenate a character list with commas and ands in the right places.


#' Concatenates the elements of a vector (optionaly enclosing them in quotation
#' marks or parentheses) adding appropriate punctuation and conjunctions.
#' 
#' A vector \code{x} becomes "\code{x[1]}", "\code{x[1]} and \code{x[2]}", or
#' "\code{x[1]}, \code{x[2]}, and \code{x[3]}", depending on the langth of
#' \code{x}.
#' 
#' 
#' @param x A vector.
#' @param oq Opening quotation symbol. (Defaults to none.)
#' @param cq Closing quotation symbol. (Defaults to none.)
#' @param con Conjunction to be used if `length(x)>1`. (Defaults to "and".)
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
#' print(paste.and(1:4,con='or'))
#' @export
paste.and <- function(x, oq='', cq='', con='and'){
  x <- paste(oq, x, cq, sep='')
  if(length(x)==0) return('')
  if(length(x)==1) return(x)
  if(length(x)==2) return(paste(x[1L],con,x[2L]))
  if(length(x)>=3) return(paste0(paste(x[-length(x)], collapse=", "),', ',con,' ',ult(x)))
}


#' [`print`] objects to the [`message`] output.
#'
#' A thin wrapper around [`print`] that captures its output and prints
#' it as a [`message`], usually to STDERR.
#'
#' @param ... arguments to [`print`].
#' @param messageArgs a list of arguments to be passed directly to [`message`].
#'
#' @examples
#' cat(1:5)
#' 
#' print(1:5)
#' message_print(1:5) # Looks the same (though may be in a different color on some frontends).
#' 
#' suppressMessages(print(1:5)) # Still prints
#' suppressMessages(message_print(1:5)) # Silenced
#' @export
message_print <- function(..., messageArgs=NULL){
  #' @importFrom utils capture.output
  do.call(message, c(list(paste(capture.output(print(...)),collapse="\n")), messageArgs))
}

#' A one-line function to strip whitespace from its argument.
#' @param s a character vector.
#' @examples
#' stopifnot(despace("\n \t  ")=="")
#' @export
despace <- function(s) gsub("[[:space:]]", "", s)
