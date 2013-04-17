#  File R/string.utilities.R in package statnet.common, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2003-2013 Statnet Commons
#######################################################################
## Concatenate a character list with commas and ands in the right places.
paste.and <- function(x, oq='', cq=''){
  x <- paste(oq, x, cq, sep='')
  if(length(x)==0) return('')
  if(length(x)==1) return(x)
  if(length(x)==2) return(paste(x[1],'and',x[2]))
  if(length(x)>=3) return(paste(paste(x[-length(x)], collapse=", "),', and ',x[length(x)],sep=''))
}

