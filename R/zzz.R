#  File R/zzz.R in package statnet.common, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2007-2017 Statnet Commons
#######################################################################
.onUnload <- function(libpath){
  library.dynam.unload("statnet.common",libpath)
}
