#  File R/cite.utilities.R in package statnet.common, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2003-2013 Statnet Commons
#######################################################################
statnet.cite.head <- function(pkg)
  citHeader(
    paste("`",pkg,"` is part of the Statnet suite of packages.  ",
          "If you are using the `",pkg,"` package for research that will be published, ",
          "we request that you acknowledge this by citing the following.\n\n",
          'For BibTeX format, use toBibtex(citation("',pkg,'")).',
          sep="")
    )

statnet.cite.foot <- function(pkg)
  citFooter("We have invested a lot of time and effort in creating the",
            "Statnet suite of packages for use by other researchers.",
            "Please cite it in all papers where it is used.")

statnet.cite.pkg <- function(pkg){
  
  desc <- packageDescription(pkg)

  projhomepage <- "http://www.statnet.org"
    
  auts <- eval(parse(text=desc$`Authors@R`))
  auts <- auts[sapply(auts, function(aut) "aut" %in% aut$role)]
  
  bibentry("Manual",
           author = auts,
           title = paste(desc$Package,": ", desc$Title, sep=""),
           organization = paste("The Statnet Project (\\url{", projhomepage, "})",sep=""),
           year         = substr(desc$Date,1,4),
           note         = paste("R package version ", desc$Version, sep=""),
#           address      = "Seattle, WA",
           url          = paste("CRAN.R-project.org/package=",desc$Package,sep="")
           )
}
