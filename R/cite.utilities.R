#  File R/cite.utilities.R in package statnet.common, part of the Statnet suite
#  of packages for network analysis, https://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free, open
#  source, and has the attribution requirements (GPL Section 7) at
#  https://statnet.org/attribution .
#
#  Copyright 2007-2025 Statnet Commons
################################################################################
# ---- BEGIN STATNET CITATION FUNCTIONS ----

#' \code{CITATION} file utilities for Statnet packages (DEPRECATED)
#' 
#' These functions automate citation generation for Statnet Project
#' packages. They no longer appear to work with CRAN and are thus
#' deprecated.
#' 
#' 
#' @param pkg Name of the package whose citation is being generated.
#' @return For \code{statnet.cite.head} and \code{statnet.cite.foot}, an object
#' of type \code{citationHeader} and \code{citationFooter}, respectively,
#' understood by the \code{\link{citation}} function, with package name
#' substituted into the template.
#' 
#' For \code{statnet.cite.pkg}, an object of class \code{\link{bibentry}}
#' containing a 'software manual' citation for the package constructed from the
#' current version and author information in the \code{DESCRIPTION} and a
#' template.
#' @seealso citation, citHeader, citFooter, bibentry
#' @keywords utilities
#' @name statnet.cite
#' @examples
#'
#' \dontrun{
#' statnet.cite.head("statnet.common")
#' 
#' statnet.cite.pkg("statnet.common")
#' 
#' statnet.cite.foot("statnet.common")
#' }
NULL

# A header function for ensuring that all the statnet packages provide consistent messaging
#' @rdname statnet.cite
#' @export
statnet.cite.head <- function(pkg){
  .Deprecated("No longer usable.")
  utils::citHeader(
    paste("`",pkg,"` is part of the Statnet suite of packages.  ",
          "If you are using the `",pkg,"` package for research that will be published, ",
          "we request that you acknowledge this by citing the following.\n",
          'For BibTeX format, use toBibtex(citation("',pkg,'")).',
          sep="")
    )
}

# A footer function for ensuring that all the statnet packages provide consistent messaging
#' @rdname statnet.cite
#' @export
statnet.cite.foot <- function(pkg){
  .Deprecated("No longer usable.")
  # the 'meta' variable should be provided by R's CITATION processing script
  # instead of using packageDescription().  But if this code is called in another context
  # use packageDescription() to assign meta
  if(!exists("meta") || is.null(meta)){
    meta <- utils::packageDescription(pkg) 
  }
  utils::citFooter("We have invested a lot of time and effort in creating the",
            "Statnet suite of packages for use by other researchers.",
            "Please cite it in all papers where it is used. The package",pkg," is made distributed under the terms of the license:",meta$License )
}

# generates a consistent bibentry citation for the software manual of the package
#' @rdname statnet.cite
#' @export
statnet.cite.pkg <- function(pkg){
  .Deprecated("No longer usable.")  
  # the 'meta' variable should be provided by R's CITATION processing script
  # instead of using packageDescription().  But if this code is called in another context
  # use packageDescription() to assign meta
  if(!exists("meta") || is.null(meta)){
      meta <- utils::packageDescription(pkg) 
  }

  projhomepage <- "http://www.statnet.org"
  # compute the list of authors  
  auts <- eval(parse(text=meta$`Authors@R`))
  auts <- auts[sapply(auts, function(aut) "aut" %in% aut$role)]
  # create a citation entry for a "software manual" for this version of the software
  # it will be appended with any specific articles defineded inthe package citation file
  utils::bibentry("Manual",
           author = auts,
           title = paste(meta$Package,": ", meta$Title, sep=""),
           organization = paste("The Statnet Project (\\url{", projhomepage, "})",sep=""),
           year         = substr(meta$Date,1,4),
           note         = paste("R package version ", meta$Version, sep=""),
           url          = paste("CRAN.R-project.org/package=",meta$Package,sep="")
           )
}
# ---- END STATNET CITATION FUNCTIONS ----
