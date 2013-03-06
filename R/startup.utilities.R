statnetStartupMessage <- function(pkgname, friends, nofriends){
  INST_MAP <- list(washington.edu="University of Washington",
                   uw.edu="University of Washington",
                   psu.edu="Penn State University",
                   uci.edu="University of California -- Irvine",
                   ucla.edu="University of California -- Los Angeles",
                   nyu.edu="New York University",
                   murdoch.edu.au="Murdoch University"
                   ) 

  # Note that all options are ignored at this time, and the "wall of
  # text" is displayed unconditionally.
  
  desc <- packageDescription(pkgname)
  pns <- eval(parse(text=desc$`Authors@R`))
  # The gsub is necessary because R CMD build can put line breaks in all sorts of fun places.
  pnnames <- gsub("[\n ]+", " ", format(pns, include=c("given","family")))

  # Find the institution associated with the domain of the specific e-mail message.
  find.inst <- function(email, map){
    if(is.null(email)) return(NULL)
    insts <- which(sapply(names(map),
                          function(inst){
                            instre <- paste('[@.]',gsub('.','\\.',inst,fixed=TRUE),sep='')
                            grepl(instre, email)
                          }
                          ))
    if(length(insts)) map[[insts]]
    else NULL
  }
  
  pninsts <- sapply(pns, function(pn) NVL(find.inst(pn$email, INST_MAP),""))

  authors <- sapply(pns, function(pn) "aut" %in% pn$role)

  pnlines <- ifelse(pninsts=="", pnnames, paste(pnnames,pninsts, sep=", "))
  
  copylist <- paste("Copyright (c) ",substr(desc$Date,1,4),", ",sep="")
  copylist <- paste(copylist, pnlines[authors][1],"\n",
                    paste(
                      paste(rep(" ",nchar(copylist)),collapse=""),
                      c(pnlines[authors][-1],if(sum(!authors)) "with contributions from",pnlines[!authors]),sep="",collapse="\n"),
                    sep="") 
     paste("\n",desc$Package,": version ", desc$Version, ', created on ', desc$Date, '\n',copylist,"\n",
          'Based on "statnet" project software (statnet.org).\n',
          'For license and citation information see statnet.org/attribution\n',
          'or type citation("',desc$Package,'").\n', sep="")
}
