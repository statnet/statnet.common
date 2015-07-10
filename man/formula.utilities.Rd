\name{formula.utils}
\alias{nonsimp.update.formula}
\alias{term.list.formula}
\alias{append.rhs.formula}
\title{
Functions for Querying, Validating and Extracting from ERGM Formulas
}
\description{
These are all functions that are generally not called directly by users, but may be employed by other depending packages.  
}
\usage{
nonsimp.update.formula(object, new, \dots, from.new = FALSE)

term.list.formula(rhs, sign = +1)

append.rhs.formula(object, newterms, keep.onesided = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{object}{formula object to be updated}
  \item{new}{new formula to be used in updating}
  \item{from.new}{logical or character vector of variable names. controls how environment of formula gets updated.}
  \item{rhs}{a formula-style call containing the right hand side of
  formula, obtained by \code{fmla[[3]]} for a two-sided formula and
  \code{fmla[[2]]} for a one-sided formula.}
  \item{sign}{an internal parameter used by \code{term.list.formula}
    when calling itself recursively}
  \item{newterms}{list of terms (names) to append to the formula }
  \item{keep.onesided}{if the initial formula is one-sided, keep it
    whether to keep it one-sided or whether to make the initial formula
    the new LHS}
  \item{\dots}{Additional arguments. Currently unused.}
}
\details{
\itemize{
 
 \item{\code{nonsimp.update.formula} is a reimplementation of \code{\link{update.formula}} that does not simplify.  Note that the resulting formula's environment is set as follows. If
 \code{from.new==FALSE}, it is set to that of object. Otherwise, a new
 sub-environment of object, containing, in addition, variables in new
 listed in from.new (if a character vector) or all of new (if TRUE).}
 
 \item{\code{term.list.formula} returns a list containing terms in a given
   formula, handling \code{+} and \code{-} operators and parentheses,
   and keeping track of whether a term has a plus or a minus sign. }
 
 \item{\code{append.rhs.formula}  appends a list of terms to the RHS of a formula. If the formula is one-sided, the RHS becomes the LHS, if \code{keep.onesided==FALSE} (the default).}
}
}
\value{
     \code{terms.list.formula} returns a list of formula terms, each of
     witch having an additional attribute \code{"sign"}.
     
  \code{nonsimp.update.formula} and 
\code{append.rhs.formula} each return an updated formula object
}

\examples{
## append.rhs.formula

append.rhs.formula(y~x,list(as.name("z1"),as.name("z2"))) # y~x+z1+z2
append.rhs.formula(~y,list(as.name("z"))) # y~z
append.rhs.formula(~y+x,list(as.name("z"))) # y+x~z
append.rhs.formula(~y,list(as.name("z")),TRUE) # ~y+z
}