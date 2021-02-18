# This document provides a template for exporting a package's
# control.* functions to be visible to sctrl() and providing a
# sensible help document. Currently, the packages have to be updated
# manually when the template changes.


## Usage:
# 1. Add the following line:
#    "eval(statnet.common::COLLATE_ALL_MY_CONTROLS_EXPR)" to the
#    .onLoad() function. It is important that the function's arguments
#    have their standard names ("libname" and "pkgname").
#
# 2. Add the following text block to provide help. Note that the NULL is important, because otherwise, Roxygen will create an unsightly usage statement.
#
# 3. Run roxygen.


# TODO: Figure out some automatic way to keep this in sync with statnet.common.

## BEGIN text block

#' @name sctrl
#'
#' @title Statnet Control
#'
#' @description A utility `sctrl(...)`, to facilitate argument completion of control lists, reexported from `statnet.common`.
#'
#' @section Currently recognised control parameters:
#' This list is updated as packages are loaded and unloaded.
#'
#' \Sexpr[results=rd,stage=render]{statnet.common::sctrl_names()}
#'
#' @seealso [statnet.common::sctrl()]
#' @docType import
NULL
#' @export
sctrl <- statnet.common::sctrl

eval(UPDATE_MY_SCTRL_EXPR)

# END text block
