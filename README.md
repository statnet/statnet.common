# `statnet.common`: Common R Scripts and Utilities Used by the Statnet Project Software

[![rstudio mirror downloads](https://cranlogs.r-pkg.org/badges/statnet.common?color=2ED968)](https://cranlogs.r-pkg.org/)
[![cran version](https://www.r-pkg.org/badges/version/statnet.common)](https://cran.r-project.org/package=statnet.common)
[![Coverage status](https://codecov.io/gh/statnet/statnet.common/branch/master/graph/badge.svg)](https://codecov.io/github/statnet/statnet.common?branch=master)
[![R build status](https://github.com/statnet/statnet.common/workflows/R-CMD-check/badge.svg)](https://github.com/statnet/statnet.common/actions)
[![R-universe](https://statnet.r-universe.dev/statnet.common/badges/version)](https://statnet.r-universe.dev/statnet.common)

Non-statistical utilities used by the software developed by the Statnet Project. They may also be of use to others.

## Public and Private repositories

To facilitate open development of the package while giving the core developers an opportunity to publish on their developments before opening them up for general use, this project comprises two repositories:
* A public repository `statnet/statnet.common`
* A private repository `statnet/statnet.common-private`

The intention is that all developments in `statnet/statnet.common-private` will eventually make their way into `statnet/statnet.common` and onto CRAN.

Developers and Contributing Users to the Statnet Project should read https://statnet.org/private/ for information about the relationship between the public and the private repository and the workflows involved.

## Latest Windows and MacOS binaries

[R-Universe](https://r-universe.dev) builds a set of binaries after every commit to the main branch of the repository. We strongly encourage testing against them before filing a bug report, as they may contain fixes that have not yet been sent to CRAN. To obtain the binaries from r-universe, navigate to the package page at https://statnet.r-universe.dev/statnet.common .