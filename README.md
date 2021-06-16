# `statnet.common`: Common R Scripts and Utilities Used by the Statnet Project Software

[![rstudio mirror downloads](https://cranlogs.r-pkg.org/badges/statnet.common?color=2ED968)](https://cranlogs.r-pkg.org/)
[![cran version](https://www.r-pkg.org/badges/version/statnet.common)](https://cran.r-project.org/package=statnet.common)
[![Coverage status](https://codecov.io/gh/statnet/statnet.common/branch/master/graph/badge.svg)](https://codecov.io/github/statnet/statnet.common?branch=master)
[![R build status](https://github.com/statnet/statnet.common/workflows/R-CMD-check/badge.svg)](https://github.com/statnet/statnet.common/actions)

Non-statistical utilities used by the software developed by the Statnet Project. They may also be of use to others.

## Public and Private repositories

To facilitate open development of the package while giving the core developers an opportunity to publish on their developments before opening them up for general use, this project comprises two repositories:
* A public repository `statnet/statnet.common`
* A private repository `statnet/statnet.common-private`

The intention is that all developments in `statnet/statnet.common-private` will eventually make their way into `statnet/statnet.common` and onto CRAN.

Developers and Contributing Users to the Statnet Project should read https://statnet.github.io/private/ for information about the relationship between the public and the private repository and the workflows involved.

## Latest Windows and MacOS binaries

A set of binaries is built after every commit to the repository. We strongly encourage testing against them before filing a bug report, as they may contain fixes that have not yet been sent to CRAN. They can be downloaded through the following links:

* [MacOS binary (a `.tgz` file in a `.zip` file)](https://nightly.link/statnet/statnet.common/workflows/R-CMD-check.yaml/master/macOS-rrelease-binaries.zip)
* [Windows binary (a `.zip` file in a `.zip` file)](https://nightly.link/statnet/statnet.common/workflows/R-CMD-check.yaml/master/Windows-rrelease-binaries.zip)

You will need to extract the MacOS `.tgz` or the Windows `.zip` file from the outer `.zip` file before installing. These binaries are usually built under the latest version of R and their operating system and may not work under other versions.
