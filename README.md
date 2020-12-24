# `statnet.common`:  Common R Scripts and Utilities Used by the Statnet Project Software

[![Build Status](https://travis-ci.org/statnet/statnet.common.svg?branch=master)](https://travis-ci.org/statnet/statnet.common)
[![Build Status](https://ci.appveyor.com/api/projects/status/28p03h7f78rp95if?svg=true)](https://ci.appveyor.com/project/statnet/statnet-common)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/statnet.common?color=2ED968)](http://cranlogs.r-pkg.org/)
[![cran version](http://www.r-pkg.org/badges/version/statnet.common)](https://cran.r-project.org/package=statnet.common)
[![Coverage status](https://codecov.io/gh/statnet/statnet.common/branch/master/graph/badge.svg)](https://codecov.io/github/statnet/statnet.common?branch=master)
[![R build status](https://github.com/statnet/statnet.common/workflows/R-CMD-check/badge.svg)](https://github.com/statnet/statnet.common/actions)

Non-statistical utilities used by the software developed by the Statnet Project. They may also be of use to others.

## Latest Windows and MacOS binaries

A set of binaries is built after every commit to the public repository. We strongly encourage testing against them before filing a bug report, as they may contain fixes that have not yet been sent to CRAN. They can be downloaded through the following links:

* [MacOS binary (a `.tgz` file in a `.zip` file)](https://nightly.link/statnet/statnet.common/workflows/R-CMD-check.yaml/master/macOS-rrelease-binaries.zip)
* [Windows binary (a `.zip` file in a `.zip` file)](https://nightly.link/statnet/statnet.common/workflows/R-CMD-check.yaml/master/Windows-rrelease-binaries.zip)

You will need to extract the MacOS `.tgz` or the Windows `.zip` file from the outer `.zip` file before installing. These binaries are usually built under the latest version of R and their operating system and may not work under other versions.

You may also want to install the corresponding latest binaries for packages on which `statnet.common` depends, in particular [`rle`](https://github.com/statnet/rle).
