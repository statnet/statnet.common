# amen

<details>

* Version: 1.4.5
* GitHub: https://github.com/pdhoff/amen
* Source code: https://github.com/cran/amen
* Date/Publication: 2024-02-20 23:20:07 UTC
* Number of recursive dependencies: 37

Run `revdepcheck::revdep_details(, "amen")` for more info

</details>

## In both

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘binary_demo.Rmd’ using rmarkdown
    
    Quitting from lines 37-38 [unnamed-chunk-4] (binary_demo.Rmd)
    Error: processing vignette 'binary_demo.Rmd' failed with diagnostics:
    there is no package called 'codetools'
    --- failed re-building ‘binary_demo.Rmd’
    
    --- re-building ‘diy_Poisson_demo.Rmd’ using rmarkdown
    
    ...
    Quitting from lines 167-201 [unnamed-chunk-7] (diy_binary_demo.Rmd)
    Error: processing vignette 'diy_binary_demo.Rmd' failed with diagnostics:
    there is no package called 'codetools'
    --- failed re-building ‘diy_binary_demo.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘binary_demo.Rmd’ ‘diy_Poisson_demo.Rmd’ ‘diy_binary_demo.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# BANAM

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/BANAM
* Date/Publication: 2024-06-20 10:30:02 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::revdep_details(, "BANAM")` for more info

</details>

## In both

*   checking whether package ‘BANAM’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/home/pavel/Documents/Research/Software/statnet/statnet.common/revdep/checks/BANAM/new/BANAM.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘BANAM’ ...
** package ‘BANAM’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘BFpack’ in dyn.load(file, DLLpath = DLLpath, ...):
 unable to load shared object '/home/pavel/Documents/Research/Software/statnet/statnet.common/revdep/library/BANAM/gsl/libs/gsl.so':
  libgsl.so.27: cannot open shared object file: No such file or directory
Execution halted
ERROR: lazy loading failed for package ‘BANAM’
* removing ‘/home/pavel/Documents/Research/Software/statnet/statnet.common/revdep/checks/BANAM/new/BANAM.Rcheck/BANAM’


```
### CRAN

```
* installing *source* package ‘BANAM’ ...
** package ‘BANAM’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘BFpack’ in dyn.load(file, DLLpath = DLLpath, ...):
 unable to load shared object '/home/pavel/Documents/Research/Software/statnet/statnet.common/revdep/library/BANAM/gsl/libs/gsl.so':
  libgsl.so.27: cannot open shared object file: No such file or directory
Execution halted
ERROR: lazy loading failed for package ‘BANAM’
* removing ‘/home/pavel/Documents/Research/Software/statnet/statnet.common/revdep/checks/BANAM/old/BANAM.Rcheck/BANAM’


```
# BasketballAnalyzeR

<details>

* Version: 0.5.0
* GitHub: https://github.com/sndmrc/BasketballAnalyzeR
* Source code: https://github.com/cran/BasketballAnalyzeR
* Date/Publication: 2020-06-26 09:00:11 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::revdep_details(, "BasketballAnalyzeR")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘circlize’ ‘hexbin’ ‘scales’ ‘sna’
      All declared Imports should be used.
    ```

# BFpack

<details>

* Version: 1.3.0
* GitHub: https://github.com/jomulder/BFpack
* Source code: https://github.com/cran/BFpack
* Date/Publication: 2024-06-19 10:50:02 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::revdep_details(, "BFpack")` for more info

</details>

## In both

*   checking whether package ‘BFpack’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/home/pavel/Documents/Research/Software/statnet/statnet.common/revdep/checks/BFpack/new/BFpack.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘BFpack’ ...
** package ‘BFpack’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Debian 14.2.0-3) 14.2.0’
using Fortran compiler: ‘GNU Fortran (Debian 14.2.0-3) 14.2.0’
gcc -I"/usr/share/R/include" -DNDEBUG       -fpic  -g -O2 -ffile-prefix-map=/build/reproducible-path/r-base-4.4.1=. -fstack-protector-strong -fstack-clash-protection -Wformat -Werror=format-security -fcf-protection -Wdate-time -D_FORTIFY_SOURCE=2  -c BFpack_init.c -o BFpack_init.o
gfortran  -fpic  -g -O2 -ffile-prefix-map=/build/reproducible-path/r-base-4.4.1=. -fstack-protector-strong -fstack-clash-protection -fcf-protection  -c  bct_mixedordinal.f90 -o bct_mixedordinal.o
gfortran  -fpic  -g -O2 -ffile-prefix-map=/build/reproducible-path/r-base-4.4.1=. -fstack-protector-strong -fstack-clash-protection -fcf-protection  -c  bct_prior.f90 -o bct_prior.o
gcc -shared -L/usr/lib/R/lib -Wl,-z,relro -o BFpack.so BFpack_init.o bct_mixedordinal.o bct_prior.o -llapack -lblas -lgfortran -lm -lquadmath -L/usr/lib/R/lib -lR
...
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/home/pavel/Documents/Research/Software/statnet/statnet.common/revdep/library/BFpack/gsl/libs/gsl.so':
  libgsl.so.27: cannot open shared object file: No such file or directory
Calls: <Anonymous> ... asNamespace -> loadNamespace -> library.dynam -> dyn.load
Execution halted
ERROR: lazy loading failed for package ‘BFpack’
* removing ‘/home/pavel/Documents/Research/Software/statnet/statnet.common/revdep/checks/BFpack/new/BFpack.Rcheck/BFpack’


```
### CRAN

```
* installing *source* package ‘BFpack’ ...
** package ‘BFpack’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Debian 14.2.0-3) 14.2.0’
using Fortran compiler: ‘GNU Fortran (Debian 14.2.0-3) 14.2.0’
gcc -I"/usr/share/R/include" -DNDEBUG       -fpic  -g -O2 -ffile-prefix-map=/build/reproducible-path/r-base-4.4.1=. -fstack-protector-strong -fstack-clash-protection -Wformat -Werror=format-security -fcf-protection -Wdate-time -D_FORTIFY_SOURCE=2  -c BFpack_init.c -o BFpack_init.o
gfortran  -fpic  -g -O2 -ffile-prefix-map=/build/reproducible-path/r-base-4.4.1=. -fstack-protector-strong -fstack-clash-protection -fcf-protection  -c  bct_mixedordinal.f90 -o bct_mixedordinal.o
gfortran  -fpic  -g -O2 -ffile-prefix-map=/build/reproducible-path/r-base-4.4.1=. -fstack-protector-strong -fstack-clash-protection -fcf-protection  -c  bct_prior.f90 -o bct_prior.o
gcc -shared -L/usr/lib/R/lib -Wl,-z,relro -o BFpack.so BFpack_init.o bct_mixedordinal.o bct_prior.o -llapack -lblas -lgfortran -lm -lquadmath -L/usr/lib/R/lib -lR
...
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/home/pavel/Documents/Research/Software/statnet/statnet.common/revdep/library/BFpack/gsl/libs/gsl.so':
  libgsl.so.27: cannot open shared object file: No such file or directory
Calls: <Anonymous> ... asNamespace -> loadNamespace -> library.dynam -> dyn.load
Execution halted
ERROR: lazy loading failed for package ‘BFpack’
* removing ‘/home/pavel/Documents/Research/Software/statnet/statnet.common/revdep/checks/BFpack/old/BFpack.Rcheck/BFpack’


```
# BGGM

<details>

* Version: 2.1.3
* GitHub: https://github.com/donaldRwilliams/BGGM
* Source code: https://github.com/cran/BGGM
* Date/Publication: 2024-07-05 20:30:02 UTC
* Number of recursive dependencies: 206

Run `revdepcheck::revdep_details(, "BGGM")` for more info

</details>

## In both

*   checking whether package ‘BGGM’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/home/pavel/Documents/Research/Software/statnet/statnet.common/revdep/checks/BGGM/new/BGGM.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘BGGM’ ...
** package ‘BGGM’ successfully unpacked and MD5 sums checked
** using staged installation
checking whether the C++ compiler works... yes
checking for C++ compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C++ compiler... yes
checking whether g++ -std=gnu++17 accepts -g... yes
...
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/home/pavel/Documents/Research/Software/statnet/statnet.common/revdep/library/BGGM/gsl/libs/gsl.so':
  libgsl.so.27: cannot open shared object file: No such file or directory
Calls: <Anonymous> ... asNamespace -> loadNamespace -> library.dynam -> dyn.load
Execution halted
ERROR: lazy loading failed for package ‘BGGM’
* removing ‘/home/pavel/Documents/Research/Software/statnet/statnet.common/revdep/checks/BGGM/new/BGGM.Rcheck/BGGM’


```
### CRAN

```
* installing *source* package ‘BGGM’ ...
** package ‘BGGM’ successfully unpacked and MD5 sums checked
** using staged installation
checking whether the C++ compiler works... yes
checking for C++ compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C++ compiler... yes
checking whether g++ -std=gnu++17 accepts -g... yes
...
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/home/pavel/Documents/Research/Software/statnet/statnet.common/revdep/library/BGGM/gsl/libs/gsl.so':
  libgsl.so.27: cannot open shared object file: No such file or directory
Calls: <Anonymous> ... asNamespace -> loadNamespace -> library.dynam -> dyn.load
Execution halted
ERROR: lazy loading failed for package ‘BGGM’
* removing ‘/home/pavel/Documents/Research/Software/statnet/statnet.common/revdep/checks/BGGM/old/BGGM.Rcheck/BGGM’


```
# bigergm

<details>

* Version: 1.2.1
* GitHub: NA
* Source code: https://github.com/cran/bigergm
* Date/Publication: 2024-06-13 21:50:13 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::revdep_details(, "bigergm")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.1Mb
      sub-directories of 1Mb or more:
        data   1.3Mb
        libs   8.9Mb
    ```

# bingat

<details>

* Version: 1.3
* GitHub: NA
* Source code: https://github.com/cran/bingat
* Date/Publication: 2017-07-05 18:30:37 UTC
* Number of recursive dependencies: 32

Run `revdepcheck::revdep_details(, "bingat")` for more info

</details>

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘genalg’
    ```

# bootcluster

<details>

* Version: 0.3.2
* GitHub: NA
* Source code: https://github.com/cran/bootcluster
* Date/Publication: 2022-01-29 22:50:03 UTC
* Number of recursive dependencies: 70

Run `revdepcheck::revdep_details(, "bootcluster")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘sna’
      All declared Imports should be used.
    ```

# btergm

<details>

* Version: 1.10.12
* GitHub: https://github.com/leifeld/btergm
* Source code: https://github.com/cran/btergm
* Date/Publication: 2024-03-31 22:30:06 UTC
* Number of recursive dependencies: 100

Run `revdepcheck::revdep_details(, "btergm")` for more info

</details>

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 89 marked UTF-8 strings
    ```

# CINNA

<details>

* Version: 1.2.2
* GitHub: NA
* Source code: https://github.com/cran/CINNA
* Date/Publication: 2023-08-08 16:40:02 UTC
* Number of recursive dependencies: 131

Run `revdepcheck::revdep_details(, "CINNA")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘circlize’ ‘utils’
      All declared Imports should be used.
    ```

# conserveR

<details>

* Version: 1.0.4
* GitHub: https://github.com/azizka/conserveR
* Source code: https://github.com/cran/conserveR
* Date/Publication: 2021-08-02 09:10:06 UTC
* Number of recursive dependencies: 52

Run `revdepcheck::revdep_details(, "conserveR")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘scales’ ‘sna’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 244 marked UTF-8 strings
    ```

# dnr

<details>

* Version: 0.3.5
* GitHub: NA
* Source code: https://github.com/cran/dnr
* Date/Publication: 2020-11-30 17:10:03 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::revdep_details(, "dnr")` for more info

</details>

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) dnr.Rd:12: Escaped LaTeX specials: \#
    ```

# ecoCopula

<details>

* Version: 1.0.2
* GitHub: NA
* Source code: https://github.com/cran/ecoCopula
* Date/Publication: 2022-03-02 00:20:02 UTC
* Number of recursive dependencies: 114

Run `revdepcheck::revdep_details(, "ecoCopula")` for more info

</details>

## In both

*   checking whether package ‘ecoCopula’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/home/pavel/Documents/Research/Software/statnet/statnet.common/revdep/checks/ecoCopula/new/ecoCopula.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ecoCopula’ ...
** package ‘ecoCopula’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘mvabund’ in dyn.load(file, DLLpath = DLLpath, ...):
 unable to load shared object '/home/pavel/Documents/Research/Software/statnet/statnet.common/revdep/library/ecoCopula/mvabund/libs/mvabund.so':
  libgsl.so.27: cannot open shared object file: No such file or directory
Execution halted
ERROR: lazy loading failed for package ‘ecoCopula’
* removing ‘/home/pavel/Documents/Research/Software/statnet/statnet.common/revdep/checks/ecoCopula/new/ecoCopula.Rcheck/ecoCopula’


```
### CRAN

```
* installing *source* package ‘ecoCopula’ ...
** package ‘ecoCopula’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘mvabund’ in dyn.load(file, DLLpath = DLLpath, ...):
 unable to load shared object '/home/pavel/Documents/Research/Software/statnet/statnet.common/revdep/library/ecoCopula/mvabund/libs/mvabund.so':
  libgsl.so.27: cannot open shared object file: No such file or directory
Execution halted
ERROR: lazy loading failed for package ‘ecoCopula’
* removing ‘/home/pavel/Documents/Research/Software/statnet/statnet.common/revdep/checks/ecoCopula/old/ecoCopula.Rcheck/ecoCopula’


```
# econet

<details>

* Version: 1.0.0.1
* GitHub: NA
* Source code: https://github.com/cran/econet
* Date/Publication: 2024-07-31 10:59:28 UTC
* Number of recursive dependencies: 64

Run `revdepcheck::revdep_details(, "econet")` for more info

</details>

## In both

*   checking installed files from ‘inst/doc’ ... NOTE
    ```
    The following files should probably not be installed:
      ‘econet.tex’
    
    Consider the use of a .Rinstignore file: see ‘Writing R Extensions’,
    or move the vignette sources from ‘inst/doc’ to ‘vignettes’.
    ```

# edgebundle

<details>

* Version: 0.4.2
* GitHub: https://github.com/schochastics/edgebundle
* Source code: https://github.com/cran/edgebundle
* Date/Publication: 2023-12-16 06:00:02 UTC
* Number of recursive dependencies: 55

Run `revdepcheck::revdep_details(, "edgebundle")` for more info

</details>

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 22 marked UTF-8 strings
    ```

# EloRating

<details>

* Version: 0.46.18
* GitHub: https://github.com/gobbios/EloRating
* Source code: https://github.com/cran/EloRating
* Date/Publication: 2024-07-15 16:40:02 UTC
* Number of recursive dependencies: 69

Run `revdepcheck::revdep_details(, "EloRating")` for more info

</details>

## In both

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘EloRating_tutorial.Rmd’ using rmarkdown
    
    Quitting from lines 525-568 [differentshapes] (EloRating_tutorial.Rmd)
    Error: processing vignette 'EloRating_tutorial.Rmd' failed with diagnostics:
    there is no package called 'codetools'
    --- failed re-building ‘EloRating_tutorial.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘EloRating_tutorial.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# EpiModel

<details>

* Version: 2.4.0
* GitHub: https://github.com/EpiModel/EpiModel
* Source code: https://github.com/cran/EpiModel
* Date/Publication: 2023-06-20 18:20:05 UTC
* Number of recursive dependencies: 125

Run `revdepcheck::revdep_details(, "EpiModel")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        doc   2.8Mb
    ```

# ergm

<details>

* Version: 4.6.0
* GitHub: https://github.com/statnet/ergm
* Source code: https://github.com/cran/ergm
* Date/Publication: 2023-12-18 09:20:02 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::revdep_details(, "ergm")` for more info

</details>

## In both

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Network-Callback-API.Rmd’ using rmarkdown
    --- finished re-building ‘Network-Callback-API.Rmd’
    
    --- re-building ‘Proposal-Lookup-API.Rmd’ using rmarkdown
    --- finished re-building ‘Proposal-Lookup-API.Rmd’
    
    --- re-building ‘Terms-API.Rmd’ using rmarkdown
    --- finished re-building ‘Terms-API.Rmd’
    
    ...
    --- failed re-building ‘ergm.Rmd’
    
    --- re-building ‘nodal_attributes.Rmd’ using rmarkdown
    --- finished re-building ‘nodal_attributes.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ergm.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.2Mb
      sub-directories of 1Mb or more:
        R      1.0Mb
        doc    1.7Mb
        libs   2.7Mb
    ```

# ergm.multi

<details>

* Version: 0.2.1
* GitHub: https://github.com/statnet/ergm.multi
* Source code: https://github.com/cran/ergm.multi
* Date/Publication: 2024-02-20 23:20:05 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::revdep_details(, "ergm.multi")` for more info

</details>

## In both

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Goeyvaerts_reproduction.Rmd’ using rmarkdown
    
    Quitting from lines 29-34 [unnamed-chunk-3] (Goeyvaerts_reproduction.Rmd)
    Error: processing vignette 'Goeyvaerts_reproduction.Rmd' failed with diagnostics:
    there is no package called 'codetools'
    --- failed re-building ‘Goeyvaerts_reproduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Goeyvaerts_reproduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ergMargins

<details>

* Version: 1.3
* GitHub: NA
* Source code: https://github.com/cran/ergMargins
* Date/Publication: 2024-05-08 21:30:02 UTC
* Number of recursive dependencies: 70

Run `revdepcheck::revdep_details(, "ergMargins")` for more info

</details>

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘margins’
    ```

# ergmito

<details>

* Version: 0.3-1
* GitHub: https://github.com/muriteams/ergmito
* Source code: https://github.com/cran/ergmito
* Date/Publication: 2023-06-14 10:42:05 UTC
* Number of recursive dependencies: 68

Run `revdepcheck::revdep_details(, "ergmito")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.7Mb
      sub-directories of 1Mb or more:
        libs   6.3Mb
    ```

# FinNet

<details>

* Version: 0.1.2
* GitHub: https://github.com/FATelarico/FinNet
* Source code: https://github.com/cran/FinNet
* Date/Publication: 2023-08-10 06:50:20 UTC
* Number of recursive dependencies: 42

Run `revdepcheck::revdep_details(, "FinNet")` for more info

</details>

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) fun-igraph_financial.Rd:34: Lost braces; missing escapes or markup?
        34 | The same result for both \code{igraph} and {igraph_financial} objects
           |                                            ^
    checkRd: (-1) igraph_E_iterators.Rd:31: Lost braces; missing escapes or markup?
        31 | The same result for both \code{igraph} and {igraph_financial} objects
           |                                            ^
    checkRd: (-1) igraph_v_iterators.Rd:29: Lost braces; missing escapes or markup?
        29 | The same result for both \code{igraph} and {igraph_financial} objects
           |                                            ^
    checkRd: (-1) plot_igraph-methods.Rd:22: Lost braces
        22 | Methods to extend code{igraph}'s plotting functions to \code{igraph_financial} objects
           |                       ^
    checkRd: (-1) plot_igraph-methods.Rd:19: Lost braces; missing escapes or markup?
        19 | For both \code{igraph} and {igraph_financial} objects, returns NULL invisibly. It is called to print the graph to any R device. (see method and \href{https://rdrr.io/cran/igraph/man/plot.igraph.html}{igraph::plot.igraph})
           |                            ^
    ```

# fssemR

<details>

* Version: 0.1.8
* GitHub: https://github.com/Ivis4ml/fssemR
* Source code: https://github.com/cran/fssemR
* Date/Publication: 2022-02-11 13:00:02 UTC
* Number of recursive dependencies: 87

Run `revdepcheck::revdep_details(, "fssemR")` for more info

</details>

## In both

*   checking C++ specification ... NOTE
    ```
      Specified C++11: please drop specification unless essential
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 24.1Mb
      sub-directories of 1Mb or more:
        libs  23.4Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘qpdf’
      All declared Imports should be used.
    ```

# ftsspec

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/ftsspec
* Date/Publication: 2015-09-08 13:13:41
* Number of recursive dependencies: 16

Run `revdepcheck::revdep_details(, "ftsspec")` for more info

</details>

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) Spec_compare_localize_freq.Rd:49: Escaped LaTeX specials: \&
    checkRd: (-1) Spec_compare_localize_freq.Rd:52: Escaped LaTeX specials: \&
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# genBaRcode

<details>

* Version: 1.2.7
* GitHub: NA
* Source code: https://github.com/cran/genBaRcode
* Date/Publication: 2023-12-11 13:10:05 UTC
* Number of recursive dependencies: 160

Run `revdepcheck::revdep_details(, "genBaRcode")` for more info

</details>

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available: 'S4Vectors', 'ShortRead'
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# GGMncv

<details>

* Version: 2.1.1
* GitHub: https://github.com/donaldRwilliams/GGMncv
* Source code: https://github.com/cran/GGMncv
* Date/Publication: 2021-12-15 07:40:28 UTC
* Number of recursive dependencies: 174

Run `revdepcheck::revdep_details(, "GGMncv")` for more info

</details>

## In both

*   checking C++ specification ... NOTE
    ```
      Specified C++11: please drop specification unless essential
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Rdpack’ ‘mathjaxr’
      All declared Imports should be used.
    ```

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) boot_eip.Rd:50: Escaped LaTeX specials: \&
    checkRd: (-1) constrained.Rd:93: Escaped LaTeX specials: \&
    ```

# GGMnonreg

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/GGMnonreg
* Date/Publication: 2021-04-08 11:30:06 UTC
* Number of recursive dependencies: 123

Run `revdepcheck::revdep_details(, "GGMnonreg")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Matrix’ ‘Rdpack’
      All declared Imports should be used.
    ```

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) constrained.Rd:37: Escaped LaTeX specials: \&
    ```

# ggnetwork

<details>

* Version: 0.5.13
* GitHub: https://github.com/briatte/ggnetwork
* Source code: https://github.com/cran/ggnetwork
* Date/Publication: 2024-02-14 11:20:02 UTC
* Number of recursive dependencies: 71

Run `revdepcheck::revdep_details(, "ggnetwork")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘sna’ ‘utils’
      All declared Imports should be used.
    ```

# ggraph

<details>

* Version: 2.2.1
* GitHub: https://github.com/thomasp85/ggraph
* Source code: https://github.com/cran/ggraph
* Date/Publication: 2024-03-07 12:40:02 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::revdep_details(, "ggraph")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.3Mb
      sub-directories of 1Mb or more:
        doc    3.9Mb
        libs   2.8Mb
    ```

# GOxploreR

<details>

* Version: 1.2.7
* GitHub: NA
* Source code: https://github.com/cran/GOxploreR
* Date/Publication: 2023-11-03 14:10:02 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::revdep_details(, "GOxploreR")` for more info

</details>

## In both

*   checking whether package ‘GOxploreR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/home/pavel/Documents/Research/Software/statnet/statnet.common/revdep/checks/GOxploreR/new/GOxploreR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘GOxploreR’ ...
** package ‘GOxploreR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package ‘S4Vectors’ required by ‘IRanges’ could not be found
Execution halted
ERROR: lazy loading failed for package ‘GOxploreR’
* removing ‘/home/pavel/Documents/Research/Software/statnet/statnet.common/revdep/checks/GOxploreR/new/GOxploreR.Rcheck/GOxploreR’


```
### CRAN

```
* installing *source* package ‘GOxploreR’ ...
** package ‘GOxploreR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package ‘S4Vectors’ required by ‘IRanges’ could not be found
Execution halted
ERROR: lazy loading failed for package ‘GOxploreR’
* removing ‘/home/pavel/Documents/Research/Software/statnet/statnet.common/revdep/checks/GOxploreR/old/GOxploreR.Rcheck/GOxploreR’


```
# inferCSN

<details>

* Version: 1.0.8
* GitHub: https://github.com/mengxu98/inferCSN
* Source code: https://github.com/cran/inferCSN
* Date/Publication: 2024-08-24 05:30:02 UTC
* Number of recursive dependencies: 197

Run `revdepcheck::revdep_details(, "inferCSN")` for more info

</details>

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘inferCSN-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_network_heatmap
    > ### Title: Plot network heatmap
    > ### Aliases: plot_network_heatmap
    > 
    > ### ** Examples
    > 
    > data("example_matrix")
    ...
    > 
    > p1 <- plot_network_heatmap(
    +   example_ground_truth[, 1:3],
    +   heatmap_title = "Ground truth",
    +   legend_name = "Ground truth"
    + )
    Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
      there is no package called ‘S4Vectors’
    Calls: plot_network_heatmap ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 22.1Mb
      sub-directories of 1Mb or more:
        libs  20.8Mb
    ```

# InflectSSP

<details>

* Version: 1.6
* GitHub: NA
* Source code: https://github.com/cran/InflectSSP
* Date/Publication: 2023-04-19 08:00:03 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::revdep_details(, "InflectSSP")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘svglite’
      All declared Imports should be used.
    ```

# KinMixLite

<details>

* Version: 2.1.0
* GitHub: NA
* Source code: https://github.com/cran/KinMixLite
* Date/Publication: 2023-04-09 12:50:02 UTC
* Number of recursive dependencies: 49

Run `revdepcheck::revdep_details(, "KinMixLite")` for more info

</details>

## In both

*   R CMD check timed out
    

# latentnet

<details>

* Version: 2.11.0
* GitHub: https://github.com/statnet/latentnet
* Source code: https://github.com/cran/latentnet
* Date/Publication: 2024-02-19 19:40:02 UTC
* Number of recursive dependencies: 110

Run `revdepcheck::revdep_details(, "latentnet")` for more info

</details>

## In both

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# leiden

<details>

* Version: 0.4.3.1
* GitHub: https://github.com/TomKellyGenetics/leiden
* Source code: https://github.com/cran/leiden
* Date/Publication: 2023-11-17 10:20:23 UTC
* Number of recursive dependencies: 138

Run `revdepcheck::revdep_details(, "leiden")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        doc   5.3Mb
    ```

# lolog

<details>

* Version: 1.3.1
* GitHub: https://github.com/statnet/lolog
* Source code: https://github.com/cran/lolog
* Date/Publication: 2023-12-07 12:40:02 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::revdep_details(, "lolog")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking installed package size ... NOTE
    ```
      installed size is 29.0Mb
      sub-directories of 1Mb or more:
        libs  27.3Mb
    ```

*   checking whether startup messages can be suppressed ... NOTE
    ```
    code for methods in class “Rcpp_DirectedLatentOrderLikelihood” was not checked for suspicious field assignments (recommended package ‘codetools’ not available?)
    code for methods in class “Rcpp_DirectedLatentOrderLikelihood” was not checked for suspicious field assignments (recommended package ‘codetools’ not available?)
    code for methods in class “Rcpp_DirectedModel” was not checked for suspicious field assignments (recommended package ‘codetools’ not available?)
    code for methods in class “Rcpp_DirectedModel” was not checked for suspicious field assignments (recommended package ‘codetools’ not available?)
    code for methods in class “Rcpp_DirectedNet” was not checked for suspicious field assignments (recommended package ‘codetools’ not available?)
    code for methods in class “Rcpp_DirectedNet” was not checked for suspicious field assignments (recommended package ‘codetools’ not available?)
    code for methods in class “Rcpp_UndirectedLatentOrderLikelihood” was not checked for suspicious field assignments (recommended package ‘codetools’ not available?)
    code for methods in class “Rcpp_UndirectedLatentOrderLikelihood” was not checked for suspicious field assignments (recommended package ‘codetools’ not available?)
    code for methods in class “Rcpp_UndirectedModel” was not checked for suspicious field assignments (recommended package ‘codetools’ not available?)
    code for methods in class “Rcpp_UndirectedModel” was not checked for suspicious field assignments (recommended package ‘codetools’ not available?)
    code for methods in class “Rcpp_UndirectedNet” was not checked for suspicious field assignments (recommended package ‘codetools’ not available?)
    code for methods in class “Rcpp_UndirectedNet” was not checked for suspicious field assignments (recommended package ‘codetools’ not available?)
    
    It looks like this package (or a package it requires) has a startup
    message which cannot be suppressed: see ?packageStartupMessage.
    ```

# manynet

<details>

* Version: 1.1.0
* GitHub: https://github.com/stocnet/manynet
* Source code: https://github.com/cran/manynet
* Date/Publication: 2024-09-12 22:10:10 UTC
* Number of recursive dependencies: 147

Run `revdepcheck::revdep_details(, "manynet")` for more info

</details>

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘Rgraphviz’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 7 marked UTF-8 strings
    ```

# MBCbook

<details>

* Version: 0.1.2
* GitHub: https://github.com/cbouveyron/MBCbook
* Source code: https://github.com/cran/MBCbook
* Date/Publication: 2024-05-08 11:00:06 UTC
* Number of recursive dependencies: 22

Run `revdepcheck::revdep_details(, "MBCbook")` for more info

</details>

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 66 marked UTF-8 strings
    ```

# MRPC

<details>

* Version: 3.1.0
* GitHub: NA
* Source code: https://github.com/cran/MRPC
* Date/Publication: 2022-04-11 14:32:34 UTC
* Number of recursive dependencies: 170

Run `revdepcheck::revdep_details(, "MRPC")` for more info

</details>

## In both

*   checking whether package ‘MRPC’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/home/pavel/Documents/Research/Software/statnet/statnet.common/revdep/checks/MRPC/new/MRPC.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘MRPC’ ...
** package ‘MRPC’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘S4Vectors’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘MRPC’
* removing ‘/home/pavel/Documents/Research/Software/statnet/statnet.common/revdep/checks/MRPC/new/MRPC.Rcheck/MRPC’


```
### CRAN

```
* installing *source* package ‘MRPC’ ...
** package ‘MRPC’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘S4Vectors’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘MRPC’
* removing ‘/home/pavel/Documents/Research/Software/statnet/statnet.common/revdep/checks/MRPC/old/MRPC.Rcheck/MRPC’


```
# netdiffuseR

<details>

* Version: 1.22.6
* GitHub: https://github.com/USCCANA/netdiffuseR
* Source code: https://github.com/cran/netdiffuseR
* Date/Publication: 2023-08-30 17:00:10 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::revdep_details(, "netdiffuseR")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 14.2Mb
      sub-directories of 1Mb or more:
        doc    2.5Mb
        libs   9.9Mb
    ```

# netmediate

<details>

* Version: 1.0.1
* GitHub: NA
* Source code: https://github.com/cran/netmediate
* Date/Publication: 2024-06-19 08:40:07 UTC
* Number of recursive dependencies: 89

Run `revdepcheck::revdep_details(, "netmediate")` for more info

</details>

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘mediation’
    ```

# networkABC

<details>

* Version: 0.8-1
* GitHub: https://github.com/fbertran/networkABC
* Source code: https://github.com/cran/networkABC
* Date/Publication: 2022-10-19 00:02:35 UTC
* Number of recursive dependencies: 55

Run `revdepcheck::revdep_details(, "networkABC")` for more info

</details>

## In both

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘vignette.Rmd’ using rmarkdown
    
    Quitting from lines 39-40 [unnamed-chunk-2] (vignette.Rmd)
    Error: processing vignette 'vignette.Rmd' failed with diagnostics:
    there is no package called 'codetools'
    --- failed re-building ‘vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# NetworkChange

<details>

* Version: 0.8
* GitHub: https://github.com/jongheepark/NetworkChange
* Source code: https://github.com/cran/NetworkChange
* Date/Publication: 2022-03-04 07:30:02 UTC
* Number of recursive dependencies: 122

Run `revdepcheck::revdep_details(, "NetworkChange")` for more info

</details>

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) NetworkChange.Rd:163: Escaped LaTeX specials: \&
    checkRd: (-1) NetworkChangeRobust.Rd:144: Escaped LaTeX specials: \&
    checkRd: (-1) NetworkStatic.Rd:142: Escaped LaTeX specials: \&
    ```

# NetworkDistance

<details>

* Version: 0.3.4
* GitHub: NA
* Source code: https://github.com/cran/NetworkDistance
* Date/Publication: 2021-08-21 15:00:08 UTC
* Number of recursive dependencies: 53

Run `revdepcheck::revdep_details(, "NetworkDistance")` for more info

</details>

## In both

*   checking C++ specification ... NOTE
    ```
      Specified C++11: please drop specification unless essential
    ```

# networkDynamicData

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/networkDynamicData
* Date/Publication: 2016-01-12 00:23:27
* Number of recursive dependencies: 22

Run `revdepcheck::revdep_details(, "networkDynamicData")` for more info

</details>

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) harry_potter.Rd:44: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) harry_potter.Rd:45: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) harry_potter.Rd:46: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) harry_potter.Rd:47-48: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) harry_potter.Rd:49: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) vanDeBunt_students.Rd:46: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) vanDeBunt_students.Rd:47: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) vanDeBunt_students.Rd:48: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) vanDeBunt_students.Rd:49: Lost braces in \itemize; meant \describe ?
    ```

# NetworkExtinction

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/NetworkExtinction
* Date/Publication: 2023-03-31 11:40:02 UTC
* Number of recursive dependencies: 101

Run `revdepcheck::revdep_details(, "NetworkExtinction")` for more info

</details>

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) ExtinctionOrder.Rd:35: Lost braces
        35 | \item{RewiringDist}{a numeric matrix of NxN dimension (N... number of nodes in Network). Contains, for example, phylogenetic or functional trait distances between nodes in Network which are used by the Rewiring argument to calculate rewiring probabilities. If Rewiring == function(x){x}, this matrix is expected to contain probabilities of a connection being present between species-pairs.}
           |                                                                                                                                                                                                                                                                                            ^
    checkRd: (-1) RandomExtinctions.Rd:52: Lost braces
        52 | \item{RewiringDist}{a numeric matrix of NxN dimension (N... number of nodes in Network). Contains, for example, phylogenetic or functional trait distances between nodes in Network which are used by the Rewiring argument to calculate rewiring probabilities. If Rewiring == function(x){x}, this matrix is expected to contain probabilities of a connection being present between species-pairs.}
           |                                                                                                                                                                                                                                                                                            ^
    checkRd: (-1) RandomExtinctions.Rd:71-72: Lost braces
        71 | primary removals. For example, if a network has five species {A,B,
           |                                                              ^
    checkRd: (-1) RandomExtinctions.Rd:72: Lost braces; missing escapes or markup?
        72 | ,D,E} and a pre-defined "random" order of removal {C,A,B,E,D} with
           |                                                   ^
    checkRd: (-1) RandomExtinctions.Rd:77: Lost braces; missing escapes or markup?
        77 | vector, i.e., {C,A,B}."
           |               ^
    checkRd: (-1) SimulateExtinctions.Rd:38: Lost braces
        38 | \item{RewiringDist}{a numeric matrix of NxN dimension (N... number of nodes in Network). Contains, for example, phylogenetic or functional trait distances between nodes in Network which are used by the Rewiring argument to calculate rewiring probabilities. If Rewiring == function(x){x}, this matrix is expected to contain probabilities of a connection being present between species-pairs.}
           |                                                                                                                                                                                                                                                                                            ^
    ```

# PDN

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/PDN
* Date/Publication: 2017-11-03 23:27:43 UTC
* Number of recursive dependencies: 60

Run `revdepcheck::revdep_details(, "PDN")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘glmnet’
      All declared Imports should be used.
    ```

# relevent

<details>

* Version: 1.2-1
* GitHub: NA
* Source code: https://github.com/cran/relevent
* Date/Publication: 2023-01-24 08:10:02 UTC
* Number of recursive dependencies: 17

Run `revdepcheck::revdep_details(, "relevent")` for more info

</details>

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) rem.dyad.Rd:121: Lost braces; missing escapes or markup?
       121 | \item Single or multiple covariates, time varying: For \code{CovSnd}, \code{CovRec}, or \code{CovInt}, an \code{m} by \code{p} by {n} array whose respective dimensions index time (i.e., event number), covariate, and actor.  For \code{CovEvent}, a \code{m} by \code{p} by \code{n} by \code{n} array, whose dimensions are analogous to the previous case.
           |                                                                                                                                   ^
    ```

# RHMS

<details>

* Version: 1.7
* GitHub: NA
* Source code: https://github.com/cran/RHMS
* Date/Publication: 2021-09-27 15:50:02 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::revdep_details(, "RHMS")` for more info

</details>

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) reachRouting.Rd:15: Lost braces
        15 |   \item{inflow}{a vector of runoff (cms) presenting a runoff event generated by excess rainfall computed by \code{loss} methods or an object inherited from any of the following classes :{\code{transform} ; \code{reachRouting} ; \code{reservoirRouting}}.}
           |                                                                                                                                                                                           ^
    checkRd: (-1) reachRouting.base.Rd:14: Lost braces
        14 |   \item{inflow}{a vector of runoff (cms) or an object inherited from any of the following classes :{\code{transform} ; \code{reachRouting} ; \code{reservoirRouting}}.}
           |                                                                                                    ^
    checkRd: (-1) reachRouting.default.Rd:21: Lost braces
        21 |   \item{inflow}{a vector of runoff (cms) or an object inherited from any of the following classes :{\code{transform} ; \code{reachRouting} ; \code{reservoirRouting}}.}
           |                                                                                                    ^
    checkRd: (-1) reservoirRouting.Rd:13: Lost braces
        13 |   \item{inflow}{a vector of in (cms) presenting a runoff event generated by excess rainfall computed by \code{loss} methods or an object inherited from any of the following classes :{\code{transform} ; \code{reachRouting} ; \code{reservoirRouting}}.}
           |                                                                                                                                                                                       ^
    checkRd: (-1) reservoirRouting.base.Rd:13: Lost braces
        13 |   \item{inflow}{a vector of in (cms) presenting a runoff event generated by excess rainfall computed by \code{loss} methods or an object inherited from any of the following classes :{\code{transform} ; \code{reachRouting} ; \code{reservoirRouting}}.}
           |                                                                                                                                                                                       ^
    checkRd: (-1) reservoirRouting.default.Rd:18: Lost braces
        18 |   \item{inflow}{a vector of in (cms) presenting a runoff event generated by excess rainfall computed by \code{loss} methods or an object inherited from any of the following classes :{\code{transform} ; \code{reachRouting} ; \code{reservoirRouting}}.}
           |                                                                                                                                                                                       ^
    ```

# RSiena

<details>

* Version: 1.4.7
* GitHub: https://github.com/stocnet/rsiena
* Source code: https://github.com/cran/RSiena
* Date/Publication: 2024-02-21 12:10:02 UTC
* Number of recursive dependencies: 19

Run `revdepcheck::revdep_details(, "RSiena")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 20.5Mb
      sub-directories of 1Mb or more:
        R      1.0Mb
        libs  19.0Mb
    ```

# sand

<details>

* Version: 2.0.0
* GitHub: https://github.com/kolaczyk/sand
* Source code: https://github.com/cran/sand
* Date/Publication: 2020-07-02 07:20:06 UTC
* Number of recursive dependencies: 147

Run `revdepcheck::revdep_details(, "sand")` for more info

</details>

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      'GOstats', 'networkTomography'
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6 marked UTF-8 strings
    ```

# SBICgraph

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/SBICgraph
* Date/Publication: 2021-03-02 19:10:09 UTC
* Number of recursive dependencies: 47

Run `revdepcheck::revdep_details(, "SBICgraph")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘network’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# sfc

<details>

* Version: 0.1.0
* GitHub: https://github.com/ctfysh/sfc
* Source code: https://github.com/cran/sfc
* Date/Publication: 2016-08-25 10:01:01
* Number of recursive dependencies: 29

Run `revdepcheck::revdep_details(, "sfc")` for more info

</details>

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# sidier

<details>

* Version: 4.1.0
* GitHub: NA
* Source code: https://github.com/cran/sidier
* Date/Publication: 2021-06-25 11:50:02 UTC
* Number of recursive dependencies: 56

Run `revdepcheck::revdep_details(, "sidier")` for more info

</details>

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) pop.dist.Rd:41: Lost braces; missing escapes or markup?
        41 | where \emph{dist(i,j)} represents the distance between populations \emph{i} and \emph{j}, \emph{m} and \emph{n} are the number of sequences in populations \emph{i} and \emph{j}, respectively, and \emph{dist(H_{ki},H_{lj})} is the distance between the \emph{k-th} sequence found in population \emph{i} and the \emph{l-th} sequence found in population \emph{j}. 
           |                                                                                                                                                                                                                  ^
    checkRd: (-1) pop.dist.Rd:41: Lost braces; missing escapes or markup?
        41 | where \emph{dist(i,j)} represents the distance between populations \emph{i} and \emph{j}, \emph{m} and \emph{n} are the number of sequences in populations \emph{i} and \emph{j}, respectively, and \emph{dist(H_{ki},H_{lj})} is the distance between the \emph{k-th} sequence found in population \emph{i} and the \emph{l-th} sequence found in population \emph{j}. 
           |                                                                                                                                                                                                                         ^
    ```

# spaceNet

<details>

* Version: 1.2
* GitHub: NA
* Source code: https://github.com/cran/spaceNet
* Date/Publication: 2019-05-19 22:30:03 UTC
* Number of recursive dependencies: 31

Run `revdepcheck::revdep_details(, "spaceNet")` for more info

</details>

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘spaceNet-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: multiNet
    > ### Title: Latent Space Models for Multivariate Networks
    > ### Aliases: multiNet print.multiNet
    > 
    > ### ** Examples
    > 
    > data(vickers)
    ...
    > 
    > it <- 10     # small number of iterations just for example
    > 
    > # 2-dimensional latent space model, no covariates
    > mod <- multiNet(vickers, niter = it, D = 2)
    Error in dyn.load(file, DLLpath = DLLpath, ...) : 
      unable to load shared object '/home/pavel/Documents/Research/Software/statnet/statnet.common/revdep/library/spaceNet/RcppZiggurat/libs/RcppZiggurat.so':
      libgsl.so.27: cannot open shared object file: No such file or directory
    Calls: multiNet ... asNamespace -> loadNamespace -> library.dynam -> dyn.load
    Execution halted
    ```

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) heuSearch.Rd:24: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) heuSearch.Rd:25: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) heuSearch.Rd:26: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) multiNet.Rd:124: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) multiNet.Rd:125: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) multiNet.Rd:126-137: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) multiNet.Rd:128: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) multiNet.Rd:129-134: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) multiNet.Rd:138-147: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) multiNet.Rd:142: Lost braces in \itemize; meant \describe ?
    ...
    checkRd: (-1) multiNet.Rd:89: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) multiNet.Rd:90: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) multiNet.Rd:91: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) multiNet.Rd:97: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) multiNet.Rd:98: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) multiNet.Rd:99: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) multiNet.Rd:100: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) multiNet.Rd:101: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) multiNet.Rd:102: Lost braces in \itemize; \value handles \item{}{} directly
    checkRd: (-1) multiNet.Rd:103: Lost braces in \itemize; \value handles \item{}{} directly
    ```

# SSrat

<details>

* Version: 1.1
* GitHub: NA
* Source code: https://github.com/cran/SSrat
* Date/Publication: 2018-04-03 22:36:44 UTC
* Number of recursive dependencies: 18

Run `revdepcheck::revdep_details(, "SSrat")` for more info

</details>

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) readratdatafixed.Rd:25: Lost braces; missing escapes or markup?
        25 | number of the set {1, 2, 3}, {1..5}, {1..7} or {1..9}. When there are 10
           |                   ^
    checkRd: (-1) readratdatafixed.Rd:25: Lost braces; missing escapes or markup?
        25 | number of the set {1, 2, 3}, {1..5}, {1..7} or {1..9}. When there are 10
           |                              ^
    checkRd: (-1) readratdatafixed.Rd:25: Lost braces; missing escapes or markup?
        25 | number of the set {1, 2, 3}, {1..5}, {1..7} or {1..9}. When there are 10
           |                                      ^
    checkRd: (-1) readratdatafixed.Rd:25: Lost braces; missing escapes or markup?
        25 | number of the set {1, 2, 3}, {1..5}, {1..7} or {1..9}. When there are 10
           |                                                ^
    ```

# stargazer

<details>

* Version: 5.2.3
* GitHub: NA
* Source code: https://github.com/cran/stargazer
* Date/Publication: 2022-03-04 11:50:02 UTC
* Number of recursive dependencies: 0

Run `revdepcheck::revdep_details(, "stargazer")` for more info

</details>

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'AER', 'betareg', 'brglm', 'censReg', 'dynlm', 'eha', 'erer', 'ergm',
      'fGarch', 'gee', 'glmx', 'gmm', 'lfe', 'lme4', 'lmtest', 'MASS',
      'mclogit', 'mgcv', 'mlogit', 'nlme', 'nnet', 'ordinal', 'plm',
      'pscl', 'quantreg', 'rms', 'relevent', 'robustbase',
      'sampleSelection', 'spdep', 'survey', 'survival'
    ```

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) stargazer.Rd:229: Lost braces
       229 |   \item{report}{a character string containing only elements of {\code{"v"}, \code{"c"}, \code{"s"},\code{"t"}, \code{"p"}, \code{"*"}} that determines whether, and in which order, variable names (\code{"v"}), coefficients (\code{"c"}), standard errors/confidence intervals (\code{"s"}), test statistics (\code{"t"}) and p-values (\code{"p"}) should be reported in regression tables. If one of the aforementioned letters is followed by an asterisk (\code{"*"}), significance stars will be reported next to the corresponding statistic.}
           |                                                                ^
    checkRd: (-1) stargazer.Rd:246: Lost braces
       246 |   \item{table.placement}{a character string containing only elements of {\code{"h"}, \code{"t"},\code{"b"}, \code{"p"}, \code{"!"}, \code{"H"}} that determines the table placement in its LaTeX floating environment.}
           |                                                                         ^
    ```

# statnet

<details>

* Version: 2019.6
* GitHub: https://github.com/statnet/statnet
* Source code: https://github.com/cran/statnet
* Date/Publication: 2019-06-14 08:00:06 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::revdep_details(, "statnet")` for more info

</details>

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘networksis’
    ```

*   checking whether startup messages can be suppressed ... NOTE
    ```
    unable to reach CRAN
    
    It looks like this package (or a package it requires) has a startup
    message which cannot be suppressed: see ?packageStartupMessage.
    ```

# tergm

<details>

* Version: 4.2.0
* GitHub: https://github.com/statnet/tergm
* Source code: https://github.com/cran/tergm
* Date/Publication: 2023-05-30 12:20:02 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::revdep_details(, "tergm")` for more info

</details>

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) NEWS.Rd:427: Lost braces; missing escapes or markup?
       427 |       \item  Changes to \code{simulate.stergm} parameters: addition of \code{duration.dependent} parameter: {Logical: Whether the model terms in formula or model are duration dependent. E.g., if a duration-dependent term  is used in estimation/simulation model, the probability of forming or dissolving a tie may dependent on the age the dyad status. If TRUE, the matrix of tie ages will be allocated.}
           |                                                                                                             ^
    checkRd: (-1) discordBDStratTNT-ergmProposal-9267ec8b.Rd:18: Lost braces
        18 | code{\link[ergm:blocks-ergmConstraint]{blocks}} constraints and
           |     ^
    checkRd: (-1) discordTNT-ergmProposal-bc75dff0.Rd:9: Lost braces
         9 | code{discordance_fraction} of proposed toggles being made on the set of discordant dyads,
           |     ^
    ```

# texreg

<details>

* Version: 1.39.4
* GitHub: https://github.com/leifeld/texreg
* Source code: https://github.com/cran/texreg
* Date/Publication: 2024-07-24 12:20:01 UTC
* Number of recursive dependencies: 109

Run `revdepcheck::revdep_details(, "texreg")` for more info

</details>

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'AER', 'alpaca', 'betareg', 'Bergm', 'bife', 'biglm', 'brglm',
      'brms', 'btergm', 'dynlm', 'eha', 'ergm', 'feisr', 'fGarch',
      'fixest', 'forecast', 'gamlss', 'gamlss.inf', 'gee', 'glmmTMB',
      'gmm', 'gnm', 'h2o', 'latentnet', 'lfe', 'logitr', 'lqmm', 'maxLik',
      'metaSEM', 'mfx', 'mhurdle', 'miceadds', 'mlogit', 'MuMIn', 'oglmx',
      'ordinal', 'pglm', 'plm', 'relevent', 'remify', 'remstats',
      'remstimate', 'rms', 'robust', 'simex', 'spatialreg', 'spdep',
      'speedglm', 'truncreg', 'VGAM'
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘h2o’, ‘spatialreg’, ‘eha’, ‘MuMIn’, ‘Bergm’, ‘mfx’, ‘betareg’, ‘bife’, ‘biglm’, ‘brglm’, ‘brms’, ‘btergm’, ‘ordinal’, ‘dynlm’, ‘ergm’, ‘latentnet’, ‘forecast’, ‘fGarch’, ‘alpaca’, ‘feisr’, ‘lfe’, ‘fixest’, ‘gamlss’, ‘gamlss.inf’, ‘gee’, ‘gmm’, ‘miceadds’, ‘glmmTMB’, ‘gnm’, ‘AER’, ‘robust’, ‘lqmm’, ‘rms’, ‘maxLik’, ‘mhurdle’, ‘mlogit’, ‘oglmx’, ‘plm’, ‘pglm’, ‘relevent’, ‘remstimate’, ‘simex’, ‘speedglm’, ‘truncreg’, ‘VGAM’, ‘metaSEM’
    ```

# tip

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/tip
* Date/Publication: 2022-11-14 17:30:02 UTC
* Number of recursive dependencies: 101

Run `revdepcheck::revdep_details(, "tip")` for more info

</details>

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) partition_undirected_graph.Rd:20: Lost braces; missing escapes or markup?
        20 | \item{cutoff}{Numeric. The value max(0, g_{i,j} - cutoff) so that there are <\code{.num_components}> components in the graph.}
           |                                           ^
    ```

# WOTPLY

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/WOTPLY
* Date/Publication: 2022-09-12 07:43:01 UTC
* Number of recursive dependencies: 71

Run `revdepcheck::revdep_details(, "WOTPLY")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘sna’
      All declared Imports should be used.
    ```

# WRSS

<details>

* Version: 3.1
* GitHub: NA
* Source code: https://github.com/cran/WRSS
* Date/Publication: 2022-05-29 18:10:02 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::revdep_details(, "WRSS")` for more info

</details>

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) createDemandSite.Rd:26: Lost braces
        26 |   \item{suppliers}{(optional) A list of object(s) inherited from the folowing classes: \code{\link[WRSS]{createAquifer}}, \code{\link[WRSS]{createRiver}}, \code{\link[WRSS]{createReservoir}}, code{\link[WRSS]{createDiversion}}.}
           |                                                                                                                                                                                                     ^
    checkRd: (-1) createDemandSite.base.Rd:26: Lost braces
        26 |   \item{suppliers}{(optional) A list of object(s) inherited from the folowing classes: \code{\link[WRSS]{createAquifer}}, \code{\link[WRSS]{createRiver}}, \code{\link[WRSS]{createReservoir}}, code{\link[WRSS]{createDiversion}}.}
           |                                                                                                                                                                                                     ^
    checkRd: (-1) createDemandSite.default.Rd:32: Lost braces
        32 |   \item{suppliers}{(optional) A list of object(s) inherited from the folowing classes: \code{\link[WRSS]{createAquifer}}, \code{\link[WRSS]{createRiver}}, \code{\link[WRSS]{createReservoir}}, code{\link[WRSS]{createDiversion}}.}
           |                                                                                                                                                                                                     ^
    ```

# xergm.common

<details>

* Version: 1.7.8
* GitHub: https://github.com/leifeld/xergm.common
* Source code: https://github.com/cran/xergm.common
* Date/Publication: 2020-04-07 09:50:02 UTC
* Number of recursive dependencies: 36

Run `revdepcheck::revdep_details(, "xergm.common")` for more info

</details>

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) chemnet.Rd:111: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) chemnet.Rd:112: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) knecht.Rd:103: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) knecht.Rd:104: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) knecht.Rd:105: Lost braces in \itemize; meant \describe ?
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘RSiena’
    ```

