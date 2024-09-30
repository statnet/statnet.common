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
