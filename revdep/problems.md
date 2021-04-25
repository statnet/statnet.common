# networkDynamic

<details>

* Version: 0.10.1
* GitHub: NA
* Source code: https://github.com/cran/networkDynamic
* Date/Publication: 2020-01-21 09:50:02 UTC
* Number of recursive dependencies: 36

Run `revdep_details(, "networkDynamic")` for more info

</details>

## Newly broken

*   checking loading without being on the library search path ... WARNING
    ```
    Loading required package: network
    network: Classes for Relational Data
    Version 1.16.1 created on 2020-10-06.
    copyright (c) 2005, Carter T. Butts, University of California-Irvine
                        Mark S. Handcock, University of California -- Los Angeles
                        David R. Hunter, Penn State University
                        Martina Morris, University of Washington
                        Skye Bender-deMoll, University of Washington
     For citation information, type citation("network").
     Type help("network-package") to get started.
    
    Error: package or namespace load failed for ‘networkDynamic’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     there is no package called ‘purrr’
    Execution halted
    
    It looks like this package has a loading problem when not on .libPaths:
    see the messages for details.
    ```

# sna

<details>

* Version: 2.6
* GitHub: NA
* Source code: https://github.com/cran/sna
* Date/Publication: 2020-10-06 08:50:03 UTC
* Number of recursive dependencies: 61

Run `revdep_details(, "sna")` for more info

</details>

## Newly broken

*   checking loading without being on the library search path ... WARNING
    ```
    Loading required package: statnet.common
    Error: package or namespace load failed for ‘statnet.common’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     there is no package called ‘purrr’
    Error: package ‘statnet.common’ could not be loaded
    Execution halted
    
    It looks like this package has a loading problem when not on .libPaths:
    see the messages for details.
    ```

