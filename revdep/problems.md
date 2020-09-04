# broom

<details>

* Version: 0.7.0
* GitHub: https://github.com/tidymodels/broom
* Source code: https://github.com/cran/broom
* Date/Publication: 2020-07-09 12:30:09 UTC
* Number of recursive dependencies: 277

Run `revdep_details(, "broom")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    networks (row)    10  10  10  10  10  10  10  10  10   10
    networks (col)    10  10  10  10  10  10  10  10  10   10
    covariates (row)  10  10  10  10  10  10  10  10  10   10
    covariates (col)  10  10  10  10  10  10  10  10  10   10
    
    All networks are conformable.
    
    Dimensions of the network and covariates after adjustment:
                     t=1 t=2 t=3 t=4 t=5 t=6 t=7 t=8 t=9 t=10
    networks (row)    10  10  10  10  10  10  10  10  10   10
    networks (col)    10  10  10  10  10  10  10  10  10   10
    covariates (row)  10  10  10  10  10  10  10  10  10   10
    covariates (col)  10  10  10  10  10  10  10  10  10   10
    
    Starting pseudolikelihood estimation with 100 bootstrapping replications on a single computing core...
    Warning: 'compact.rle' is deprecated.
    Use 'compress' instead.
    See help("Deprecated")
    Error in as.rle(x) : could not find function "as.rle"
    Calls: btergm ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
    Execution halted
    ```

# btergm

<details>

* Version: 1.9.9
* GitHub: https://github.com/leifeld/btergm
* Source code: https://github.com/cran/btergm
* Date/Publication: 2020-06-18 05:00:06 UTC
* Number of recursive dependencies: 73

Run `revdep_details(, "btergm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    networks (row)    10  10  10  10  10  10  10  10  10   10
    networks (col)    10  10  10  10  10  10  10  10  10   10
    covariates (row)  10  10  10  10  10  10  10  10  10   10
    covariates (col)  10  10  10  10  10  10  10  10  10   10
    
    All networks are conformable.
    
    Dimensions of the network and covariates after adjustment:
                     t=1 t=2 t=3 t=4 t=5 t=6 t=7 t=8 t=9 t=10
    networks (row)    10  10  10  10  10  10  10  10  10   10
    networks (col)    10  10  10  10  10  10  10  10  10   10
    covariates (row)  10  10  10  10  10  10  10  10  10   10
    covariates (col)  10  10  10  10  10  10  10  10  10   10
    
    Starting pseudolikelihood estimation with 100 bootstrapping replications on a single computing core...
    Warning: 'compact.rle' is deprecated.
    Use 'compress' instead.
    See help("Deprecated")
    Error in as.rle(x) : could not find function "as.rle"
    Calls: btergm ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [90m 3. [39mergm::ergm.design(nw, verbose = verbose)
      [90m 7. [39mergm:::InitErgmConstraint..attributes(...)
      [90m 8. [39mergm::rlebdm(compact.rle(d), n)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 0 | SKIPPED: 1 | WARNINGS: 6 | FAILED: 6 ]
      1. Error: btergm estimation works (@test-btergm.R#23) 
      2. Error: fastglm works like speedglm (@test-btergm.R#58) 
      3. Error: offset argument in btergm works without composition change (@test-btergm.R#66) 
      4. Error: mtergm estimation works (@test-btergm.R#144) 
      5. Error: simulation of new networks works (@test-btergm.R#156) 
      6. Error: (unknown) (@test-gof.R#21) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking whether package ‘btergm’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/homes/morrism/GitHub/StatnetOrganization/statnet.common/revdep/checks/btergm/new/btergm.Rcheck/00install.out’ for details.
    ```

# ergm

<details>

* Version: 3.10.4
* GitHub: https://github.com/statnet/ergm
* Source code: https://github.com/cran/ergm
* Date/Publication: 2019-06-10 05:30:07 UTC
* Number of recursive dependencies: 71

Run `revdep_details(, "ergm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Running examples in ‘ergm-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ergmMPLE
    > ### Title: ERGM Predictors and response for logistic regression calculation
    > ###   of MPLE
    > ### Aliases: ergmMPLE
    > ### Keywords: models regression
    > 
    > ### ** Examples
    > 
    > 
    > data(faux.mesa.high)
    > formula <- faux.mesa.high ~ edges + nodematch("Sex") + nodefactor("Grade")
    > mplesetup <- ergmMPLE(formula)
    Warning: 'compact.rle' is deprecated.
    Use 'compress' instead.
    See help("Deprecated")
    Error in as.rle(x) : could not find function "as.rle"
    Calls: ergmMPLE ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/constrain_degrees_edges.R’ failed.
    Last 13 lines of output:
      > id <- function(nw) apply(as.matrix(nw, matrix.type="adjacency"), 2, sum)
      > e <- function(nw) network.edgecount(nw)
      > 
      > ###### Directed
      > y0 <- as.network(n, density=d, directed=TRUE)
      > 
      > ### Outdegrees
      > ys <- simulate(y0~sender(nodes=TRUE)+receiver(nodes=TRUE), constraints=~odegrees, coef=rep(0,n*2), nsim=nsim, output="stats")
      Error in as.rle(x) : could not find function "as.rle"
      Calls: simulate ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
      In addition: Warning message:
      'compact.rle' is deprecated.
      Use 'compress' instead.
      See help("Deprecated") 
      Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    rlebdm: no visible global function definition for ‘as.rle’
    Undefined global functions or variables:
      as.rle
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        R      1.2Mb
        doc    1.7Mb
        libs   2.5Mb
    ```

# ergm.count

<details>

* Version: 3.4.0
* GitHub: https://github.com/statnet/ergm.count
* Source code: https://github.com/cran/ergm.count
* Date/Publication: 2019-05-15 07:42:59 UTC
* Number of recursive dependencies: 31

Run `revdep_details(, "ergm.count")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/valued_fit.R’ failed.
    Last 13 lines of output:
      > diag(m) <- 0
      > y <- as.network(m, matrix.type="a", directed=TRUE, ignore.eval=FALSE, names.eval="w")
      > 
      > truth <- log(sum(m)/n/(n-1))
      > diag(m) <- NA
      > 
      > efit <- ergm(y ~ sum, response="w", reference=~Poisson, verbose=TRUE, control=control.ergm(MCMLE.effectiveSize=128))
      Evaluating network in model.
      Initializing Metropolis-Hastings proposal(s):Error in as.rle(x) : could not find function "as.rle"
      Calls: ergm ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
      In addition: Warning message:
      'compact.rle' is deprecated.
      Use 'compress' instead.
      See help("Deprecated") 
      Execution halted
    ```

# ergm.ego

<details>

* Version: 0.5
* GitHub: https://github.com/statnet/ergm.ego
* Source code: https://github.com/cran/ergm.ego
* Date/Publication: 2019-05-31 16:00:03 UTC
* Number of recursive dependencies: 57

Run `revdep_details(, "ergm.ego")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Running examples in ‘ergm.ego-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: degreedist.egodata
    > ### Title: Plotting the degree distribution of an egocentric dataset
    > ### Aliases: degreedist.egodata degreedist
    > 
    > ### ** Examples
    > 
    > 
    > data(faux.mesa.high)
    > fmh.ego <- as.egodata(faux.mesa.high)
    Network does not have vertex attribute ‘vertex.names’ to use as ego ID; using 1..N.
    > 
    > degreedist(fmh.ego,by="Grade",brgmod=TRUE)
    Warning: 'compact.rle' is deprecated.
    Use 'compress' instead.
    See help("Deprecated")
    Error in as.rle(x) : could not find function "as.rle"
    Calls: degreedist ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/EgoStat.tests.R’ failed.
    Last 13 lines of output:
      > ds <- c(10,15,5,20)
      > 
      > y <- network.initialize(n, directed=FALSE)
      > y %v% "a" <- sample(1:3+6,n,replace=TRUE)
      > y %v% "b" <- sample(letters[1:4],n,replace=TRUE)
      > y %v% "c" <- sample(runif(10),n,replace=TRUE)
      > y %v% "d" <- runif(n)
      > y <- san(y~edges+degree(0:3), target.stats=c(e,ds))
      Error in as.rle(x) : could not find function "as.rle"
      Calls: san ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
      In addition: Warning message:
      'compact.rle' is deprecated.
      Use 'compress' instead.
      See help("Deprecated") 
      Execution halted
    ```

# ergm.rank

<details>

* Version: 1.2.0
* GitHub: https://github.com/statnet/ergm.rank
* Source code: https://github.com/cran/ergm.rank
* Date/Publication: 2019-05-15 07:43:03 UTC
* Number of recursive dependencies: 31

Run `revdep_details(, "ergm.rank")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/termTests_rank.R’ failed.
    Last 13 lines of output:
      +                   rank.nonconformity("local2")+
      +                   rank.nonconformity("localAND")+
      +                   rank.deference+
      +                   rank.nodeicov("v")+
      +                   rank.edgecov("m")+
      +                   rank.inconsistency(nw0,"r",xa),
      +                 coef=rep(0,8),response="r", reference=~DiscUnif(1, n-1), nsim=S, statsonly=FALSE)
      Error in as.rle(x) : could not find function "as.rle"
      Calls: simulate ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
      In addition: Warning messages:
      1: Use of 'statsonly=' argument has been deprecated. Use 'output='stats'' instead. 
      2: 'compact.rle' is deprecated.
      Use 'compress' instead.
      See help("Deprecated") 
      Execution halted
    ```

# ergm.userterms

<details>

* Version: 3.10.0
* GitHub: https://github.com/statnet/ergm.userterms
* Source code: https://github.com/cran/ergm.userterms
* Date/Publication: 2019-05-15 07:43:05 UTC
* Number of recursive dependencies: 31

Run `revdep_details(, "ergm.userterms")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    The error most likely occurred in:
    
    > ### Name: ergm.userterms-package
    > ### Title: User-defined terms used in Exponential Family Random Graph
    > ###   Models
    > ### Aliases: ergm.userterms-package ergm.userterms
    > ### Keywords: package models
    > 
    > ### ** Examples
    > 
    > data(faux.mesa.high)
    > summary(faux.mesa.high~mindegree(2))
    mindegree2 
            97 
    > fit <- ergm(faux.mesa.high~mindegree(2), estimate="MPLE")
    Warning: 'compact.rle' is deprecated.
    Use 'compress' instead.
    See help("Deprecated")
    Error in as.rle(x) : could not find function "as.rle"
    Calls: ergm ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/examples.R’ failed.
    Last 13 lines of output:
               9 
      > summary(flomarriage~mindegree(1,by="priorates"))
      mindegree.priorates1 
                         2 
      Warning message:
      `set_attrs()` is deprecated as of rlang 0.3.0
      [90mThis warning is displayed once per session.[39m 
      > fit <- ergm(flomarriage~edges+mindegree(1,by="priorates"))
      Error in as.rle(x) : could not find function "as.rle"
      Calls: ergm ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
      In addition: Warning message:
      'compact.rle' is deprecated.
      Use 'compress' instead.
      See help("Deprecated") 
      Execution halted
    ```

# latentnet

<details>

* Version: 2.10.5
* GitHub: https://github.com/statnet/latentnet
* Source code: https://github.com/cran/latentnet
* Date/Publication: 2020-03-22 08:50:02 UTC
* Number of recursive dependencies: 110

Run `revdep_details(, "latentnet")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/nolatent.R’ failed.
    Last 13 lines of output:
          order
      
      > 
      > data(sampson)
      > 
      > monks.nmr<-ergmm(samplike~nodematch("group")+rreceiver)
      Error in as.rle(x) : could not find function "as.rle"
      Calls: ergmm ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
      In addition: Warning messages:
      1: `set_attrs()` is deprecated as of rlang 0.3.0
      [90mThis warning is displayed once per session.[39m 
      2: 'compact.rle' is deprecated.
      Use 'compress' instead.
      See help("Deprecated") 
      Execution halted
    ```

# tergm

<details>

* Version: 3.6.1
* GitHub: https://github.com/statnet/tergm
* Source code: https://github.com/cran/tergm
* Date/Publication: 2019-06-12 10:10:18 UTC
* Number of recursive dependencies: 47

Run `revdep_details(, "tergm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Title: Draw from the distribution of an Separable Temporal Exponential
    > ###   Family Random Graph Model
    > ### Aliases: simulate.stergm simulate.network simulate.networkDynamic
    > 
    > ### ** Examples
    > 
    > 
    > logit<-function(p)log(p/(1-p))
    > coef.form.f<-function(coef.diss,density) -log(((1+exp(coef.diss))/(density/(1-density)))-1)
    > 
    > # Construct a network with 20 nodes and 20 edges
    > n<-20
    > target.stats<-edges<-20
    > g0<-network.initialize(n,dir=TRUE)
    > g1<-san(g0~edges,target.stats=target.stats,verbose=TRUE)
    Warning: 'compact.rle' is deprecated.
    Use 'compress' instead.
    See help("Deprecated")
    Error in as.rle(x) : could not find function "as.rle"
    Calls: san ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/combine.networks.R’ failed.
    Last 13 lines of output:
      +                   y <- network.initialize(n,dir=FALSE)
      +                   y <- simulate(y~edges, coef=-1)
      +                   y %n% "x" <- matrix(runif(n*n),n,n)
      +                   y %v% "v" <- runif(n)
      +                   y %e% "e" <- runif(network.edgecount(y))
      +                   y
      +                 },
      +                 simplify=FALSE)
      Error in as.rle(x) : could not find function "as.rle"
      Calls: replicate ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
      In addition: Warning message:
      'compact.rle' is deprecated.
      Use 'compress' instead.
      See help("Deprecated") 
      Execution halted
    ```

# tergmLite

<details>

* Version: 2.2.1
* GitHub: NA
* Source code: https://github.com/cran/tergmLite
* Date/Publication: 2020-07-22 16:50:03 UTC
* Number of recursive dependencies: 68

Run `revdep_details(, "tergmLite")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 0 | SKIPPED: 0 | WARNINGS: 24 | FAILED: 24 ]
      1. Error: simulate_network (@test-examples.R#16) 
      2. Error: simulate_ergm (@test-examples.R#58) 
      3. Error: init_tergmLite (@test-examples.R#90) 
      4. Error: networkLite (@test-examples.R#123) 
      5. Error: updateModelTermInputs (@test-examples.R#153) 
      6. Error: add_vertices (@test-examples.R#193) 
      7. Error: delete_vertices (@test-examples.R#225) 
      8. Error: concurrent (@test-updateModelTermInputs.R#10) 
      9. Error: concurrent_by (@test-updateModelTermInputs.R#38) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

