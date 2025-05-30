# statnet.common 4.11.0

## New utilities

* New function, `modify_in_place()`, that attempts to modify the argument of its caller in place. A function can call it to modify its own arguments in place.

* New function, `log1mexp(x)` to compute `log(1-exp(-x))` minimizing the loss of precision. R provides a C macro `log1mexp(x)` but not the corresponding R wrapper.

* New matrix functions: `qrssolve()` to solve linear systems via QR decomposition after scaling, and `qrsolve()` to do the same without scaling, along with `sandwich_qrsolve()`, `sandwich_qrssolve()`, `sandwich_ginv()`, and `sandwich_sginv()`.

* New function, `match_names()` to replace `vector.namesmatch()` with more flexible behavior and error checking.

* New functions, `ERRVL2()` and `ERRVL3()` that, unlike `ERRVL()`, do not require the expressions to be wrapped in `try()`.

* New function, `replace()`, a more pipe-friendly drop-in replacement for `base::replace()` for which replacement indices and values can be specified as functions evaluated on the input vector. `replace()<-` is also implemented, allowing the list to be modified in place.

* New pair of functions, `arr_to_coo()` and `arr_from_coo()`, to translate between an array and its list of non-default values.

* New function, `set_diag()` to set a diagonal of a matrix to specified values and return (as opposed to modifying it in place).

## Enhancements to existing utilities

* The behavior of `trim_env()` has changed: if no variables are to be copied in, the environment is set directly to `baseenv()`.

* `statnetStartupMessage()` now prints `Remote:` information if the package was installed from a remote.

* Matrices returned by the `lweighted.*()` family of functions now inherit dimensional names.

* `xTAx_eigen()` (and therefore `xTAx_seigen()`) now handle matrix-valued x correctly and check that `x` is in the span of `A`.

* Scaling version of matrix operations now use `.Machine$double.xmax/(1 + .Machine$double.eps)` as the inverse of the zero diagonal.

* `all_identical()` can now use a custom comparison predicate (e.g., `all.equal()`), and the use any of the elements in the list as the reference. (Thanks to Michał Bojanowski @mbojan.)

# statnet.common 4.10.0

## New utilities

* New linear algebra utilities: `ginv_eigen()`, which performs generalised inverse via eigendecomposition rather than SVD, to be used by `sginv()` if `snnd=TRUE`; and `xTAx_eigen()` and `xTAx_seigen()` to evaluate the inverse quadratic form using eigendecomposition.

* A new function `var.mcmc.list()` "method" to evaluate the covariance matrix for an `mcmc.list` without constructing a large matrix.

## Enhancements to existing utilities

* `colMeans.mcmc.list()` "method" no longer constructs a large matrix when calculating.

* `lweighted.var()` and `lweighted.cov()` now take an additional argument `onerow=` to specify what they should return (`NA`, 0, or something else) if the input matrix has one row.

## Bug fixes

* `as.cntrol.list.list()` and hence `snctrl()` no longer clobbers nested controls, e.g., `control.ergm(SAN=control.san(...),...)`.

* To facilitate support for earlier versions of R, avoid using the built-in pipe (`|>`) for now.

# statnet.common 4.9.0

## New utilities

* A new function,`lweighted.cov()`, to compute weighted covariance between two matrices or vectors.

* New linear algebra utilities, `is.SPD()`, `sandwich_solve()`, `sandwich_ssolve()`, `sginv()`, `snearPD()`, `srcond()`, `ssolve()`, `xAxT()`, `xTAx()`, `xTAx_qrsolve()`, `xTAx_qrssolve()`, `xTAx_solve()`, and `xTAx_ssolve()` moved from `ergm` and documented.

## Bug fixes

* In `handle.controls()`, arguments that are `match.arg()`-ed are now evaluated in the correct frame.

# statnet.common 4.8.0

## New utilities

* A helper function `unused_dots_warning()` is exported that works with `rlang::check_dots_used()` to print an informative message.

# statnet.common 4.7.0

## New utilities

* An S3 class `term_list` for storing terms extracted from a formula, by `list_rhs.formula()` and others, containing information about each term's sign and environment. Concatenation, indexing, and print methods are implemented.

## Bug fixes

* `list_rhs.formula()` can now handle `NULL` terms on the RHS.

# statnet.common 4.6.0

## New utilities

* An implementation of Welford's online algorithm for calculating sample mean and variance has been added as a class `Welford` that implements method `update()` and maintains elements `$n`, `$means`, `$SSDs`, and `$vars`.

## Bug fixes

* `snctrl()` was issuing a warning twice when called with a misspelled argument.

* `deInf()` now handles `NULL` input.

* in `locate_function()` a subtle bug has been fixed in handling of visible as opposed to invisible objects.

# statnet.common 4.5.0

## New utilities

* `ergm`'s term locator functions (`locate_function()` and `locate_prefixed_function()`) have been moved from `ergm`.

* A new function, `default_options()`, a wrapper around `options()` that drops options already set.

* A new function, `as.control.list()` generic and methods which take an R list and call an appropriate `control.*()` function on it.

* `check.control.class()` now first runs the control argument through `as.control.list()` and overwrites, so `control=` arguments to many functions can be plain lists.

* A new function, `simplify_simple()`, which takes a list and returns an atomic vector if the elements of the list are atomic and of length 1, with particular handling for `NULL` and empty (0-length) elements.

* A new function, `snctrl()` (StatNet ConTRoL), designed so that argument completion will complete all available control functions. Looking up its help (`?snctrl`) produces a dynamic list of all control parameters and their packages and control functions that is updated as packages are loaded and unloaded.

* A new function, `handle.controls()`, that performs the most normal functions in a `control.*()` function.

* Two trivial helper functions, `base_env()` and `empty_env()`, to replace an object's environment with `baseenv()` and `emptyenv()`,
respectively.

* A new function, `fixed.pval()` that wraps `base::format.pval()` with better default arguments.

* A reimplementation of `attr()` is exported, which disables partial matching by default.

## Enhancements to existing utilities

* `statnetStartupMessage()` now first looks for a `comment=(affil=...)` for the contributor's affiliation, before using e-mail.

* Improved output formatting for `.Deprecate_once()`.

* `append_rhs.formula()` now accepts NULL as the first argument, in which case it creates a new formula, and takes an additonal argument `env=`, which is used as this new formula's environment.

## Miscellaneous changes

* `rle` utilities are no longer reexported.

* `statnet.common` no longer depends on `purrr`.

* `statnetStartupMessage()` has been simplified.

# statnet.common 4.4.0

## `rle` utilities have been moved to a separate package, `rle`

* Major methods are reexported, for now.

## New utilities

* `split()` methods for matrices and arrays, to split them along a margin.

* `trim_env()`, a generic that will replace an environment (possibly attached to another object) with a sub-environment containing only objects whose names are specified.

* A `diff()` method for control lists and a `print()` method for the resulting differences.

* `deInf()`, to replace `.deinf()` in package `ergm`.

* A `compress()` generic, a `compress()` method for RLEs, and a `doNotCompress` argument to `rep.rle()`.  Both `compact.rle()` and the `doNotCompact` argument to `rep.rle()` are now deprecated.

## Enhancements to existing utilities

* Various optimizations have been made to RLEs.

* `nonsimp_update.formula()` now handles both one and two sided formulas; it also now copies all names except `...` when `from.new = TRUE`.

## Bug fixes

* `str.rle()` now works despite the overridden `length()` method.

# statnet.common 4.3.0

## New utilities

* `EVL()`, a family of functions like `NVL()`, that treat any object of length 0 as `NULL`.

* `once()`, a `purrr`-style adverb that wraps a function to only evaluate the first time it's called with a given configuration of arguments.

* `persistEval()` and `persistEvalQ()` to retry evaluating a given expression a specified number of times.

## Bug fixes

* In `forkTimeout()`, don't collect a process twice. Thanks to Tomas Kalibera for suggesting the fix.

# statnet.common 4.2.0

## New utilities

* `.Deprecate_once()` calls `.Deprecated()`, passing all its arguments through, but only the first time it's called.

* `.Deprecate_method()` calls `.Deprecated()`, but only if a method has been called by name, i.e., `METHOD.CLASS`.

* `forkTimeout()` evaluates an R expression with a hard time limit (except on Windows) by forking a process. Unlike `setTimeLimit()`, it enforces the limit even on native code.

* `ult()` is a convenience function that extracts or replaces elements of a list indexed from the end.

## Miscellaneous

* `statnet.common` now depends on R >= 3.5 due to what appears to be a method dispatching bug in earlier versions.

* The package no longer Enhances `coda`.

# statnet.common 4.1.4

## New utilities

* `despace()` removes whitespace from a string.

* Pseudo-methods `colMeans.mcmc.list()`, `sweep.mcmc.list()`, and `lapply.mcmc.list()` (migrated from the `ergm` package).

* `filter_rhs.formula()` selectively deletes terms in on the RHS of a formula.

* `eval_lhs.formula()` extracts the LHS of the formula and evaluates it in the specified environment.

* `NVL2()` and `NVL3()` for flexible substitution of null values.

* `message_print()` formats its arguments as if for `print()` or `show()` methods, but then prints to stderr like `message()`.

## Enhancements to existing utilities

* `paste.and()` now takes an additional `con=` argument, allowing a conjunction other than "and" to be used.

* `ERRVL()` now uses lazy evaluation and lets the user dot-substitute the previous argument's try-error into the next argument.

## Bug fixes

* Printing for control lists now works for function arguments.

* A number of improvements to `rle` methods.

## Miscellaneous

* A number of functions have been renamed for consistency:
    * `term.list.formula()` →  `list_rhs.formula()`
    * `append.rhs.formula()` →  `append_rhs.formula()`
    * `nonsimp.update.formula()` →  `nonsimp_update.formula()`

* Citation utilities have been deprecated, since CRAN's structure makes them unusable.

# statnet.common 4.0.0

* The package now uses `Roxygen` for documentation.

* `term.list.formula()` output format has been changed, since support of attributes on symbols is being deprecated.

* A library of methods has been added for the base `rle` class, implementing concatenation, compaction, and a number of binary operations.

* `all_same()` has been  moved from ergm and renamed to `all_identical()`.

* A new assignment method `NVL()<-` overwrites a variable if its value is NULL.

* A set of classes and functions for manipulating and efficiently performing calculations on dense matrices or vectors with weighted rows or elements (possibly on the log scale) has been added.

* New control parameter helper function, control.remap() has been added. Autodetection of function names by `set.control.class()` and `check.control.class()` has been deprecated and now results in a warning.

* Improvements to the compressed data frame code, including an order() generic.

* Miscellaneous robustifications added.

* Native routine registration has been added.

# statnet.common 3.3.0
* `append.rhs.formula()`, `vectors.namesmatch()`, `term.list.formula()`, and `ergm.update.formula()` (renamed to `nosimp.update.formula()`) moved from `ergm`.

* Skye Bender-deMoll has been added as a contributor.

# statnet.common 3.2.3
* `ERRVL()` moved from `ergm`.

* Some `NAMESPACE` and other fixes to pass CRAN checks.

# statnet.common 3.2.2
* control class improvements and bug fixes.

# statnet.common 3.1.1
* Updated e-mail address

* Some improvements to opttest.

# statnet.common 3.1.0
* Initial release, incorporating the control class framework (`set.control.class()`, `check.control.class()`, `print.control.list()`); startup message framework; `NVL()`; `sort.data.frame()`; `compress.data.frame()`; `paste.and()`; citation utilities framework; and `opttest()` framework.
