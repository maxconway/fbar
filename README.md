
<!-- README.md is generated from README.Rmd. Please edit that file -->
FluxBalanceAnalyzeR
===================

FluxBalanceAnalyzeR is a simple, easy to use Flux Balance Analysis package with a tidy data approach. Just `data_frames` and the occasional `list`, no new classes to learn. The focus is on simplicity and speed. Models are expected as a flat table, and results can be simply appended to the table. This makes this package very suitable for useage in pipelines with pre- and post- processing of models and results, so that it works well as a backbone for customized methods. Loading, parsing and evaluating a model takes around 0.1s, which, together with the straightforward data structures used, makes this library very suitable for large parameter sweeps.

A Simple Example
----------------

This example calculates the fluxes for the model ecoli\_core. Ecoli\_core starts out as a data frame, and is returned as the same data frame, with fluxes appended.

``` r
library(FluxBalanceAnalyzeR)
data(ecoli_core)

ecoli_core_with_flux <- find_fluxes_df(ecoli_core)
```

A Complicated Example
---------------------

This example finds the fluxes in ecoli\_core, just like the previous case. However, this has more detail to show how the package works.

``` r
library(FluxBalanceAnalyzeR)
library(dplyr)
library(gurobi)
data(ecoli_core)

gurobi_result <- ecoli_core %>%
  reactiontbl_to_expanded %>%
  expanded_to_gurobi %>%
  gurobi(params = list(OutputFlag=0))

ecoli_core_with_flux <- ecoli_core %>%
  mutate(flux = gurobi_result$x)
```

This example expands the single data frame model into an intermediate form, the collapses it back to a gurobi model and evaluates it. Then it adds the result as a column, named flux. This longer style is useful because it allows access to the intermediate form. This just consists of three data frames: metabolites, stoichiometry, and reactions. This makes it easy to alter and combine models.

Functions
---------

FluxBalanceAnalyzeR's functions can be considered in three groups: convenience wrappers which perform a common workflow all in one go, parsing and conversion functions that form the core of the package and provide extensibility, and functions for gene set processing which allow models to be parameterized by genetic information.

#### Convenience wrappers

These functions wrap common workflows. They parse and evaluate models all in one go.

-   `find_fluxes_df` Given a metabolic model as a data frame, return a new data frame with fluxes. For simple FBA, this is what you want.
-   `find_fluxes_vector` Given a metabolic model as vectors, return the flux distribution.
-   `find_flux_varability_df` Given a metabolic model as a data frame, return a new data frame with fluxes and variability.

#### Parsing and conversion

These functions convert metabolic models between different formats.

-   `reactiontbl_to_gurobi` Convert a reaction table data frame to gurobi format. This is shorthand for `reactiontbl_to_expanded` followed by `expanded_to_gurobi`.
-   `reactiontbl_to_expanded` Convert a reaction table to an expanded, intermediate, format.
-   `expanded_to_gurobi` Convert a metabolic model in expanded format to a gurobi problem.

#### Gene set processing

These functions process gene protein reaction mappings.

-   `gene_eval` Evaluate gene sets in the context of particular gene presence levels.
-   `gene_associate` Apply gene presence levels to a metabolic model.

Notes and FAQs
--------------

### Installation

``` r
devtools::install_github('maxconway/FluxBalanceAnalyzeR')
```

### Comparison with other packages

The most used famous package for constraint based methods is probably COBRA, a Matlab package. If you prefer Matlab to R, you'll probably want to try that before this.

The existing R packages include `sybil` and `abcdeFBA`.

-   `sybil` has a total 750 functions and classes, whereas FluxBalanceAnalyzeR has only 12 functions; this is mainly because sybil makes extensive use of S4 classes. For instance `sybil` has 10 functions dealing with reaction abbreviations, whereas in FluxBalanceAnalyzeR the reaction abbreviation is just a normal character vector. For this reason, you'll probably find FluxBalanceAnalyzeR much faster to get to grips with.
-   `abcdeFBA` is much smaller than `sybil`, so it is another package to try before `sybil`, especially if you can't get hold of Gurobi.

### Gurobi

At present, FluxBalanceAnalyzeR only works with Gurobi. Gurobi is fast, relatively easy to install, and free for academics, but other optimizers are on the todo list.

To install Gurobi for R on Linux, you need to download a tar file from the website, and follow instructions from <http://www.gurobi.com/documentation/6.0/quickstart_linux.pdf> up to page 8, then get a licence from the Gurobi website. You can then install the R package contained in the tar file using a simple `install.packages`.

### Bugs and feature requests

If you find problems with the package, or there's anything that it doesn't do which you think it should, please submit them to <https://github.com/maxconway/FluxBalanceAnalyzeR/issues>. In particular, let me know if there is an optimizer that isn't supported which you would like supported, or if you have a workflow which might make sense for inclusion as a default convenience function.
