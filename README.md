---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# Introduction
A simple, easy to use Flux Balance Analysis package with a tidy data approach. Just `data_frames` and the occasional `list`, no new classes to learn. The focus is on simplicity and speed. Models are expected as a flat table, and results can be simply appended to the table. This makes this package very suitable for useage in pipelines with pre- and post- processing of models and results, so that it works well as a backbone for customized methods. Loading, parsing and evaluating a model takes around 0.1s, which, together with the straightforward data structures used, makes this library very suitable for large parameter sweeps.

## Simple Example
This example calculates the fluxes for the model ecoli_core. Ecoli_core starts out as a data frame, and is returned as the same data frame, with fluxes appended.


```r
library(FluxBalanceAnalyzeR)
data(ecoli_core)

ecoli_core_with_flux <- find_fluxes_df(ecoli_core)
```

## Verbose Example
This example finds the fluxes in ecoli_core, just like the previous case. However, this is longer to show how the package works.


```r
library(FluxBalanceAnalyzeR)
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

## Installation

```r
devtools::install_github('maxconway/FluxBalanceAnalyzeR')
```

## Notes and FAQs

### Comparison with other packages
The most used famous package for constraint based methods is probably COBRA, a Matlab package. If you prefer Matlab to R, you'll probably want to try that before this.

The existing R packages include `sybil` and `abcdeFBA`. 

- `sybil` has 785 items in its help, whereas this FluxBalanceAnalyzeR has 12. This is mainly because sybil makes extensive use of S4 classes; for instance `sybil` has 10 functions dealing with reaction IDs, whereas in FluxBalanceAnalyzeR, the reaction Id is just a normal character vector. For this reason, you'll probably find FluxBalanceAnalyzeR much faster to get to grips with.
- `abcdeFBA` is much smaller than `sybil`, so it is another package to try first.

### Gurobi
At present, FluxBalanceAnalyzeR only works with Gurobi. Gurobi is fast, relatively easy to install, and free for academics, but other optimizers are on the todo list.
To install Gurobi for R on Linux, you need to download a tar file from the website, and follow instructions from http://www.gurobi.com/documentation/6.0/quickstart_linux.pdf up to page 8, then get a licence from the Gurobi website. 
You can then install the R package contained in the tar file using a simple `install.packages`.
