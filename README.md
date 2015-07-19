# Introduction
This package currently provides one function, \code{parse_reaction_table}, which taks a metabolic model as a data frame, and outputs it in a format suitable for input the the Gurobi Linear Programming library.

The motivation is to provide a simple FBA package that is suitable for integration with other code; this contrasts with Sybil, which attempts to provide an all in one toolkit.

# Example
```{r}
library(FluxBalanceAnalyzeR)
library(gurobi)
data(iJO1366)
model <- parse_reaction_table(iJO1366)
result <- gurobi(model)
iJO1366$flux <- result$x
```

# Installation
## Gurobi
To install Gurobi for R on Linux, you need to download a tar file from the website, and follow instructions from http://www.gurobi.com/documentation/6.0/quickstart_linux.pdf up to page 8, then get a licence from the Gurobi website. 
You can then install the R package contained in the tar file using a simple \code{install.packages}.