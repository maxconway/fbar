# Introduction
A simple, easy to use Flux Balance Analysis package with a tidy data approach. Just `data_frames` and the occasional `list`, no new classes to learn. The focus is on simplicity and speed. Models are expected as a flat table, and results can be simply appended to the table. This makes this package very suitable for useage in pipelines with pre- and post- processing of models and results, so that it works well as a backbone for customized methods. Loading, parsing and evaluating a model takes around 0.1s, which, together with the straightforward data structures used, makes this library very suitable for large parameter sweeps.

## Example
```{r}
library(FluxBalanceAnalyzeR)
library(gurobi)
data(iJO1366)
model <- parse_reaction_table(iJO1366)
result <- gurobi(model)
iJO1366$flux <- result$x
```

## Installation
```{r, eval=FALSE}
devtools::install_github('maxconway/FluxBalanceAnalyzeR')
```

### Gurobi
At present, FluxBalanceAnalyzeR only works with Gurobi. Gurobi is fast, relatively easy to install, and free for academics, but other optimizers are on the todo list.
To install Gurobi for R on Linux, you need to download a tar file from the website, and follow instructions from http://www.gurobi.com/documentation/6.0/quickstart_linux.pdf up to page 8, then get a licence from the Gurobi website. 
You can then install the R package contained in the tar file using a simple `r install.packages}`.