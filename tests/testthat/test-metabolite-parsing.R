context("metabolite-parsing")
suppressMessages(library(tidyverse))

test_that("tricky equations split correctly", {
  tricky_equations <- c("peptidylproline (omega=180) <=> peptidylproline (omega=0)",
                        "2 o2.- + 2 h+ <=> hydrogen peroxide + oxygen",
                        "beta-d-galactosyl-(1->4)-beta-d-glucosyl-(1<->1)-ceramide + h2o <=> beta-d-glucosyl-(1<->1)-ceramide + d-galactose")
  fbar:::split_on_arrow(tricky_equations, regex_arrow = '[[:space:]]*<?[=]+>[[:space:]]*')
})