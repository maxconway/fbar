context("metabolite-parsing")
suppressMessages(library(tidyverse))
data("ecoli_core")

test_that("tricky equations split correctly", {
  tricky_equations <- c("peptidylproline (omega=180) <=> peptidylproline (omega=0)",
                        "2 o2.- + 2 h+ <=> hydrogen peroxide + oxygen",
                        "beta-d-galactosyl-(1->4)-beta-d-glucosyl-(1<->1)-ceramide + h2o <=> beta-d-glucosyl-(1<->1)-ceramide + d-galactose")
  fbar:::split_on_arrow(tricky_equations, regex_arrow = '[[:space:]]*<?[=]+>[[:space:]]*')
})

test_that('metabolites are decomposed and recomposed correctly', {
  mod <- ecoli_core %>%
    reactiontbl_to_expanded()

  expect_equal(mod$mets %>%
                 decompose_metabolites() %>%
                 recompose_metabolites() %>%
                 arrange(met),
               mod$mets %>%
                 arrange(met)
               )
})