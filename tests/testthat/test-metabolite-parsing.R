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

test_that('works correctly with duplicated metabolite', {
  
  normal_example <- reactiontbl_to_expanded(dplyr::data_frame(
    abbreviation = c('one', 'two'),
    equation = c('2 alpha -> beta', 'gamma -> 2 delta'),
    lowbnd = c(-1,-1),
    uppbnd = c(3,3),
    obj_coef = c(1,0)))
  
  duplicated_example <- reactiontbl_to_expanded(dplyr::data_frame(
    abbreviation = c('one', 'two'),
    equation = c('alpha + alpha -> beta', 'gamma -> delta + delta'),
    lowbnd = c(-1,-1),
    uppbnd = c(3,3),
    obj_coef = c(1,0)))
  
  expect_equal(duplicated_example$mets %>% arrange(met),
               normal_example$mets %>% arrange(met))
  
  expect_equal(duplicated_example$stoich %>% arrange(abbreviation, met),
               normal_example$stoich %>% arrange(abbreviation, met))
})