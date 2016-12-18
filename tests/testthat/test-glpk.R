context("glpk")
suppressMessages(library(tidyverse))

test_that('toy model 1', {
  skip_if_not_installed('Rglpk')
  library(Rglpk)
  
  model <- expanded_to_glpk(reactiontbl_to_expanded(dplyr::data_frame(
    abbreviation = c('one', 'two'),
    equation = c('alpha -> beta', 'gamma -> delta'),
    lowbnd = c(-1,-1),
    uppbnd = c(3,3),
    obj_coef = c(1,0))))
  
  result <- purrr::lift_dl(Rglpk_solve_LP)(model)
  
  expect_equal(result$status, 0)
  expect_equal(result$optimum, 0)
})

test_that('toy model 2', {
  skip_if_not_installed('Rglpk')
  library(Rglpk)
  
  model <- expanded_to_glpk(reactiontbl_to_expanded(dplyr::data_frame(
    abbreviation = c('one', 'two'),
    equation = c('alpha -> ', ' <-> alpha'),
    lowbnd = c(-1,-1),
    uppbnd = c(3,3),
    obj_coef = c(1,0))))
  
  result <- purrr::lift_dl(Rglpk_solve_LP)(model)
  expect_equal(result$status, 0)
  expect_equal(result$optimum, 3)
})
