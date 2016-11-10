context('toy models')
library(gurobi)

test_that('toy model 1', {
  skip_if_not_installed('gurobi')
  
  model <- parse_reaction_table(dplyr::data_frame(
    abbreviation = c('one', 'two'),
    equation = c('alpha -> beta', 'gamma -> delta'),
    lowbnd = c(-1,-1),
    uppbnd = c(3,3),
    obj_coef = c(1,0)))
  
  result <- gurobi(model)
  expect_equal(result$status, 'OPTIMAL')
  expect_equal(result$objval, 0)
})

test_that('toy model 2', {
  skip_if_not_installed('gurobi')
  
  model <- parse_reaction_table(dplyr::data_frame(
    abbreviation = c('one', 'two'),
    equation = c('alpha -> ', ' <-> alpha'),
    lowbnd = c(-1,-1),
    uppbnd = c(3,3),
    obj_coef = c(1,0)))
  
  result <- gurobi(model)
  expect_equal(result$status, 'OPTIMAL')
  expect_equal(result$objval, 3)
})