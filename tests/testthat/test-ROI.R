context("ROI")

test_that('toy model 1', {
  skip_if_not_installed('ROI')
  library(ROI)
  
  expanded <- reactiontbl_to_expanded(dplyr::data_frame(
    abbreviation = c('one', 'two'),
    equation = c('alpha -> beta', 'gamma -> delta'),
    lowbnd = c(-1,-1),
    uppbnd = c(3,3),
    obj_coef = c(1,0)))
  
  model <- expanded_to_ROI(expanded)
  
  result <- ROI_solve(model)
  
  expect_equal(result$status$code, 0)
  expect_equal(result$objval, 0)
})

test_that('toy model 2', {
  skip_if_not_installed('ROI')
  library(ROI)
  
  expanded <- reactiontbl_to_expanded(dplyr::data_frame(
    abbreviation = c('one', 'two'),
    equation = c('alpha -> ', ' <-> alpha'),
    lowbnd = c(-1,-1),
    uppbnd = c(3,3),
    obj_coef = c(1,0)))
  
  model <- expanded_to_ROI(expanded)
  
  result <- ROI_solve(model)
  model <- expanded_to_ROI(expanded)
  
  expect_equal(result$status$code, 0)
  expect_equal(result$objval, 3)
})