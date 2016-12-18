context("parsing_and_conversion")
library(Matrix)
suppressMessages(library(tidyverse))

data("ecoli_core")

test_that("reactiontbl_to_expanded works on ecoli_core", {
  a <- reactiontbl_to_expanded(ecoli_core)
  
  expect_equal(length(a),3)
  expect_named(a, c('rxns','mets','stoich'), ignore.order = TRUE)
  expect_equal(nrow(a$stoich), 360)
  expect_equal(nrow(a$rxns), nrow(ecoli_core))
  expect_equal(nrow(a$mets), 72)
})

test_that("expanded_to_gurobi works on ecoli_core", {
  a <- reactiontbl_to_expanded(ecoli_core)
  
  b <- expanded_to_gurobi(a)
  expect_named(b, c("A", "obj", "sense", "rhs", "lb", "ub", "modelsense"), ignore.order = TRUE)
  expect_equal(b$sense, '=')
  expect_equal(b$rhs, 0)
  expect_true(setequal(b$lb, c(0, -1000, 8.39, -10)))
  expect_equal(length(b$lb), 95)
  expect_true(setequal(b$ub, 1000))
  expect_equal(length(b$ub), 95)
  expect_equal(sort(unname(rowSums(b$A))), c(-67.81, -66.81, -17, -10.547, -9.0279, -3.7478, -2.9414, -2.8977, 
                                             -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, 
                                             -2, -2, -1.7867, -1.5191, -1.496, -1.205, -1, -1, -1, -1, -1, 
                                             -0.361, -0.2557, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                             0, 0.5, 1, 1, 1, 1.1672, 1.7478, 1.871, 2, 2, 2, 2, 3.9291, 5.1182, 
                                             7, 9.0279, 10.547, 64.81, 67.81, 88.81))
  expect_equal(sort(unname(colSums(b$A))), c(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
                                             -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 0, 0, 0, 0, 0, 
                                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                             0.5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 
                                             50.4329))
})

test_that('reactiontbl_to_gurobi works on ecoli_core', {
  b <- reactiontbl_to_gurobi(ecoli_core)
  expect_named(b, c("A", "obj", "sense", "rhs", "lb", "ub", "modelsense"), ignore.order = TRUE)
  expect_equal(b$sense, '=')
  expect_equal(b$rhs, 0)
  expect_true(setequal(b$lb, c(0, -1000, 8.39, -10)))
  expect_equal(length(b$lb), 95)
  expect_true(setequal(b$ub, 1000))
  expect_equal(length(b$ub), 95)
  expect_equal(sort(unname(rowSums(b$A))), c(-67.81, -66.81, -17, -10.547, -9.0279, -3.7478, -2.9414, -2.8977, 
                                             -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, 
                                             -2, -2, -1.7867, -1.5191, -1.496, -1.205, -1, -1, -1, -1, -1, 
                                             -0.361, -0.2557, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                             0, 0.5, 1, 1, 1, 1.1672, 1.7478, 1.871, 2, 2, 2, 2, 3.9291, 5.1182, 
                                             7, 9.0279, 10.547, 64.81, 67.81, 88.81))
  expect_equal(sort(unname(colSums(b$A))), c(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
                                             -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 0, 0, 0, 0, 0, 
                                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                             0.5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 
                                             50.4329))
})
