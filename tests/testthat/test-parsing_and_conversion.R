context("parsing_and_conversion")
library(Matrix)

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
  expect_equal(unname(rowSums(b$A)), c(2, -2, -1.496, 0, 0, 0, -2, 1, -2, -3.7478, 0, 2, 67.81, 5.1182, 
                                       -2, 0, -67.81, 0, 7, -2, 1.7478, 0, -0.361, 0, -2, 3.9291, -1, 
                                       1, -1, -2, 0, -2, 1.871, -1.205, -2, -0.2557, -2, -2.9414, -2, 
                                       0, -66.81, -2, 88.81, -17, -1, 0, -2, 0, -2, -10.547, 10.547, 
                                       9.0279, -9.0279, 2, -2, 0.5, -2, -1.7867, -1.5191, 64.81, -2, 
                                       1.1672, -2, 0, 0, -2.8977, 1, 0, 0, -1, 2, -1))
  expect_equal(unname(colSums(b$A)), c(0.5, 0, 0, 0, -1, 50.4329, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, -1, 
                                       -1, 0, -1, 0, 0, 1, 1, 2, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 1, -1, 
                                       0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, -1, -1, 
                                       -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
                                       -1, -1, 0, 0, -1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                       0, 0))
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
  expect_equal(unname(rowSums(b$A)), c(2, -2, -1.496, 0, 0, 0, -2, 1, -2, -3.7478, 0, 2, 67.81, 5.1182, 
                                       -2, 0, -67.81, 0, 7, -2, 1.7478, 0, -0.361, 0, -2, 3.9291, -1, 
                                       1, -1, -2, 0, -2, 1.871, -1.205, -2, -0.2557, -2, -2.9414, -2, 
                                       0, -66.81, -2, 88.81, -17, -1, 0, -2, 0, -2, -10.547, 10.547, 
                                       9.0279, -9.0279, 2, -2, 0.5, -2, -1.7867, -1.5191, 64.81, -2, 
                                       1.1672, -2, 0, 0, -2.8977, 1, 0, 0, -1, 2, -1))
  expect_equal(unname(colSums(b$A)), c(0.5, 0, 0, 0, -1, 50.4329, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, -1, 
                                       -1, 0, -1, 0, 0, 1, 1, 2, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 1, -1, 
                                       0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, -1, -1, 
                                       -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
                                       -1, -1, 0, 0, -1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                       0, 0))
})
