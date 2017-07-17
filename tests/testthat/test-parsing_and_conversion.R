context("parsing_and_conversion")
suppressMessages(library(Matrix))
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

test_that('reactiontbl_to_expanded is opposite of expanded_to_reactiontbl', {
  a <- reactiontbl_to_expanded(ecoli_core)
  b <- expanded_to_reactiontbl(a)
  
  expect_equal(nrow(b), nrow(ecoli_core))
  expect_named(b, names(ecoli_core), ignore.order = TRUE)
  expect_equal(ecoli_core %>% arrange(abbreviation) %>% select(-equation),
               b %>% arrange(abbreviation) %>% select(-equation))
})
