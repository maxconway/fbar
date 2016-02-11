context("Gene Expression Processing")
library(dplyr)
library(purrr)
library(stringr)

test_that("works with missing values, boolean", {
  genes <- letters[1:2]
  presences <- c(T,F)
  expressions <- c('a','','b')
  expect_equal(gene_eval_boolean(expressions, genes, presences), c(T,T,F))
})

test_that("works with single values, boolean", {
  genes <- letters[1:3]
  presences <- c(T,T,F)
  expressions <- letters[c(1,2,3,1)]
  expect_equal(gene_eval_boolean(expressions, genes, presences), c(T,T,F,T))
})

test_that("works with double value expressions, boolean", {
  genes <- letters[1:3]
  presences <- c(T,T,F)
  expressions <- c('a & b', 'b | a', 'b & c', 'c | b')
  expect_equal(gene_eval_boolean(expressions, genes, presences), c(T,T,F,T))
})

test_that("works with multi value expressions, boolean", {
  genes <- letters[1:3]
  presences <- c(T,T,F)
  expressions <- c('(a & b) | c', '(b | a) & c', '(b & c) | b', 'c & (c | b)')
  expect_equal(gene_eval_boolean(expressions, genes, presences), c(T,F,T,F))
})

test_that('full test', {
  model <- readr::read_tsv('inst/extdata/iJO1366.tsv', na='NA') %>%
    mutate(geneAssociation = `Gene-Reaction Association` %>%
             str_replace_all(fixed('or'),'|') %>%
             str_replace_all(fixed('and'),'&'))
  genes <- data_frame(name = str_extract_all(model$geneAssociation, '[[:alpha:]][0-9]{4}') %>%
    flatten_chr() %>%
    discard(is.na)
  ) %>%
    mutate(presence = runif(n())<0.05)
  gene_associate(model, genes)
})
