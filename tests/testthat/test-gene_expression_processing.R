context("Gene Expression Processing")

suppressMessages(library(tidyverse))
suppressMessages(library(ROI.plugin.ecos))

data("ecoli_core")
data("iJO1366")

test_that("works with missing values, boolean", {
  genes <- letters[1:2]
  presences <- c(T,F)
  expressions <- c('a','','b')
  expect_equal(fbar::gene_eval(expressions, genes, presences)==1, c(T,NA,F))
})

test_that("works with single values, boolean", {
  genes <- letters[1:3]
  presences <- c(T,T,F)
  expressions <- letters[c(1,2,3,1)]
  expect_equal(fbar::gene_eval(expressions, genes, presences)==1, c(T,T,F,T))
})

test_that("works with double value expressions, boolean", {
  genes <- letters[1:3]
  presences <- c(T,T,F)
  expressions <- c('a & b', 'b | a', 'b & c', 'c | b')
  expect_equal(fbar::gene_eval(expressions, genes, presences)==1, c(T,T,F,T))
})

test_that("works with multi value expressions, boolean", {
  genes <- letters[1:3]
  presences <- c(T,T,F)
  expressions <- c('(a & b) | c', '(b | a) & c', '(b & c) | b', 'c & (c | b)')
  expect_equal(fbar::gene_eval(expressions, genes, presences)==1, c(T,F,T,F))
})

test_that('full test', {
  genes <- data_frame(name = stringr::str_extract_all(ecoli_core$geneAssociation, '[[:alpha:]][0-9]{4}') %>%
    flatten_chr() %>%
    discard(is.na)
  ) %>%
    mutate(presence = runif(n())<0.05)
  res <- gene_associate(ecoli_core, genes)
  
  expect_true(all(is.finite(res$lowbnd)))
  expect_true(all(is.finite(res$uppbnd)))
})

test_that('works with IJO1366', {
  gene_table = data_frame(name = iJO1366$GRassoc %>%
                            stringr::str_split('and|or|\\s|\\(|\\)') %>%
                            purrr::flatten_chr() %>%
                            unique,
                          presence = 1) %>%
    filter(name != '', !is.na(name))
  
  res <- gene_associate(reaction_table = iJO1366 %>%
                   mutate(geneAssociation = GRassoc %>%
                            stringr::str_replace_all('and', '&') %>%
                            stringr::str_replace_all('or', '|')
                          ),
                 gene_table = gene_table
                 )
  
  expect_true(all(is.finite(res$lowbnd)))
  expect_true(all(is.finite(res$uppbnd)))
})