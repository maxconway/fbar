context("Gene Expression Processing")


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
