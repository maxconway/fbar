context("convenience")

data("ecoli_core")

test_that("find_fluxes_df gives sensible result with minimization", {
  evaluated <- find_fluxes_df(ecoli_core)
  
  expect_equal(evaluated %>% select(-flux), ecoli_core)
  expect_true(all(evaluated$flux <= (ecoli_core$uppbnd + 1e-6)))
  expect_true(all(evaluated$flux >= (ecoli_core$lowbnd - 1e-6)))
})

test_that("find_fluxes_df gives sensible result without minimization", {
  evaluated <- find_fluxes_df(ecoli_core, do_minimization = FALSE)
  
  expect_equal(evaluated %>% select(-flux), ecoli_core)
  expect_true(all(evaluated$flux <= (ecoli_core$uppbnd + 1e-6)))
  expect_true(all(evaluated$flux >= (ecoli_core$lowbnd - 1e-6)))
})
