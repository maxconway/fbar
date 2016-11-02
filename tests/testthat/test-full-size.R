context("full-size")
data("iJO1366")

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("find_fluxes_vector and find_fluxes_df produce same results", {
  v <- find_fluxes_vector(abbreviation = iJO1366$abbreviation,
                          equation = iJO1366$equation,
                          lowbnd = iJO1366$lowbnd,
                          uppbnd = iJO1366$uppbnd,
                          obj_coef = iJO1366$obj_coef)
  
  d <- find_fluxes_df(iJO1366)
  
  expect_equal(d$flux, v)
})

test_that("find_fluxes_vector works in grouped context", {
  d <- find_fluxes_df(iJO1366)
  
  library(dplyr)
  g <- purrr::map_df(1:10, function(x){iJO1366}, .id='sample') %>%
    group_by(sample) %>%
    mutate(flux = find_fluxes_vector(abbreviation, equation, lowbnd, uppbnd, obj_coef)) %>%
    ungroup %>%
    filter(sample=='3') %>%
    select(-sample)
  
  expect_equal(g, d)
})
