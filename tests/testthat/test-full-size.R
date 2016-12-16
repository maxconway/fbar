context("full-size")
data("iJO1366")
library(tidyverse)

test_models <- list(iJO1366 = iJO1366)

test_that("find_fluxes_df works in grouped context", {
  skip_if_not_installed('ROI')
  library(ROI)
  
  d <- find_fluxes_df(iJO1366)$flux
  
  g <- purrr::map_df(1:10, function(x){iJO1366}, .id='sample') %>%
    group_by(sample) %>%
    tidyr::nest() %>%
    mutate(data = map(data, find_fluxes_df)) %>%
    tidyr::unnest() %>%
    ungroup %>%
    filter(sample=='3') %>%
    select(-sample) %>%
    getElement('flux')
  
  expect_equal(g, d)
})

test_that("find_fluxes_df stable across shuffling", {
  skip_if_not_installed('ROI')
  library(ROI)
  skip('known theoretical issue')
  
  d1 <- iJO1366 %>% sample_frac() %>% find_fluxes_df() %>% arrange(abbreviation)
  d2 <- iJO1366 %>% sample_frac() %>% find_fluxes_df() %>% arrange(abbreviation)
  
  expect_equal(d1$flux, d2$flux)
})

test_that("find_flux_variability_df works", {
  testthat::skip_if_not_installed('ROI')
  library(ROI)
  skip('known theoretical issue')

  for(rxns in test_models){
  v1 <- rxns %>% 
    find_flux_variability_df() %>%
    filter(sd > 0.25) %>%
    getElement('abbreviation')
  v2 <- rxns %>% 
    find_flux_variability_df() %>%
    filter(sd > 0.25) %>%
    getElement('abbreviation')
  
  expect_gte(length(intersect(v1,v2))/length(union(v1,v2)), 0.9)
  }
})
