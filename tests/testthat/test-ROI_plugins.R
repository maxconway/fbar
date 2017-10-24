context("ROI_plugins")
library(tidyverse)

data("ecoli_core")
data("iJO1366")

suppressMessages(
  walk(
    setdiff(
      intersect(
        ROI::ROI_registered_solvers(),
        ROI::ROI_installed_solvers()
      ), 
      "ROI.plugin.nlminb"
    ), 
    requireNamespace)
)

test_that("same results on ecoli_core", {

  mod <- ecoli_core %>%
    reactiontbl_to_expanded() %>%
    expanded_to_ROI()
  
  ROI::ROI_applicable_solvers(mod)%>%
    map(safely(~ ROI::ROI_solve(mod, solver=.))) %>%
    walk(~ expect_null(.$error)) %>% 
    keep(~is.null(.$error)) %>%
    map('result')%>%
    map(ROI::solution) %>%
    walk(expect_length, n=95) %>%
    walk(~ expect_true(all(is.finite(.))))
    
})

test_that("same results on iJO1366", {

    mod <- iJO1366 %>%
    reactiontbl_to_expanded() %>%
    expanded_to_ROI()
  
  ROI::ROI_applicable_solvers(mod)%>%
    map(safely(~ ROI::ROI_solve(mod, solver=.))) %>%
    walk(~ expect_null(.$error)) %>% 
    keep(~is.null(.$error)) %>%
    map('result')%>%
    map(ROI::solution) %>%
    walk(expect_length, n=2583) %>%
    walk(~ expect_true(all(is.finite(.))))
  
})
