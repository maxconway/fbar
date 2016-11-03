context("full-size")
data("iJO1366")

test_models <- list(iJO1366 = iJO1366,
                    salmonella = read_tsv('../salmonella-mouse/data/LT2_react.tsv') %>%
                      select(abbreviation=`Rxn name`,
                             equation=Formula,
                             uppbnd=UB,
                             lowbnd=LB,
                             geneAssociation=`Gene-reaction association`,
                             Subsystem,
                             name = `Rxn description`
                      )%>%
                      mutate(obj_coef=1*(abbreviation=='biomass_iRR1083')) %>%
                      mutate(lowbnd = ifelse(abbreviation=='EX_skm(e)',-10,lowbnd)))

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
  
  g <- purrr::map_df(1:10, function(x){iJO1366}, .id='sample') %>%
    group_by(sample) %>%
    mutate(flux = find_fluxes_vector(abbreviation, equation, lowbnd, uppbnd, obj_coef)) %>%
    ungroup %>%
    filter(sample=='3') %>%
    select(-sample)
  
  expect_equal(g, d)
})

test_that("find_fluxes_df stable across shuffling", {
  skip('iJO1366 is not working')
  
  d1 <- iJO1366 %>% sample_frac() %>% find_fluxes_df() %>% arrange(abbreviation)
  d2 <- iJO1366 %>% sample_frac() %>% find_fluxes_df() %>% arrange(abbreviation)
  
  expect_equal(d1$flux, d2$flux)
})

test_that("find_flux_variability_df works", {
  skip('not working')

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
