context('normal usage')

test_that('parse_SBML does not throw errors', {
  data(models)
  models %>% map(parse_SBML2metmod)
})

test_that('metmod2gurobi does not throw errors', {
  data(models)
  models %>% map(parse_SBML2metmod) %>% map(metmod2gurobi) %>% map(gurobi)
})