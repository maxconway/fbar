# Create iJO1366 dataset

iJO1366 <- get_BiGG('http://bigg.ucsd.edu/static/models/iJO1366.json') %>%
  expanded_to_reactiontbl %>%
  as_tibble %>%
  select(abbreviation, lowbnd, uppbnd, obj_coef, equation, officialName, geneAssociation, subsystem)

devtools::use_data(iJO1366)
readr::write_tsv(iJO1366, 'inst/extdata/iJO1366.tsv')
  