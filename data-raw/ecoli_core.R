# Create ecoli_core dataset

ecoli_core <- get_BiGG('http://bigg.ucsd.edu/static/models/e_coli_core.json') %>%
  expanded_to_reactiontbl %>%
  as_tibble %>%
  select(abbreviation, lowbnd, uppbnd, obj_coef, equation, officialName, geneAssociation, subsystem)

devtools::use_data(ecoli_core)
readr::write_tsv(ecoli_core, 'inst/extdata/ecoli_core.tsv')
