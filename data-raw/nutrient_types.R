# Create nutrient types dataset

nutrient_types <- read_tsv('inst/extdata/nutrient_types.tsv')

devtools::use_data(nutrient_types)
  