# Model compartment utilities

list_compartments <- function(model){
  model$equation %>%
    str_extract_all('\\[\\w+?][ $]|^\\[\\w+?]: ') %>%
    flatten %>%
    str_replace_all('\\[|] ','') %>%
    unique
}

list_metabolites <- function(model){
  model$equation %>%
    str_split('[[:space:]]*[+<=>]+[[:space:]]*') %>%
    flatten %>%
    str_replace('\\[\\w+]$', '') %>%
    str_replace('^[0-9]+(\\.[0-9]+)? ', '') %>%
    unique
}

list_metabolites_withcompartment <- function(model){
  model$equation %>%
    str_split('[[:space:]]*[+<=>]+[[:space:]]*') %>%
    flatten %>%
    str_replace('^[0-9]+(\\.[0-9]+)? ', '') %>%
    unique
}

alter_compartment <- function(model, from, to){
  assert_that(length(from)==1,length(to)==1)
  mutate(model, equation = equation %>%
           str_replace_all(fixed(paste0('[',from,']')),paste0('[',to,']'))
         )
}

alter_compartments <- function(model, from, to){
  assert_that(length(from)==length(to) | length(from)==1 | length(to)==1)
  alterations <- data.frame(from, to, stringsAsFactors = FALSE)
  for(i in 1:nrow(alterations)){
    model <- alter_compartment(model, from = alterations[i,'from'], to=alterations[i,'to'])
  }
  return(model)
}

defactor_compartments <- function(model, name){
  compartments <- list_compartments(model)
  newcompartments <- paste0(name,'_',compartments)
  alter_compartments(model, from=compartments, to=newcompartments) %>%
    mutate(abbreviation = paste(name, abbreviation))
}

