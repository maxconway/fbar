# Model compartment utilities

#'
#' @import stringr
#' @import dplyr
list_compartments <- function(model){
  model$equation %>%
    str_extract_all('\\[\\w+?][ $]|^\\[\\w+?]: ') %>%
    flatten %>%
    str_replace_all('\\[|] ','') %>%
    unique
}

alter_compartment <- function(model, from, to){
  assert_that(length(from)==1,length(to)==1)
  mutate(model, equation = equation %>%
           str_replace_all(fixed(paste0('[',from,']')),paste0('[',to,']'))
         )
}

alter_comparments <- function(model, from, to){
  assert_that(length(from)==length(to) | length(from)==1 | length(to)==1)
  alterations <- data.frame(from, to, stringsAsFactors = FALSE)
  for(i in 1:nrow(alterations)){
    model <- alter_compartments(model, from = alterations[i,'from'], to=alterations[i,'to'])
  }
  return(model)
}

defactor_compartments <- function(model, name){
  compartments <- list_compartments(model)
  newcompartments <- paste0(name,'_',compartments)
  alter_comparments(model, from=compartments, to=newcompartments)
}