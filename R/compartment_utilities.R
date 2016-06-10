# Model compartment utilities
# maybe rewrite with a metabolite data frame

validate_model_format <- function(model){
  assert_that(is.data.frame(model))
  return(model)
}

get_invalid_equations <- function(model){
  validate_model_format(model)
  model %>%
    mutate(direction_indicators = str_count(equation, '[<=>]+'),
           start_compartment = str_detect(equation,'^\\[\\w+?]: '),
           right_square = str_count(equation, ']'),
           left_square = str_count(equation, '\\['),
           plus_splits = str_count(equation,'[[:space:]]+[+][[:space:]]+')) %>%
    filter(!(
      direction_indicators==1 &
        right_square==left_square &
        ifelse(start_compartment, right_square==1, TRUE) &
        ifelse(start_compartment, TRUE, plus_splits < right_square)
    ))
}

validate_model_equations <- function(model){
  
  assert_that(nrow(get_invalid_equations(model))==0)
  return(model)
}

list_compartments <- function(model){
  validate_model_format(model)
  model$equation %>%
    str_extract_all('\\[\\w+?]$|\\[\\w+?][[:space:]]+|^\\[\\w+?]: ') %>%
    flatten %>%
    str_trim %>%
    str_replace_all('\\[|]','') %>%
    str_trim %>%
    unique
}

list_metabolites <- function(model){
  validate_model_format(model)
  model$equation %>%
    str_split('[[:space:]]*[<=>]+[[:space:]]*|[[:space:]]+[+][[:space:]]+') %>%
    flatten %>%
    str_replace('\\[\\w+]$', '') %>%
    str_replace('^[0-9]+(\\.[0-9]+)? ', '') %>%
    unique
}

list_metabolites_with_compartment <- function(model){
  validate_model_format(model)
  model$equation %>%
    str_split('[[:space:]]*[<=>]+[[:space:]]*|[[:space:]]+[+][[:space:]]+') %>%
    flatten %>%
    str_replace('^[0-9]+(\\.[0-9]+)? ', '') %>%
    unique
}

alter_compartment <- function(model, from, to){
  validate_model_format(model)
  assert_that(length(from)==1,length(to)==1)
  mutate(model, equation = equation %>%
           str_replace_all(fixed(paste0('[',from,']')),paste0('[',to,']'))
         )
}

alter_compartments <- function(model, from, to){
  validate_model_format(model)
  assert_that(length(from)==length(to) | length(from)==1 | length(to)==1)
  alterations <- data.frame(from, to, stringsAsFactors = FALSE)
  for(i in 1:nrow(alterations)){
    model <- alter_compartment(model, from = alterations[i,'from'], to=alterations[i,'to'])
  }
  return(model)
}

defactor_compartments <- function(model, name){
  validate_model_format(model)
  compartments <- list_compartments(model)
  newcompartments <- paste0(name,'_',compartments)
  alter_compartments(model, from=compartments, to=newcompartments) %>%
    mutate(abbreviation = paste0(name, '_', abbreviation))
}

list_metabolites_in_compartment <- function(model, compartment){
  validate_model_format(model)
  model %>% 
    list_metabolites_with_compartment %>%
    keep(str_detect, fixed(paste0('[',compartment,']')))
}

wrap_exchanges <- function(model, surrounding_compartment){
  validate_model_format(model)
  outbound <- model %>%
    filter(str_detect(equation, '>[[:space:]]*$'))
  
  inbound <- model %>%
    filter(str_detect(equation, '^[[:space:]]*[=<]'))
  
  unaffected <- model %>%
    filter(!str_detect(equation, '>[[:space:]]*$'),!str_detect(equation, '^[[:space:]]*[=<]'))
  
  outbound_other_side <- outbound %>%
    select(equation) %>%
    alter_compartments(list_compartments(outbound), surrounding_compartment) %>%
    mutate(equation = str_replace(equation, '[[:space:]]*[<=>]+[[:space:]]*',''))
  
  inbound_other_side <- inbound %>%
    select(equation) %>%
    alter_compartments(list_compartments(inbound), surrounding_compartment) %>%
    mutate(equation = str_replace(equation, '[[:space:]]*[<=>]+[[:space:]]*',''))
  
  bind_rows(unaffected, 
            outbound %>% mutate(equation = str_c(equation, ' ', outbound_other_side$equation)),
            inbound %>% mutate(equation = str_c(inbound_other_side$equation, ' ', equation)))
}

add_exchange_reactions <- function(model, exchange_compartment){
  validate_model_format(model)
  metabolites <- list_metabolites_in_compartment(model, exchange_compartment)
  
  bind_rows(model,
    data_frame(abbreviation = str_c('EX_',metabolites),
               equation = str_c(metabolites, ' <=>'),
               lowbnd = -1000,
               uppbnd = 1000,
               obj_coef = 0)
  )
}

