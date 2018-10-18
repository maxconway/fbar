#' Download a model from a BiGG json file
#' 
#' @param address An address to download from
#' 
#' @return A model in expanded format
#' 
#' @export
#' 
#' @importFrom rlang .data
#' @import dplyr
get_BiGG <- function(address){
  if(!requireNamespace('jsonlite', quietly = TRUE)){
    stop('This function requires jsonlite. Please install it before continuing')
  }
  
  
  json_mod <- jsonlite::fromJSON(address)
  
  res <- list()
  
  res$rxns <- json_mod$reactions %>%
    purrr::keep(is.vector) %>%
    rename(abbreviation = .data$id,
           officialName = .data$name,
           lowbnd = .data$lower_bound,
           uppbnd = .data$upper_bound,
           geneAssociation = .data$gene_reaction_rule
    ) %>%
    mutate(obj_coef = coalesce(.data$objective_coefficient, 0))
  
  res$mets <- json_mod$metabolites %>%
    select(met = id)
  
  res$stoich <- json_mod$reactions$metabolites %>%
    mutate(abbreviation = json_mod$reactions$id) %>%
    tidyr::gather(key = 'met', value = 'stoich', -.data$abbreviation, na.rm=TRUE)
  
  return(res)
  
}

#' Validate an expanded model
#' 
#' @param reactions_expanded the expanded model to check
#' 
#' @return TRUE
#' 
#' @import assertthat 
validate_expanded <- function(reactions_expanded){
  
  assert_that(
    'data.frame' %in% class(reactions_expanded$rxns),
    reactions_expanded$rxns %has_name% 'abbreviation',
    reactions_expanded$rxns %has_name% 'uppbnd',
    reactions_expanded$rxns %has_name% 'lowbnd',
    reactions_expanded$rxns %has_name% 'obj_coef',
    setequal(reactions_expanded$stoich$abbreviation, reactions_expanded$rxns$abbreviation),
    setequal(reactions_expanded$stoich$met, reactions_expanded$mets$met)
  )
  
  return(TRUE)
}