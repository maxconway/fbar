#' Decompose a metabolite table into the metabolite stub itself and the compartment it is in
#' 
#' @param met_table A metabolite table, with one column, \code{met}
#' 
#' @result a metabolite table with the columns \code{chemical} and \code{compartment}
#' 
#' @import dplyr
#' @export
#' 
#' @example
#' data(ecoli_core)
#' 
#' mod <- ecoli_core %>%
#'   reactiontbl_to_expanded()
#' 
#' mod$mets %>% 
#'   decompose_metabolites()
#' 
#' mod$mets %>% 
#'   decompose_metabolites() %>%
#'   recompose_metabolites()
decompose_metabolites <- function(met_table){
  met_table %>%
    transmute(chemical = stringr::str_replace(met, '\\[[a-zA-Z0-9]+]$', ''),
              compartment = stringr::str_extract(met, '\\[[a-zA-Z0-9]+]$') %>% 
                dplyr::coalesce('') %>%
                stringr::str_replace_all('\\[|]', ''))
}

#' Merge metabolite stub and compartment to form an id
#' 
#' @param expanded_metabolites a metabolite table as created by \link{\code{decompose_metabolites}}
#' 
#' @result a merged metabolite table with one column, \code{met}
#' 
#' @import dplyr
#' @export
#' 
#' @example 
#' data(ecoli_core)
#' 
#' mod <- ecoli_core %>%
#'   reactiontbl_to_expanded()
#' 
#' mod$mets %>% 
#'   decompose_metabolites()
#' 
#' mod$mets %>% 
#'   decompose_metabolites() %>%
#'   recompose_metabolites()
recompose_metabolites <- function(expanded_metabolites){
  expanded_metabolites %>%
    transmute(met = stringr::str_c(chemical, '[', compartment, ']'))
}