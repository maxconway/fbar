#' Decompose a metabolite table into the metabolite stub itself and the compartment it is in
#' 
#' @param met_table A metabolite table, with one column, \code{met}
#' @param compartment_regex Regular expression to identify compartments in model
#' 
#' @return a metabolite table with the columns \code{chemical} and \code{compartment}
#' 
#' @import dplyr
#' @importFrom rlang .data
#' @export
#' 
#' @examples
#' data(ecoli_core)
#' 
#' mod <- reactiontbl_to_expanded(ecoli_core)
#' 
#' decompose_metabolites(mod$mets)
#' 
#' recompose_metabolites(decompose_metabolites(mod$mets))
decompose_metabolites <- function(met_table, compartment_regex = '(\\[[a-zA-Z0-9]+]$)|(_[a-zA-Z]$)'){
  met_table %>%
    transmute(chemical = stringr::str_replace(.data$met, compartment_regex, ''),
              compartment = stringr::str_extract(.data$met, compartment_regex) %>% 
                dplyr::coalesce('') %>%
                stringr::str_replace_all('\\[|]|_', ''))
}

#' Merge metabolite stub and compartment to form an id
#' 
#' @param expanded_metabolites a metabolite table as created by \code{\link{decompose_metabolites}}
#' @param before_signifier a string that is inserted before the compartment identifier
#' @param after_signifier a string that is inserted after the compartment identifier
#' 
#' @return A merged metabolite table with one column, \code{met}
#' 
#' @import dplyr
#' @export
#' 
#' @examples 
#' data(ecoli_core)
#' 
#' mod <- reactiontbl_to_expanded(ecoli_core)
#' 
#' decompose_metabolites(mod$mets)
#' 
#' recompose_metabolites(decompose_metabolites(mod$mets))
recompose_metabolites <- function(expanded_metabolites, before_signifier = '_', after_signifier = ''){
  expanded_metabolites %>%
    transmute(met = stringr::str_c(.data$chemical, before_signifier, .data$compartment, after_signifier))
}