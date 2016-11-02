#' Given a metabolic model as a set of vectors, return the flux distribution
#' 
#' 
#' @param abbreviation unique reaction names
#' @param equation stoichiometric equations of reactions
#' @param lowbnd minimum reaction rate
#' @param uppbnd maximum reaction rate
#' @param obj_coef controls which reactions are maximized or minimised
#' 
#' @seealso find_fluxes_vector
#' 
#' @export
#' @import dplyr
#' @import gurobi
find_fluxes_vector <- function(abbreviation, equation, lowbnd, uppbnd, obj_coef){
  res <- data_frame(abbreviation, equation, lowbnd, uppbnd, obj_coef) %>%
    parse_reaction_table() %>%
    gurobi::gurobi(params = list(OutputFlag=0))
  
  if(!('x' %in% names(res))){
    warning('optimization failed')
    return(0)
  }else{
    return(res$x)
  }
}

#' Given a metabolic model as a data frame, return a new data frame with fluxes
#' 
#' This function is a wrapper round \code{\link{find_fluxes_vector}}
#' 
#' @param reaction_table a data frame representing the metabolic model
#' 
#' @seealso find_fluxes_vector
#' 
#' @export
#' @import dplyr
#' @import gurobi
find_fluxes_df <- function(reaction_table){
  reaction_table %>%
    mutate(flux = find_fluxes_vector(abbreviation=abbreviation, 
                                     equation=equation, lowbnd=lowbnd, 
                                     uppbnd=uppbnd, 
                                     obj_coef=obj_coef
                                     ))
}