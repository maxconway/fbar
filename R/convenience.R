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
#' @import assertthat
find_fluxes_vector <- function(abbreviation, equation, lowbnd, uppbnd, obj_coef, do_minimization=TRUE){
  mod1 <- data_frame(abbreviation, equation, lowbnd, uppbnd, obj_coef) %>%
    parse_reaction_table()
  
  res1 <- gurobi::gurobi(mod1, params = list(OutputFlag=0))
  
  if(!('x' %in% names(res1))){
    warning('optimization failed')
    return(0)
  }
  
  if(!do_minimization){
    return(res1$x)
  }
  
  mod2 <- mod1
  flux <- res1$x
  mod2$lb[flux > 0] <- 0
  mod2$ub[flux < 0] <- 0
  mod2$lb[mod1$obj > 0] <- flux[mod1$obj > 0]
  mod2$ub[mod1$obj < 0] <- flux[mod1$obj < 0]
  mod2$obj <- -sign(flux)
  mod2$obj[mod1$obj != 0] <- 0
  
  res2 <- gurobi::gurobi(mod2, params = list(OutputFlag=0))
  assert_that(res2 %has_name% 'x')
  
  return(res2$x)
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
find_fluxes_df <- function(reaction_table, do_minimization=TRUE){
  reaction_table %>%
    mutate(flux = find_fluxes_vector(abbreviation=abbreviation, 
                                     equation=equation, lowbnd=lowbnd, 
                                     uppbnd=uppbnd, 
                                     obj_coef=obj_coef,
                                     do_minimization=do_minimization
                                     ))
}


#' Given a metabolic model as a data frame, return a new data frame with fluxes and variability
#' 
#' This function calculates fluxes 10 times with shuffled versions of the metabolic model.
#' 
#' @param reaction_table a data frame representing the metabolic model
#' 
#' @return reaction_table with two added columns: sd (the standard deviation of fluxes found) and flux (a typical flux) from this distribution
#' 
#' @export
#' @import dplyr
#' @import gurobi
find_flux_variability_df <- function(reaction_table, do_minimization=TRUE){
  fluxdf <- data_frame(index=1:10, data = map(index, function(x){reaction_table})) %>%
    mutate(data = map(data, sample_frac)) %>%
    mutate(data = map(data, find_fluxes_df, do_minimization=do_minimization)) %>%
    mutate(data = map(data, arrange_, 'abbreviation')) %>%
    unnest() %>%
    select(abbreviation, flux) %>%
    group_by(abbreviation) %>%
    summarise(sd = sd(flux, na.rm=TRUE), flux = first(flux))
  
  inner_join(reaction_table, fluxdf, by='abbreviation')
}