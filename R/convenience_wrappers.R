#' Given a metabolic model as a data frame, return a new data frame with fluxes
#' 
#' @details 
#' This function uses ROI, so to solve models, you will need a solver plugin for ROI. Probably the easiest one to install is ROI.plugin.glpk.
#' To install this in Linux, run \code{sudo apt-get install libglpk-dev} in a terminal, and then run \code{install.packages('ROI.plugin.glpk')} in R.
#'
#' 
#' @param reaction_table a data frame representing the metabolic model
#' @param do_minimization toggle to uniformly minimize all non-objective fluxes after finding the objective
#' 
#' @return The input data frame with a new numeric column, "\code{flux}".
#' 
#' @seealso find_fluxes_vector
#' 
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @import dplyr
#' @import ROI.plugin.ecos
#' 
#' @examples 
#' \dontrun{
#' data(ecoli_core)
#' ecoli_core_with_flux <- find_fluxes_df(ecoli_core)
#' }
find_fluxes_df <- function(reaction_table, do_minimization=FALSE){
  mod1 <- reaction_table %>%
    reactiontbl_to_expanded() %>%
    expanded_to_ROI()
  
  if(length(ROI_applicable_solvers(mod1))<1){
    stop("ROI found no suitable solver. The documentation includes instructions on how to install one.")
  }
  
  res1 <- ROI::ROI_solve(mod1)
  
  if(res1$status$code!=0){
    return(reaction_table %>%
             mutate(flux = 0))
  }
  
  if(!do_minimization){
    return(reaction_table %>%
             mutate(flux = res1[['solution']])
    )
  }
  
  mod2 <- reaction_table %>%
    mutate(flux = res1[['solution']],
           lowbnd = if_else(.data$flux>0, pmax(0, .data$lowbnd), .data$lowbnd),
           uppbnd = if_else(.data$flux<0, pmin(0, .data$uppbnd), .data$uppbnd),
           lowbnd = if_else(.data$obj_coef>0, .data$flux, .data$lowbnd),
           uppbnd = if_else(.data$obj_coef<0, .data$flux, .data$uppbnd),
           obj_coef = if_else(near(.data$obj_coef,0),-sign(.data$flux), 0)
           ) %>%
    reactiontbl_to_expanded() %>%
    expanded_to_ROI()
  
  res2 <- ROI::ROI_solve(mod2)
  
  return(reaction_table %>%
           mutate(flux = res2[['solution']])
  )
}


#' Given a metabolic model as a data frame, return a new data frame with fluxes and variability
#' 
#' This function calculates fluxes \code{folds} times with shuffled versions of the metabolic model.
#' This is designed to detect and quantify underdetermined fluxes.
#' 
#' @details 
#' This function uses ROI, so to solve models, you will need a solver plugin for ROI. Probably the easiest one to install is ROI.plugin.glpk.
#' To install this in Linux, run \code{sudo apt-get install libglpk-dev} in a terminal, and then run \code{install.packages('ROI.plugin.glpk')} in R.
#'
#' 
#' @param reaction_table a data frame representing the metabolic model
#' @param folds number of times to calculate fluxes
#' @param do_minimization toggle to uniformly minimize all non-objective fluxes after finding the objective
#' 
#' @return reaction_table with two added columns: sd (the standard deviation of fluxes found) and flux (a typical flux) from this distribution
#' 
#' @export
#' @importFrom magrittr %>%
#' @import dplyr
#' @importFrom purrr map
#' @importFrom rlang .data
find_flux_variability_df <- function(reaction_table, folds=10, do_minimization=TRUE){
  replicates <- tibble(index=1:(folds %/% 2)) %>% 
    mutate(data = purrr::map(.data$index, function(x){reaction_table})) %>%
    mutate(data = map(.data$data, sample_frac)) 
  
  replicates_reversed <- replicates %>%
    mutate(data = map(.data$data, function(x){x[nrow(x):1,]}))
  
  fluxdf <- bind_rows(replicates, replicates_reversed) %>%
    mutate(data = map(.data$data, find_fluxes_df, do_minimization=do_minimization)) %>%
    mutate(data = map(.data$data, arrange_, 'abbreviation')) %>%
    tidyr::unnest(cols='data') %>%
    select(.data$abbreviation, .data$flux) %>%
    group_by(.data$abbreviation) %>%
    summarise(sd = stats::sd(.data$flux, na.rm=TRUE), flux = first(.data$flux))
  
  inner_join(reaction_table, fluxdf, by='abbreviation')
}