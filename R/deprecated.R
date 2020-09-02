# Deprecated

#' Parse a long format metabolic model to a Gurobi model
#' 
#' \strong{This function is deprecated. \code{github.com/Fl0Sch/ROI.plugin.gurobi} is recommended instead.}
#' 
#' Used as the second half of \code{\link{reactiontbl_to_gurobi}}, this parses the long format produced by \code{reactiontbl_to_expanded} to a Gurobi model
#' 
#' @details 
#' For installation instructions for Gurobi, refer to the Gurobi website: \url{https://www.gurobi.com/}.
#' 
#' The \code{reaction_table} must have columns:
#' \itemize{
#'  \item \code{abbreviation},
#'  \item \code{equation},
#'  \item \code{uppbnd},
#'  \item \code{lowbnd}, and
#'  \item \code{obj_coef}.
#' }
#' 
#' @param reactions_expanded A list of data frames as output by \code{expand_reactions}
#' 
#' @return A list suitable for input to Gurobi.
#' 
#' @family parsing_and_conversion
#' @export
#' @import assertthat 
#' @import Matrix
expanded_to_gurobi <- function(reactions_expanded){
  .Deprecated("expanded_to_ROI")
  
  rxns <- reactions_expanded$rxns
  stoich <- reactions_expanded$stoich
  mets <- reactions_expanded$mets
  
  assert_that('data.frame' %in% class(rxns))
  assert_that(rxns %has_name% 'abbreviation')
  assert_that(rxns %has_name% 'uppbnd')
  assert_that(rxns %has_name% 'lowbnd')
  assert_that(rxns %has_name% 'obj_coef')
  
  stoichiometric_matrix <- Matrix::sparseMatrix(j = match(stoich$abbreviation, rxns$abbreviation),
                                                i = match(stoich$met, mets$met),
                                                x = stoich$stoich,
                                                dims = c(nrow(mets),
                                                         nrow(rxns)
                                                ),
                                                dimnames = list(metabolites=mets$met,
                                                                reactions=rxns$abbreviation)
  )
  
  model <- list(
    A = stoichiometric_matrix,
    obj = rxns$obj_coef,
    sense='=',
    rhs=0,
    lb=rxns$lowbnd,
    ub=rxns$uppbnd,
    modelsense='max'
  )
  
  return(model)
}

#' Parse a long format metabolic model to a glpk model
#' 
#' \strong{This function is deprecated. \code{ROI.plugin.glpk} is recommended instead.}
#' 
#' This parses the long format produced by \code{reactiontbl_to_expanded} to a glpk model.
#' 
#' @details 
#' To install the Rglpk package in Linux, run \code{sudo apt-get install libglpk-dev} in a terminal, and then run \code{install.packages('Rglpk')} in R.
#' 
#' The \code{reaction_table} must have columns:
#' \itemize{
#'  \item \code{abbreviation},
#'  \item \code{equation},
#'  \item \code{uppbnd},
#'  \item \code{lowbnd}, and
#'  \item \code{obj_coef}.
#' }
#' 
#' @param reactions_expanded A list of data frames as output by \code{reactiontbl_to_expanded}
#' 
#' @return A list suitable for input to Rglpk
#' 
#' @family parsing_and_conversion
#' @export
#' @import assertthat 
#' @import Matrix
expanded_to_glpk <- function(reactions_expanded){
  .Deprecated("expanded_to_ROI")
  
  rxns <- reactions_expanded$rxns
  stoich <- reactions_expanded$stoich
  mets <- reactions_expanded$mets
  
  assert_that('data.frame' %in% class(rxns))
  assert_that(rxns %has_name% 'abbreviation')
  assert_that(rxns %has_name% 'uppbnd')
  assert_that(rxns %has_name% 'lowbnd')
  assert_that(rxns %has_name% 'obj_coef')
  
  stoichiometric_matrix <- Matrix::sparseMatrix(j = match(stoich$abbreviation, rxns$abbreviation),
                                                i = match(stoich$met, mets$met),
                                                x = stoich$stoich,
                                                dims = c(nrow(mets),
                                                         nrow(rxns)
                                                ),
                                                dimnames = list(metabolites=mets$met,
                                                                reactions=rxns$abbreviation)
  )
  
  model <- list(
    mat = stoichiometric_matrix,
    obj = rxns$obj_coef,
    dir= rep('==', times=nrow(stoichiometric_matrix)),
    rhs= rep(0, times=nrow(stoichiometric_matrix)),
    bounds=list(lower=list(ind = seq_along(rxns$lowbnd), val = rxns$lowbnd),
                upper=list(ind = seq_along(rxns$uppbnd), val = rxns$uppbnd)),
    max=TRUE
  )
  
  return(model)
}

#' Parse reaction table to Gurobi format
#' 
#' \strong{This function is deprecated. \code{github.com/Fl0Sch/ROI.plugin.gurobi} is recommended instead.}
#' 
#' Parses a reaction table to give a list in Gurobi's input format.
#' This function is a shorthand for \code{\link{reactiontbl_to_expanded}} followed by \code{\link{expanded_to_gurobi}}.
#' 
#' The \code{reaction_table} must have columns:
#' \itemize{
#'  \item \code{abbreviation},
#'  \item \code{equation},
#'  \item \code{uppbnd},
#'  \item \code{lowbnd}, and
#'  \item \code{obj_coef}.
#' }
#' 
#' @param reaction_table A data frame describing the metabolic model.
#' @param regex_arrow Regular expression for the arrow splitting sides of the reaction equation.
#' 
#' @return A list suitable for input to Gurobi.
#' 
#' @family parsing_and_conversion
#' @export
reactiontbl_to_gurobi <- function(reaction_table, regex_arrow = '<?[-=]+>'){
  .Deprecated("reactiontbl_to_expanded %>% expanded_to_ROI")
  expanded_to_gurobi(reactiontbl_to_expanded(reaction_table, regex_arrow))
}

expand_reactions <- function(reaction_table, regex_arrow = '<?[-=]+>'){
  .Deprecated('reactiontbl_to_expanded')
  reactiontbl_to_expanded(reaction_table, regex_arrow)$stoich
}

collapse_reactions_gurobi <- function(reactions_expanded, reaction_table){
  .Deprecated('expanded_to_ROI')
  expanded_to_gurobi(list(stoich=reactions_expanded, rxns=reaction_table))
}

parse_reaction_table <- function(reaction_table, regex_arrow = '<?[-=]+>'){
  .Deprecated('reactiontbl_to_ROI')
  reactiontbl_to_gurobi(reaction_table, regex_arrow)
}