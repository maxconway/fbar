#' Internal function: Splitting reaction equation into substrate and product
#' 
#' @param equations Character vector of reaction equations.
#' @param regex_arrow Regular expression for the arrow splitting sides of the reaction equation.
#' 
#' @return a \code{data_frame}, with columns: \describe{
#'   \item{reversible}{boolean, is reaction reversible}
#'   \item{before}{the left hand side of the reaction string}
#'   \item{after}{the right hand side of the reaction string}
#' }
#' 
#' 
#' @importFrom magrittr %>%
#' @import assertthat
#' @import dplyr
#' @import stringr
#' @importFrom rlang .data
split_on_arrow <- function(equations, regex_arrow = '<?[-=]+>'){
  #assert_that(length(equations)>0)
  assert_that(all(str_count(equations, regex_arrow) == 1))
  
  split <- str_split_fixed(equations, regex_arrow, 2) 
  
  colnames(split) <- c('before', 'after')
  
  split %>% 
    tibble::as_tibble() %>%
    mutate(reversible = equations %>%
              str_extract(regex_arrow) %>%
              str_detect('<'),
            before = str_trim(.data$before),
            after = str_trim(.data$after)
    ) %>%
    return()
}


#' Internal function: Expand half reaction equations into a long form
#' 
#' @param mets Character vector of halves of reaction equations.
#' 
#' @return a \code{date_frame} with columns: \describe{
#'   \item{stoich}{the stoichiometric coefficient}
#'   \item{met}{the metabolite}
#' }
#' 
#' @importFrom magrittr %>%
#' @import dplyr
#' @import stringr
parse_met_list <- function(mets){
  pattern_stoich <- '^[[:space:]]*[[:digit:].()e-]+[[:space:]]+'
  stoich <- mets %>% 
    str_extract(pattern_stoich) %>% 
    str_replace_all('[[:space:]()]+','') %>%
    as.numeric
  stoich[is.na(stoich)] <- 1
  met <- mets %>% 
    str_replace( pattern_stoich,'') %>% 
    str_trim()
  tibble::tibble(stoich, met)
}


#' Parse a reaction table to an intermediate, long format
#' 
#' The long format can also be suitable for manipulating equations.
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
#' @return A list of data frames: \itemize{
#'   \item \code{rxns}, which has one row per reaction, 
#'   \item \code{mets}, which has one row for each metabolite, and 
#'   \item \code{stoich}, which has one row for each time a metabolite appears in a reaction.
#' }
#' 
#' @family parsing_and_conversion
#' @export
#' @importFrom magrittr %>%
#' @import assertthat
#' @import dplyr
#' @import stringr
#' @importFrom rlang .data
#' 
#' @examples 
#' 
#' \dontrun{
#' data(ecoli_core)
#' library(dplyr)
#' try(library(ROI.plugin.ecos)) # make a solver available to ROI
#'
#' roi_model <- ecoli_core %>%
#'   reactiontbl_to_expanded %>%
#'   expanded_to_ROI
#'   
#' if(length(ROI::ROI_applicable_solvers(roi_model))>=1){
#'   roi_result <- ROI::ROI_solve(roi_model)
#'   
#'   ecoli_core_with_flux <- ecoli_core %>%
#'     mutate(flux = roi_result[['solution']])
#' }
#' }
reactiontbl_to_expanded <- function(reaction_table, regex_arrow = '<?[-=]+>'){
  assert_that('data.frame' %in% class(reaction_table))
  assert_that(reaction_table %has_name% 'abbreviation')
  assert_that(reaction_table %has_name% 'equation')
  assert_that(reaction_table %has_name% 'uppbnd')
  assert_that(reaction_table %has_name% 'lowbnd')
  assert_that(reaction_table %has_name% 'obj_coef')
  assert_that(sum(duplicated(reaction_table$abbreviation))==0)
  assert_that(!any(stringr::str_detect(reaction_table$equation, '^\\[\\w+?]:'))) # can't handle compartments at start of string
  
  const_inf <- 1000
  
  reactions_expanded_partial_1 <- split_on_arrow(reaction_table[['equation']], regex_arrow) %>%
    mutate(abbreviation = reaction_table[['abbreviation']])
  
  
  reactions_expanded_partial_2 <- bind_rows(
    reactions_expanded_partial_1 %>%
      transmute(.data$abbreviation, string = .data$before, direction = -1),
    reactions_expanded_partial_1 %>%
      transmute(.data$abbreviation, string = .data$after, direction = 1)
  )
  
  reactions_expanded_partial_3 <- reactions_expanded_partial_2 %>%
    mutate(symbol = stringr::str_split(.data$string, stringr::fixed(' + '))) %>%
    (function(x){
      if(nrow(x)>0){
        tidyr::unnest(x, cols=.data$symbol)
      } else {
        return(x)
      }
    }) %>%
    filter(.data$symbol!='')
  
  reactions_expanded <- bind_cols(reactions_expanded_partial_3,
                                  parse_met_list(reactions_expanded_partial_3$symbol)) %>%
    transmute(abbreviation = .data$abbreviation,
               stoich = .data$stoich*.data$direction,
               met = .data$met) %>%
    filter(.data$met!='')
  
  return(list(stoich = reactions_expanded %>%
                group_by(.data$abbreviation, .data$met) %>%
                summarise(stoich = sum(.data$stoich)) %>%
                ungroup(), 
              rxns = reaction_table %>% 
                select(-.data$equation) %>%
                ungroup(),
              mets = reactions_expanded %>% 
                group_by(.data$met) %>%
                summarise() %>%
                ungroup()))
}

#' Convert intermediate expanded format back to a reaction table
#' 
#' Useful for saving a new or edited model
#' 
#' @param expanded A list of data frames: \itemize{
#'   \item \code{rxns}, which has one row per reaction, 
#'   \item \code{mets}, which has one row for each metabolite, and 
#'   \item \code{stoich}, which has one row for each time a metabolite appears in a reaction.
#' }
#' 
#' @return A data frame describing the metabolic model.
#' 
#' @import dplyr
#' @import stringr
#' @importFrom rlang .data
#' @export
expanded_to_reactiontbl <- function(expanded){
  equation_tbl <- expanded$stoich %>%
    mutate(side = c('substrate', 'none', 'product')[sign(.data$stoich)+2],
            symbol = if_else(abs(.data$stoich)!=1, 
                              str_c('(',abs(.data$stoich),') ',.data$met), 
                             .data$met
            )
    ) %>%
    group_by(.data$abbreviation, .data$side) %>%
    summarise(sum = str_c(.data$symbol, collapse=' + ', '')) %>%
    tidyr::spread(key = 'side', value = 'sum', fill='')
  
  reaction_table <- inner_join(expanded$rxns, equation_tbl, by = "abbreviation") %>%
    mutate(reversible = .data$lowbnd<0) %>%
    mutate(equation = str_c(.data$substrate, c('-->', '<==>')[.data$reversible+1], .data$product,sep=' ')) %>%
    select(-.data$substrate, -.data$product, -.data$reversible) %>%
    ungroup
  
  assert_that('data.frame' %in% class(reaction_table))
  assert_that(reaction_table %has_name% 'abbreviation')
  assert_that(reaction_table %has_name% 'equation')
  assert_that(reaction_table %has_name% 'uppbnd')
  assert_that(reaction_table %has_name% 'lowbnd')
  assert_that(reaction_table %has_name% 'obj_coef')
  assert_that(sum(duplicated(reaction_table$abbreviation))==0)
  assert_that(!any(stringr::str_detect(reaction_table$equation, '^\\[\\w+?]:'))) # can't handle compartments at start of string
  
  return(reaction_table)
}

#' Parse a long format metabolic model to an ROI model
#' 
#' This parses the long format produced by \code{reactiontbl_to_expanded} to an ROI model.
#' 
#' @details 
#' To solve models using ROI, you will need a solver plugin for ROI. Probably the easiest one to install is ROI.plugin.glpk.
#' To install this in Linux, run \code{sudo apt-get install libglpk-dev} in a terminal, and then run \code{install.packages('ROI.plugin.glpk')} in R.
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
#' @return A list suitable for input to ROI.
#' 
#' @family parsing_and_conversion
#' @export
#' @import Matrix
#' @import ROI
#' 
#' @examples 
#' 
#' \dontrun{
#' data(ecoli_core)
#' library(dplyr)
#' try(library(ROI.plugin.ecos)) # make a solver available to ROI
#'
#' roi_model <- ecoli_core %>%
#'   reactiontbl_to_expanded %>%
#'   expanded_to_ROI
#'   
#' if(length(ROI::ROI_applicable_solvers(roi_model))>=1){
#'   roi_result <- ROI::ROI_solve(roi_model)
#'   
#'   ecoli_core_with_flux <- ecoli_core %>%
#'     mutate(flux = roi_result[['solution']])
#' }
#' }
expanded_to_ROI <- function(reactions_expanded){
  
  validate_expanded(reactions_expanded)
  
  rxns <- reactions_expanded$rxns
  stoich <- reactions_expanded$stoich
  mets <- reactions_expanded$mets
  
  stoichiometric_matrix <- Matrix::sparseMatrix(j = match(stoich$abbreviation, rxns$abbreviation),
                                                i = match(stoich$met, mets$met),
                                                x = stoich$stoich,
                                                dims = c(nrow(mets),
                                                         nrow(rxns)
                                                ),
                                                dimnames = list(metabolites=mets$met,
                                                                reactions=rxns$abbreviation)
  )
  
  problem <- OP(objective = L_objective(L = rxns$obj_coef), 
                constraints =  L_constraint(L = as.matrix(stoichiometric_matrix), 
                                            dir = rep('==', times=nrow(stoichiometric_matrix)), 
                                            rhs = rep(0, times=nrow(stoichiometric_matrix))
                ),
                bounds=V_bound(li = seq_along(rxns$lowbnd), lb = rxns$lowbnd,
                               ui = seq_along(rxns$uppbnd), ub = rxns$uppbnd),
                maximum = TRUE
  )
  
  return(problem)
}


