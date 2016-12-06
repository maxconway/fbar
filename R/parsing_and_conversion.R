#' Internal function for splitting reaction equation into substrate and product
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
split_on_arrow <- function(equations, regex_arrow = '<?[-=]+>'){
  #assert_that(length(equations)>0)
  assert_that(all(str_count(equations, regex_arrow) == 1))
  
  split <- str_split_fixed(equations, regex_arrow, 2) 
  
  colnames(split) <- c('before', 'after')
  
  split %>% 
    tibble::as_data_frame() %>%
    mutate_(reversible =~ equations %>%
              str_extract(regex_arrow) %>%
              str_detect('<'),
            before =~ str_trim(before),
            after =~ str_trim(after)
    ) %>%
    return
}


#' Expand half reaction equations into a long form
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
  tibble::data_frame(stoich, met)
}


#' Parse a reaction table to an intermediate, long format
#' 
#' Used as the first half of \code{reactiontbl_to_gurobi}. The long format can also be suitable for manipulating equations.
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
#' @export
#' @importFrom magrittr %>%
#' @import assertthat
#' @import dplyr
#' @import stringr
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
    mutate_(abbreviation =~ reaction_table[['abbreviation']])
  

  reactions_expanded_partial_2 <- bind_rows(
    reactions_expanded_partial_1 %>%
      transmute_(~abbreviation, string =~ before, direction =~ -1),
    reactions_expanded_partial_1 %>%
      transmute_(~abbreviation, string =~ after, direction =~ 1)
  )
  
  reactions_expanded_partial_3 <- reactions_expanded_partial_2 %>%
    mutate_(symbol =~ stringr::str_split(string, stringr::fixed(' + '))) %>%
    (function(x){
      if(nrow(x)>0){
        tidyr::unnest_(x, 'symbol')
      } else {
        return(x)
      }
    }) %>%
    filter_(~symbol!='')
  
  reactions_expanded <- bind_cols(reactions_expanded_partial_3,
                                  parse_met_list(reactions_expanded_partial_3$symbol)) %>%
    transmute_(abbreviation =~ abbreviation,
               stoich =~ stoich*direction,
               met =~ met) %>%
    filter_(~met!='')
  
  return(list(stoich = reactions_expanded, 
              rxns = reaction_table %>% 
                select_(~-equation),
              mets = reactions_expanded %>% 
                group_by_(~met) %>%
                summarise()))
}


#' Parse a long format metabolic model to a gurobi model
#' 
#' Used as the second half of \code{parse_reactions}, this parses the long format produced by \code{expand_reactions} to a gurobi model
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
#' @export
#' @import assertthat 
#' @import Matrix
expanded_to_gurobi <- function(reactions_expanded){
  
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




#' Parse reaction table to Gurobi format
#' 
#' Parses a reaction table to give a list in Gurobi's input format.
#' 
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
#' @export
reactiontbl_to_gurobi <- function(reaction_table, regex_arrow = '<?[-=]+>'){
  expanded_to_gurobi(reactiontbl_to_expanded(reaction_table, regex_arrow))
}

# Deprecated functions
expand_reactions <- function(reaction_table, regex_arrow = '<?[-=]+>'){
  .Deprecated('reactiontbl_to_expanded')
  reactiontbl_to_expanded(reaction_table, regex_arrow)$stoich
}

collapse_reactions_gurobi <- function(reactions_expanded, reaction_table){
  .Deprecated('expanded_to_gurobi')
  expanded_to_gurobi(list(stoich=reactions_expanded, rxns=reaction_table))
}

parse_reaction_table <- function(reaction_table, regex_arrow = '<?[-=]+>'){
  .Deprecated('reactiontbl_to_gurobi')
  reactiontbl_to_gurobi(reaction_table, regex_arrow)
}

