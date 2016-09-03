#' @import dplyr 
#' @import stringr
split_on_arrow <- function(equations, pattern_arrow = '<?[-=]+>'){
  #assert_that(length(equations)>0)
  assert_that(all(str_count(equations, pattern_arrow) == 1))
  
  split <- str_split_fixed(equations, pattern_arrow, 2) 
  
  colnames(split) <- c('before', 'after')
  
  split %>% 
    as_data_frame() %>%
    mutate(reversible = equations %>%
             str_extract(pattern_arrow) %>%
             str_detect('<'),
           before = str_trim(before),
           after = str_trim(after)
           ) %>%
    return
}

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
  data_frame(stoich, met)
}


#' parse reaction table to an intermediate, long format
#' 
#' Used as the first half of \code{parse_reactions}
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
#' 
#' @export
#' @import dplyr 
#' @import assertthat 
#' @import stringr
expand_reactions <- function(reaction_table, pattern_arrow){
  assert_that('data.frame' %in% class(reaction_table))
  assert_that(reaction_table %has_name% 'abbreviation')
  assert_that(reaction_table %has_name% 'equation')
  assert_that(reaction_table %has_name% 'uppbnd')
  assert_that(reaction_table %has_name% 'lowbnd')
  assert_that(reaction_table %has_name% 'obj_coef')
  assert_that(!any(str_detect(reaction_table$equation, '^\\[\\w+?]:'))) # can't handle compartments at start of string
  
  const_inf <- 1000
  
  reactions_expanded_partial_1 <- split_on_arrow(reaction_table[['equation']], pattern_arrow) %>%
    mutate(abbreviation = reaction_table[['abbreviation']])
  
  uppbnd <- pmin(reaction_table[['uppbnd']], const_inf, na.rm=TRUE)
  lowbnd <- pmax(reaction_table[['lowbnd']], ifelse(reactions_expanded_partial_1[['reversible']], -const_inf, 0), na.rm=TRUE)
  obj_coef <- reaction_table[['obj_coef']]
  obj_coef[is.na(obj_coef)] <- 0
  
  reactions_expanded_partial_2 <- bind_rows(
    reactions_expanded_partial_1 %>%
      transmute(abbreviation, string = before, direction = -1),
    reactions_expanded_partial_1 %>%
      transmute(abbreviation, string = after, direction = 1)
  )
  
  reactions_expanded_partial_3 <- reactions_expanded_partial_2 %>%
    mutate(symbol = str_split(string, fixed(' + '))) %>%
    (function(x){
      if(nrow(x)>0){
        tidyr::unnest(x, symbol)
      } else {
        return(x)
      }
      }) %>%
    filter(symbol!='')
  
  reactions_expanded <- bind_cols(reactions_expanded_partial_3,
                                  parse_met_list(reactions_expanded_partial_3$symbol)) %>%
    transmute(abbreviation = abbreviation,
              stoich = stoich*direction,
              met = met) %>%
    filter(met!='')
  
  return(reactions_expanded)
}


#' parse a long format metabolic model to a gurobi model
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
#' @param reactions_expanded A data frame as output by \code{expand_reactions}
#' @param reaction_table A data frame describing the metabolic model.
#' 
#' @export
#' @import dplyr 
#' @import assertthat 
#' @import stringr
collapse_reactions <- function(reactions_expanded, reaction_table){
  assert_that('data.frame' %in% class(reaction_table))
  assert_that(reaction_table %has_name% 'abbreviation')
  assert_that(reaction_table %has_name% 'uppbnd')
  assert_that(reaction_table %has_name% 'lowbnd')
  assert_that(reaction_table %has_name% 'obj_coef')
  stoichiometric_matrix <- Matrix::sparseMatrix(j = match(reactions_expanded[['abbreviation']], reaction_table[['abbreviation']]),
                                                i = match(reactions_expanded[['met']], sort(unique(reactions_expanded$met))),
                                                x = reactions_expanded[['stoich']],
                                                dims = c(length(unique(reactions_expanded$met)),
                                                         length(reaction_table[['abbreviation']])
                                                ),
                                                dimnames = list(metabolites=sort(unique(reactions_expanded$met)),
                                                                reactions=reaction_table[['abbreviation']])
  )
  
  model <- list(
    A = stoichiometric_matrix,
    obj = reaction_table$obj_coef,
    sense='=',
    rhs=0,
    lb=reaction_table$lowbnd,
    ub=reaction_table$uppbnd,
    modelsense='max'
  )
  
  return(model)
}




#' parse reaction table to Gurobi format
#' 
#' Parses a reaction table to give a list in Gurobi's input format
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
#' 
#' @export
#' @import dplyr 
#' @import assertthat 
#' @import stringr
parse_reaction_table <- function(reaction_table, pattern_arrow = '<?[-=]+>'){
  collapse_reactions(reactions_expanded = expand_reactions(reaction_table, pattern_arrow), 
                     reaction_table = reaction_table
                     )
}