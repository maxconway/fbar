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
parse_reaction_table <- function(reaction_table){
  assert_that('data.frame' %in% class(reaction_table))
  assert_that(reaction_table %has_name% 'abbreviation')
  assert_that(reaction_table %has_name% 'equation')
  assert_that(reaction_table %has_name% 'uppbnd')
  assert_that(reaction_table %has_name% 'lowbnd')
  assert_that(reaction_table %has_name% 'obj_coef')
  assert_that(!any(str_detect(reaction_table$equation, '^\\[\\w+?]:'))) # can't handle compartments at start of string
  
  const_inf <- 1000
  
  pattern_arrow <- '[[:space:]]*<?[-=]+>[[:space:]]*'
  reversible <- reaction_table[['equation']] %>%
    str_extract(pattern_arrow) %>%
    str_detect('<')
  
  uppbnd <- pmin(reaction_table[['uppbnd']], const_inf, na.rm=TRUE)
  lowbnd <- pmax(reaction_table[['lowbnd']], ifelse(reversible, -const_inf, 0), na.rm=TRUE)
  obj_coef <- reaction_table[['obj_coef']]
  obj_coef[is.na(obj_coef)] <- 0
  
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
  
  reactions_expanded_partial_1 <- reaction_table[['equation']] %>%
    str_split_fixed(pattern_arrow,2) %>%
    cbind(reaction_table[['abbreviation']])
  
  reactions_expanded_partial_2 <- rbind_list(
   data.frame(direction = -1, string = reactions_expanded_partial_1[,1], abbreviation = reactions_expanded_partial_1[,3], stringsAsFactors = FALSE),
   data.frame(direction = 1, string = reactions_expanded_partial_1[,2], abbreviation = reactions_expanded_partial_1[,3], stringsAsFactors = FALSE)
   ) 
  
  symbols = str_split(reactions_expanded_partial_2$string, fixed(' + '))
  
  reactions_expanded_partial_3 <- cbind(
    reactions_expanded_partial_2[rep.int(1:nrow(reactions_expanded_partial_2), times=plyr::laply(symbols, length)),],
    symbol = unlist(symbols)
    )
  
  reactions_expanded <- parse_met_list(reactions_expanded_partial_3$symbol) %>%
    transmute(abbreviation = reactions_expanded_partial_3$abbreviation,
              stoich = stoich*reactions_expanded_partial_3$direction,
              met = met) %>%
    filter(met!='')
  
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
    obj = obj_coef,
    sense='=',
    rhs=0,
    lb=lowbnd,
    ub=uppbnd,
    modelsense='max'
    )
    
  return(model)
}