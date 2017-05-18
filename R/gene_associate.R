#' Apply gene expressions to reaction table
#' 
#' A convenience function that uses \code{\link{gene_eval}} and a custom function to apply new upper and lower bounds.
#' 
#' @param reaction_table A data frame describing the metabolic model.
#' @param gene_table A data frame showing gene presence
#' @param expression_flux_function a function to convert from gene set expression to flux
#' 
#' @return the reaction_table, with a new column, present, and altered upper and lower bounds
#' 
#' @section Warning:
#' This function relies on \code{\link{gene_eval}}, which uses \code{\link{eval}} to evaluate gene expression sets. 
#' This gives flexibility, but means that malicious code in the \code{gene_sets} argument could get evaluated.
#' \code{gene_sets} is evaluated in a restricted environment, but there might be a way around this, so you might want to check for anything suspicious in this argument manually.
#' For more information, read the code.
#' 
#' @seealso gene_eval
#' @export
#' @import assertthat 
#' @import dplyr
#' @importFrom magrittr %>%
#' 
#' @example 
#' data(iJO1366)
#' 
#' gene_table = data_frame(name = iJO1366$GRassoc %>%
#' stringr::str_split('and|or|\\s|\\(|\\)') %>%
#'   purrr::flatten_chr() %>%
#'   unique,
#' presence = 1) %>%
#'   filter(name != '', !is.na(name))
#' 
#' gene_associate(reaction_table = iJO1366 %>%
#'                  mutate(geneAssociation = GRassoc %>%
#'                           stringr::str_replace_all('and', '&') %>%
#'                           stringr::str_replace_all('or', '|')
#'                  ),
#'                gene_table = gene_table
#' )
gene_associate <- function(reaction_table, gene_table, expression_flux_function = function(x){(1+log(x)/stats::sd(x)^2)^sign(x-1)}){
  assert_that('data.frame' %in% class(reaction_table))
  assert_that('data.frame' %in% class(gene_table))
  assert_that(reaction_table %has_name% 'geneAssociation')
  assert_that(gene_table %has_name% 'name')
  assert_that(gene_table %has_name% 'presence')
  
  reaction_table %>%
    mutate_(present =~ gene_eval(geneAssociation, gene_table$name, gene_table$presence)) %>%
    mutate_(uppbnd =~ uppbnd*expression_flux_function(present),
            lowbnd =~ lowbnd*expression_flux_function(present))
}