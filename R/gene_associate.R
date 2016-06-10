#' Apply gene expressions to reaction table
#' 
#' @param reaction_table A data frame describing the metabolic model.
#' @param genes A data frame showing gene presence
#' 
#' @export
#' @import assertthat 
#' @import dplyr
gene_associate <- function(reaction_table, gene_table, expression_flux_function = function(x){(1+log(x)/sd(x)^2)^sign(x-1)}){
  assert_that('data.frame' %in% class(reaction_table))
  assert_that('data.frame' %in% class(gene_table))
  assert_that(reaction_table %has_name% 'geneAssociation')
  assert_that(gene_table %has_name% 'name')
  assert_that(gene_table %has_name% 'presence')
  
  reaction_table %>%
    mutate(present = gene_eval(geneAssociation, gene_table$name, gene_table$presence)) %>%
    mutate(uppbnd = uppbnd*expression_flux_function(present),
           lowbnd = lowbnd*expression_flux_function(present))
}