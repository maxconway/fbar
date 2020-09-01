#' A small E. coli model, created from a number of sources.
#' 
#' @format A data frame with 95 rows and 7 columns:
#' \describe{
#'   \item{abbreviation}{an abbreviated reaction name, acts as the reaction id}
#'   \item{lowbnd}{lower bound on the reaction rate}
#'   \item{uppbnd}{upper bound on the reaction rate}
#'   \item{obj_coef}{identifies a reaction (or reactions) for which the maximum possible rate should be found}
#'   \item{equation}{reaction equation}
#'   \item{officialName}{full reaction name}
#'   \item{geneAssociation}{A boolean combination of genes which control the reaction}
#'   \item{subsystem}{an indicator of reaction function}
#' }
#' @source \url{http://bigg.ucsd.edu}, \href{https://www.asmscience.org/content/journal/ecosalplus/10.1128/ecosalplus.10.2.1}{Reconstruction and Use of Microbial Metabolic Networks: the Core Escherichia coli Metabolic Model as an Educational Guide}, \href{https://www.ncbi.nlm.nih.gov/pubmed/21988831}{A comprehensive genome-scale reconstruction of Escherichia coli metabolism--2011.}
'ecoli_core'

#' A full size E. coli model.
#' 
#' @format A data frame with 2,583 rows and 10 columns:
#' \describe{
#'   \item{abbreviation}{an abbreviated reaction name, acts as the reaction id}
#'   \item{lowbnd}{lower bound on the reaction rate}
#'   \item{uppbnd}{upper bound on the reaction rate}
#'   \item{obj_coef}{identifies a reaction (or reactions) for which the maximum possible rate should be found}
#'   \item{equation}{reaction equation}
#'   \item{officialName}{full reaction name}
#'   \item{geneAssociation}{A boolean combination of genes which control the reaction}
#'   \item{subsystem}{an indicator of reaction function}
#' }
#' @source \url{http://bigg.ucsd.edu}, \href{https://www.ncbi.nlm.nih.gov/pubmed/21988831}{A comprehensive genome-scale reconstruction of Escherichia coli metabolism--2011.}
'iJO1366'

#' A subset of exchange reactions annotated to indicate typical availability
#' @format A data frame with 25 rows and 2 columns:
#' \describe{
#'   \item{abbreviation}{an exchange reaction id}
#'   \item{nutrient_type}{the nutrient availability, one of 'micro', 'macro' or 'substrate'}
#' }
'nutrient_types'