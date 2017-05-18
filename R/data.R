#' A small ecoli model, created from a number of sources.
#' 
#' @format A data frame with 95 rows and 7 columns:
#' \describe{
#'   \item{abbreviation}{an abbreviated reaction name, acts as the reaction id}
#'   \item{officialName}{full reaction name}
#'   \item{equation}{reaction equation}
#'   \item{lowbnd}{lower bound on the reaction rate}
#'   \item{uppbnd}{upper bound on the reaction rate}
#'   \item{obj_coef}{identifies a reaction (or reactions) for which the maximum possible rate should be found}
#'   \item{geneAssociation}{A boolean combination of genes which control the reaction}
#' }
#' @source \url{http://gcrg.ucsd.edu/Downloads/EcoliCore}, \href{http://www.asmscience.org/content/journal/ecosalplus/10.1128/ecosalplus.10.2.1}{Reconstruction and Use of Microbial Metabolic Networks: the Core Escherichia coli Metabolic Model as an Educational Guide}, \href{https://www.ncbi.nlm.nih.gov/pubmed/21988831}{A comprehensive genome-scale reconstruction of Escherichia coli metabolism--2011.}
'ecoli_core'

#' A full size ecoli model.
#' 
#' @format A data frame with 2,583 rows and 10 columns:
#' \describe{
#'   \item{abbreviation}{an abbreviated reaction name, acts as the reaction id}
#'   \item{name}{full reaction name}
#'   \item{equation}{reaction equation}
#'   \item{subsystem}{an indicator of reaction function}
#'   \item{lowbnd}{lower bound on the reaction rate}
#'   \item{uppbnd}{upper bound on the reaction rate}
#'   \item{obj_coef}{identifies a reaction (or reactions) for which the maximum possible rate should be found}
#'   \item{GPRassoc}{Boolean combination of genes and proteins which control the reaction}
#'   \item{GRassoc}{Boolean combination of genes which control the reaction}
#'   \item{PRassoc}{Boolean combination of proteins which control the reaction}
#' }
#' @source \href{https://www.ncbi.nlm.nih.gov/pubmed/21988831}{A comprehensive genome-scale reconstruction of Escherichia coli metabolism--2011.}
'iJO1366'