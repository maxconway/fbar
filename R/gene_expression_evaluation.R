liberalmin <- function(a1, a2){
  if(is.na(a1) && is.na(a2)){
    return(NA)
  }else{
    min(a1, a2, na.rm=TRUE)
  }
}
liberalmax <- function(a1, a2){
  if(is.na(a1) && is.na(a2)){
    return(NA)
  }else{
    max(a1, a2, na.rm=TRUE)
  }
}

#' @import assertthat
multi_subs <- function(names, presences){
  assert_that(is.character(names))
  assert_that(are_equal(length(names), length(presences)))
  
  e <- new.env(parent=emptyenv())
  assign('unmapped', TRUE, e)
  
  for(i in 1:length(names)){
    assign(names[i], presences[i], pos=e)
  }
  
  return(e)
}

#' Function to estimate the expression levels of gene sets
#' 
#' @param expressions A list of gene set expression strings: names of genes punctuated with \code{&}, \code{|} and brackets.
#' @param genes A list of gene names
#' @param presences A list of gene presences, the same length as 'genes'
#' 
#' This function evaluates the gene set expressions in the context of the gene presences. 
#' It can take booleans, or numbers, in which case it associates \code{&} with finding the minimum, and \code{|} with finding the maximum.
#' 
#' @export
gene_eval <- function(expressions, genes, presences){
  expressions[expressions=='' | is.na(expressions)] <- NA
  e <- multi_subs(genes, presences)
  
  assign('&', liberalmin, e)
  assign('|', liberalmax, e)
  assign('(', `(`, e)
  
  purrr::map_dbl(expressions, function(x){
    eval(parse(text=x),e)
  })
}