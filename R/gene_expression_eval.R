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

#' @export
gene_eval <- function(expressions, genes, presences){
  expressions[expressions=='' | is.na(expressions)] <- NA
  e <- multi_subs(genes, presences)
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
  
  assign('&', liberalmin, e)
  assign('|', liberalmax, e)
  assign('(', `(`, e)
  
  purrr::map_dbl(expressions, function(x){
    eval(parse(text=x),e)
  })
}