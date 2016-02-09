#' @import purrr
multi_subs <- function(names, presences){
  assert_that(is.character(names))
  assert_that(is.logical(presences))
  assert_that(are_equal(length(names), length(presences)))
  
  e <- new.env(parent=emptyenv())
  
  for(i in 1:length(names)){
    assign(names[i], presences[i], pos=e)
  }
  
  return(e)
}

#' @export
#' @import purrr
gene_eval_boolean <- function(expressions, genes, presences){
  e <- multi_subs(genes, presences)
  assign('&', `&&`, e)
  assign('|', `||`, e)
  assign('(', `(`, e)
  
  expressions %>%
    map_lgl(function(x){
      eval(parse(text=x),e)
    })
}