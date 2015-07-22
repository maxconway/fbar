# diagnostic tools

#' diagnose dead end metabolites
#' 
diagnose_dead_ends <- function(stoich){
  no_in <- Matrix::rowSums(stoich>0)==0
  no_out <- Matrix::rowSums(stoich<0)==0
  c(rownames(stoich)[no_in],rownames(stoich)[no_out])
}

#' shrink model
#' 
#' @export
shrink_model <- function(model){
  # remove zero flux reactions
  zero_flux_reactions <- (model$lb == 0) & (model$ub == 0)
  model$ub <- model$ub[!zero_flux_reactions]
  model$lb <- model$lb[!zero_flux_reactions]
  model$obj <- model$obj[!zero_flux_reactions]
  model$A <- model$A[,!zero_flux_reactions]
  
  # remove dead end metabolites and their reactions
  repeat{
    dead_end_mets <- rownames(model$A) %in% diagnose_dead_ends(model$A)
    dead_end_reactions <- Matrix::t((model$A!=0)) %*% Matrix::sparseMatrix(i=which(dead_end_mets),j=rep.int(1,sum(dead_end_mets)), dims=c(length(dead_end_mets),1), x=TRUE)
    dead_end_reactions <- dead_end_reactions[,1]==1
    if(!any(dead_end_mets) && !any(dead_end_reactions)){
      break
    }else{
      print(paste(sum(dead_end_mets),'dead end metabolites found and',sum(dead_end_reactions),'dead end reactions found'))
    }
    model$ub <- model$ub[!dead_end_reactions]
    model$lb <- model$lb[!dead_end_reactions]
    model$obj <- model$obj[!dead_end_reactions]
    model$A <- model$A[,!dead_end_reactions]
    model$A <- model$A[!dead_end_mets,]
  }
  return(model)
}