metmod2gurobi <- function(metmod){
  stoichiometric_matrix <- metmod$stoich %>% 
    group_by(species, id) %>%
    summarise(stoichiometry = sum(stoichiometry)) %>%
    spread(id, stoichiometry, 0) %>%
    (function(x){rownames(x) <- x$species; x}) %>%
    select(-species) %>%
    as.matrix
  
  model <- list(
    A = stoichiometric_matrix[,metmod$reactions$rxnid],
    obj = metmod$reactions$obj_coef,
    sense='=',
    rhs=0,
    lb=metmod$reactions$lowbnd,
    ub=metmod$reactions$uppbnd,
    modelsense='max'
  )
}