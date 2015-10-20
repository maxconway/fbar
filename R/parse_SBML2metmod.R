# check that models are translated correctly
parse_SBML2metmod <- function(sbml){
  compartments <- sbml[[1]] %>% 
    keep(function(x){'compartment' %in% names(x)}) %>%
    first %>%
    keep(is.list) %>% 
    map(attributes) %>% 
    bind_rows
  
  species <- sbml[[1]] %>% 
    keep(function(x){'species' %in% names(x)}) %>% 
    first %>%
    keep(is.list) %>% 
    map(attributes) %>% 
    bind_rows %>%
    mutate(boundaryCondition = boundaryCondition=='true',
           boundaryCondition = ifelse(!is.na(boundaryCondition), boundaryCondition, FALSE))
  
  reactions <- sbml[[1]] %>% 
    keep(function(x){'reaction' %in% names(x)}) %>% 
    first %>%
    keep(is.list) %>%
    map(function(x){c(x,attributes(x))}) %>% 
    map(keep, function(x){(is.atomic(x) & length(x) == 1) | ('listOfParameters' %in% names(x))}) %>% 
    map(flatten) %>% 
    map(function(x){
      result <- x$listOfParameters %>%
        map(attributes) %>%
        bind_rows
      result$rxnid <- x$id
      result$name = x$name
      result$reversible = x$reversible
      result
    }) %>%
    bind_rows %>%
    mutate(parameter = str_replace(id, rxnid, '')) %>%
    select(parameter, name, reversible, rxnid, value) %>%
    spread(key=parameter, value=value, drop=TRUE) %>%
    (function(x){
      colnames(x)[str_detect(colnames(x),'LB_|LOWER_')] <- 'lowbnd'
      colnames(x)[str_detect(colnames(x),'UB_|UPPER_')] <- 'uppbnd'
      colnames(x)[str_detect(colnames(x),'OBJ_|OBJECTIVE_COEFFICIENT')] <- 'obj_coef'
      return(x)
    }) %>%
    mutate_each(funs(as.numeric), lowbnd, obj_coef, uppbnd) %>%
    mutate(reversible = reversible=='true',
           lowbnd = ifelse(!is.na(lowbnd), lowbnd, -1000*reversible),
           uppbnd = ifelse(!is.na(uppbnd), uppbnd, 1000),
           obj_coef = ifelse(!is.na(obj_coef), obj_coef, 0)
    )
    
  
  reaction_stoich <- sbml[[1]] %>% 
    keep(function(x){'reaction' %in% names(x)}) %>% first %>% 
    keep(is.list) %>%
    map(function(x){c(x,attributes(x))}) %>% 
    map(keep, function(x){(is.atomic(x) & length(x) == 1) | ('speciesReference' %in% names(x))}) %>% 
    map(function(x){map_if(x,is.list, function(y){map(y,attributes)})}) %>%
    map(function(x){
      bind_rows(
        bind_rows(x[[1]]) %>% mutate(direction='in'),
        bind_rows(x[[2]]) %>% mutate(direction='out')
      ) %>%
        mutate(id=x[['id']],
               name=x[['name']],
               reversible=x[['reversible']])
    }) %>%
    bind_rows %>%
    mutate(stoichiometry = as.numeric(stoichiometry),
           stoichiometry= stoichiometry*(1-2*(direction=='in')))
  
  # Add boundary reactions
  boundary_species <- species %>% filter(boundaryCondition)
  boundary_reactions <- data_frame(rxnid=paste(sep='_', 'R', boundary_species$id, 'boundary'),
                                   name = rxnid,
                                   reversible = TRUE,
                                   sboTerm = NA,
                                   lowbnd = -1000,
                                   obj_coef = 0,
                                   uppbnd = 1000)
  boundary_reactions_stoich <- data_frame(species = str_replace(boundary_species$id, '^metaid_', ''),
                                          stoichiometry = 1,
                                          id = boundary_reactions$rxnid)
  
  reactions <- bind_rows(reactions, boundary_reactions)
  reaction_stoich <- rbind_list(reaction_stoich, boundary_reactions_stoich)
  
  # some checks
  assert_that(length(unique(reaction_stoich$id))==nrow(reactions))
  
  result <- list(compartments = compartments, 
                 stoichiometry = reaction_stoich,
                 reactions = reactions,
                 species = species)
  }