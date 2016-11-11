list.files('inst/extdata/','i[0-9a-zA-Z]+.tsv', full.names = TRUE) %>%
  discard(stringr::str_detect, 'iAF1260|iJO1366') %>%
  map(read_tsv) -> models

models %>%
  bind_rows() %>%
  mutate(equation = str_replace_all(equation, fixed('glycerol teichoic acid (n=25), unlinked, unsubstituted'), 'GTA25_u_s'),
         equation = str_replace_all(equation, fixed('glycerol teichoic acid (n=25), unlinked, D-ala substituted'), 'GTA25_u_D-ala'),
         equation = str_replace_all(equation, fixed('glycerol teichoic acid (n=25), unlinked, glucose substituted'), 'GTA25_u_G'),
         equation = str_replace_all(equation, fixed('Peptidylproline (omega=180)'), 'Peptidylproline_omega_180'),
         equation = str_replace_all(equation, fixed('Peptidylproline (omega=0)'), 'Peptidylproline_omega_0')) %>%
  filter(equation!='<=>') %>%
  nest(-model) %>%
  rename(model=data,name=model) %>%
  mutate(model = pmap(.,defactor_compartments)) %>%
  unnest %>%
  filter(!(abbreviation %in% 'iFap484_FE3HOXexs')) %>%
  validate_model_equations() %>%
  wrap_exchanges('outside') %>%
  add_exchange_reactions('outside') %>% 
  validate_model_equations() -> combined_model

population <- combined_model %>% mutate(flux=gurobi(parse_reaction_table(.))$x)
log <- population %>% 
  filter(obj_coef!=0) %>%
  mutate(gen = 0) %>%
  select(name, flux)
for(gen in 1:100){
  # simulate
  tryCatch(population <- population %>% mutate(flux=gurobi(parse_reaction_table(.))$x), error=break)
  # find growth
  growth <- population %>% 
    filter(obj_coef!=0) %>%
    mutate(propflux = flux/sum(flux)) %>%
    select(name, propflux)
  # update
  population <- inner_join(population, growth) %>%
           uppbnd = uppbnd*propflux) %>%
    mutate(lowbnd = lowbnd*propflux,
    select(-propflux)
  # log
  log <- bind_rows(log, population %>% 
                     filter(obj_coef!=0) %>%
                     mutate(gen = gen) %>%
                     select(name, flux))
}
