toload <- data_frame(filename=paste(list.files('raw/models', pattern = '^i.+', full.names = TRUE), 'excelRxns.txt',sep='/'),
                     name = c('iBif452','iBth801','iEre400','iFap484','iMsi385'))


map2(toload$filename, toload$name, function(filename, modelname){
  read_lines(filename) %>%
    str_replace_all('^#','') %>%
    str_replace_all('^\t|\t$','') %>%
    paste0(collapse='\n') %>%
    read_tsv() %>%
    select(abbreviation=ID,
           name=NAME,
           equation=EQUATION,
           lowbnd=`LOWER BOUND`,
           uppbnd=`UPPER BOUND`,
           obj_coef=`OBJECTIVE`,
           genes=`GENE ASSOCIATION`
           ) %>%
    defactor_compartments(name=modelname) %>%
    mutate(model=modelname)
  }
) %>% 
  bind_rows %>%
  mutate(abbreviation = paste0(model,': ',abbreviation)) -> joined_model
  

# clean
joined_model <- joined_model %>%
  filter(!str_detect(equation, '^[[:space:]]*<?=>[[:space:]]*$')) %>%
  mutate(obj_coef=0,
         lowbnd = pmin(lowbnd,0),
         equation = str_replace_all(equation, ' +',' '))


with(joined_model, joined_model[model=='iBif452' & abbreviation=='Biomass','equation'] <- paste(joined_model[model=='iBif452' & abbreviation=='Biomass','equation'],'biomass_bif[iBif452_e]'))
with(joined_model, joined_model[model=='iFap484' & abbreviation=='Biomass','equation'] <- paste(joined_model[model=='iFap484' & abbreviation=='Biomass','equation'],'biomass_bif[iFap484_e]'))

joined_model <- bind_rows(joined_model, data_frame(model=c('iBif452', 'iFap484'),
                                                   abbreviation=c('bmOut_bif', 'bmOut_fap'),
                                                   equation = c('biomass_bif[iBif452_e] => ', 'biomass_fap[iFap484_e] => '),
                                                   lowbnd=0, 
                                                   uppbnd=1000, 
                                                   obj_coef=0)
                          )

joined_model <- bind_rows(joined_model %>% filter(!str_detect(equation, '^[[:space:]]*<?=>'), !str_detect(equation, '<?=>[[:space:]]*$')),
          joined_model %>% 
            filter(str_detect(equation, '<?=>[[:space:]]*$')) %>%
            transmute(equation = paste(equation, equation %>% 
                                      str_extract('[^<=>[:space:]]+ ') %>% 
                                      str_replace('\\[\\w+] ','[E]')
                                    ),
                      abbreviation = paste(model,'2 global', abbreviation),
                      model='global',
                   lowbnd = -1000,
                   uppbnd = 1000,
                   obj_coef = 0
                   ),
          joined_model %>% 
            filter(str_detect(equation, '^[[:space:]]*<?=>')) %>%
            transmute(equation = paste(equation %>% 
                                      str_extract('[^<=>[:space:]]+') %>% 
                                      str_replace('\\[\\w+]$','[E]'),
                                    equation
                                    ),
                      abbreviation = paste(model,'2 global', abbreviation),
                  model='global',
                  lowbnd = -1000,
                  uppbnd = 1000,
                  obj_coef = 0                  )
) %>% mutate(equation = str_replace_all(equation, ' +', ' '))
  
global <- read_tsv('raw/models/global/Exchange reactions - Sheet1.tsv')

support_reactions <- bind_rows(
  data_frame(
    model = 'support',
    metabolite = list_metabolites_withcompartment(joined_model),
    abbreviation = paste('support introduction',metabolite),
    name = abbreviation,
    equation = paste('=>',metabolite),
    lowbnd = 0, uppbnd = 1000,
    obj_coef = -1.5
  ),
  data_frame(
    model = 'support',
    metabolite = list_metabolites_withcompartment(joined_model),
    abbreviation = paste('support removal',metabolite),
    name = abbreviation,
    equation = paste(metabolite,'=>'),
    lowbnd = 0, uppbnd = 1000,
    obj_coef = -1.5
  )
) %>%
  filter(!str_detect(metabolite,'[bB]iomass')) 

joined_model <- bind_rows(joined_model, global, support_reactions) %>% 
  mutate(equation = str_replace_all(equation, ' +', ' '))

joined_model %>% parse_reaction_table() %>% gurobi() -> res;
joined_model %>% 
  mutate(flux=res$x) %>%
  filter(model=='support') %>%
  filter(flux>0) %>%
  select(abbreviation, equation, flux) %>% View
