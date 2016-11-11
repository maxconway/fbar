library(purrr)
library(tidyr)
library(readr)
library(dplyr)
library(stringr)
c(iBif452= 'raw/models/iBif452.V01.00/excelRxns.txt',
  iMsi385='raw/models/iMsi385/excelRxns.txt',
  iEre400='raw/models/iEre400 v1.00/excelRxns.txt',
  iFap484='raw/models/iFap484.V01.00/excelRxns.txt',
  iBth801='raw/models/iBth801 v1.00/excelRxns.txt') %>%
  purrr::map_df(readr::read_tsv, col_types='_ccc_cddccc__', .id='model') %>%
  rename(abbreviation=ID,	officialName=NAME,	equation=EQUATION,	lowbnd=`LOWER BOUND`,	uppbnd=`UPPER BOUND`,	obj_coef=OBJECTIVE) %>%
  mutate(obj_coef = as.numeric(obj_coef),
         obj_coef = replace(obj_coef, is.na(obj_coef),0)) %>%
  mutate(equation = str_replace_all(equation, fixed('glycerol teichoic acid (n=25), unlinked, unsubstituted'), 'GTA25_u_s'),
         equation = str_replace_all(equation, fixed('glycerol teichoic acid (n=25), unlinked, D-ala substituted'), 'GTA25_u_D-ala'),
         equation = str_replace_all(equation, fixed('glycerol teichoic acid (n=25), unlinked, glucose substituted'), 'GTA25_u_G'),
         equation = str_replace_all(equation, fixed('Peptidylproline (omega=180)'), 'Peptidylproline_omega_180'),
         equation = str_replace_all(equation, fixed('Peptidylproline (omega=0)'), 'Peptidylproline_omega_0')) %>%
  filter(equation!='<=>') %>%
  mutate(group=model) %>%
  nest(-group) %>%
  pwalk(function(group,data){write_tsv(data,paste0('inst/extdata/',group[1],'.tsv'))})