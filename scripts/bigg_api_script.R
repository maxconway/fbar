# get gene associations
library(httr)
models <- GET('http://bigg.ucsd.edu/api/v2/models') %>%
  content(as='parsed') %>%
  getElement('results') %>%
  map_chr(getElement,'bigg_id')

genes <- models %>% plyr::ldply(function(model){
  GET(paste0('http://bigg.ucsd.edu/api/v2/models/',model,'/genes')) %>%
    content(as='parsed') %>%
    getElement('results') %>%
    discard(function(x){is.null(x$name)}) %>%
    map(as.data.frame, stringsAsFactors=FALSE) %>%
    bind_rows()
})
