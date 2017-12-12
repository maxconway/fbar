context("utilities")

addresses <- c('http://bigg.ucsd.edu/static/models/iJO1366.json',
               'http://bigg.ucsd.edu/static/models/iAF1260.json',
               'http://bigg.ucsd.edu/api/v2/models/iND750/download',
               'http://bigg.ucsd.edu/static/models/e_coli_core.json'
)

for (address in addresses) {
  test_that(paste("Download produces no errors with", address), {
    skip_if_not_installed('jsonlite')
    expect_silent(get_BiGG(address))
  })
  
  test_that(paste("Result can be deparsed to reactiontbl without errors with", address), {
    skip_if_not_installed('jsonlite')
    expect_silent(expanded_to_reactiontbl(get_BiGG(address)))
  })
}
