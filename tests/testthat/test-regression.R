context("regression")

## TODO: Rename context
## TODO: Add more tests

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("online ecoli model works", {
  skip_on_cran()
  gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1XdpAKFyEpGjPmI3UZYYK4zQw-RUzKWD2_GJnp3LO_Pk/edit?usp=sharing') %>%
    parse_reaction_table()
})
