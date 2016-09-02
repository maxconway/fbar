context("metabolite-parsing")

## TODO: Rename context
## TODO: Add more tests

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("tricky equations split correctly", {
  tricky_equations <- c("peptidylproline (omega=180) <=> peptidylproline (omega=0)",
                        "2 o2.- + 2 h+ <=> hydrogen peroxide + oxygen",
                        "beta-d-galactosyl-(1->4)-beta-d-glucosyl-(1<->1)-ceramide + h2o <=> beta-d-glucosyl-(1<->1)-ceramide + d-galactose")
  FluxBalanceAnalyzeR:::split_on_arrow(tricky_equations, pattern_arrow = '[[:space:]]*<?[=]+>[[:space:]]*')
})