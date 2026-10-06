library(testthat)
library(didFF)

#-----------------------------------------------------------------------------
# distDD keeps every bin in its table when one of them is dropped from the
# covariance for having a degenerate influence function.
data(mpdta, package = "did")

run_distDD <- function(...) {
  suppressWarnings(
    distDD(
      data          = mpdta,
      yname         = "lemp",
      tname         = "year",
      idname        = "countyreal",
      gname         = "first.treat",
      control_group = "nevertreated",
      nbins         = 6,
      seed          = 0,
      aggte_type    = "dynamic",
      ...
    )
  )
}

test_that("distDD returns one row per bin when a bin is dropped", {
  full    <- run_distDD()
  dropped <- run_distDD(balance_e = 1)

  expect_equal(NROW(full$table), 6)
  expect_false(anyNA(full$table$test.se))

  expect_equal(NROW(dropped$table), 6)
  expect_equal(as.character(dropped$table$level), as.character(full$table$level))
  expect_equal(sum(is.na(dropped$table$test.se)), 1)
  expect_true(all(dropped$table$test.se[!is.na(dropped$table$test.se)] > 0))
  expect_false(anyNA(dropped$table$test.estimates))
})
