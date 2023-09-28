test_that("year without leap month", {
  expect_equal("2001-01-23", lunar_to_gregorian("2000-12-29"))
})

test_that("year with leap month", {
  expect_equal("2002-02-10", lunar_to_gregorian("2001-12-29"))
})
