test_that("Properly catches a point in one of the matrices", {
  array_test <- array(c(c(0,0,1902,95), c(0,0,1902,95), c(1607,96,1920,905), c(1607,96,1920,905)), dim = c(4,2,2))
  x <- 500
  y <- 50
  expect_equal(resolve_fixed_box(array_test, x, y), 50)

  x <- 1750
  y <- 500
  expect_equal(resolve_fixed_box(array_test, x, y), 500)

  x <- 500
  y <- 500
  expect_equal(resolve_fixed_box(array_test, x, y), NA)
})

test_that("Properly shifts the y coordinate when a shift has to be done", {
  array_test <- array(c(c(1607,906,1920,1080), c(1607,6481,1920,6655)), dim = c(4,2,1))
  x <- 1700
  y <- 1000
  expect_equal(resolve_fixed_box(array_test, x, y), 6575)

  array_test <- array(c(c(0,0,1902,95), c(0,0,1902,95), c(1607,96,1920,905), c(1607,96,1920,905)), dim = c(4,2,2))
  x <- 1713.74219249536
  y <- 355.783264033507
  expect_equal(resolve_fixed_box(array_test, x, y), 355.783264033507 + 0)
})
