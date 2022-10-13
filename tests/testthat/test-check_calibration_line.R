test_that("Properly finds different types of line", {
  red <- c(1,0,0)
  green <- c(0,1,0)
  blue <- c(0,0,1)
  other <- c(1,1,0)
  line <- matrix(c(other, red, green, blue, other), nrow = 5, byrow = TRUE)
  coordinate <- 2
  reverse <- FALSE
  expect_equal(check_calibration_line(line, coordinate, reverse), TRUE)
  line <- matrix(c(other, rep(red,2), rep(green,2), rep(blue,2), other), nrow = 8, byrow = TRUE)
  expect_equal(check_calibration_line(line, coordinate, reverse), TRUE)

  line <- matrix(c(other, blue, green, red, other), nrow = 5, byrow = TRUE)
  coordinate <- 4
  reverse <- TRUE
  expect_equal(check_calibration_line(line, coordinate, reverse), TRUE)
  line <- matrix(c(other, rep(blue,2), rep(green,2), rep(red,2), other), nrow = 8, byrow = TRUE)
  coordinate <- 7
  expect_equal(check_calibration_line(line, coordinate, reverse), TRUE)
})

test_that("Properly avoids when it's not the first/last red in the line", {
  red <- c(1,0,0)
  green <- c(0,1,0)
  blue <- c(0,0,1)
  other <- c(1,1,0)
  coordinate <- 3
  reverse <- FALSE
  line <- matrix(c(other, rep(red,2), rep(green,2), rep(blue,2), other), nrow = 8, byrow = TRUE)
  expect_equal(check_calibration_line(line, coordinate, reverse), FALSE)

  reverse <- TRUE
  line <- matrix(c(other, rep(blue,2), rep(green,2), rep(red,2), other), nrow = 8, byrow = TRUE)
  coordinate <- 6
  expect_equal(check_calibration_line(line, coordinate, reverse), FALSE)
})

test_that("Properly avoids when it's not in the red/green/blue order", {
  red <- c(1,0,0)
  green <- c(0,1,0)
  blue <- c(0,0,1)
  other <- c(1,1,0)
  line <- matrix(c(other, red, blue, green, other), nrow = 5, byrow = TRUE)
  coordinate <- 2
  reverse <- FALSE
  expect_equal(check_calibration_line(line, coordinate, reverse), FALSE)
  line <- matrix(c(other, rep(red,2), rep(blue,2), rep(green,2), other), nrow = 8, byrow = TRUE)
  expect_equal(check_calibration_line(line, coordinate, reverse), FALSE)

  line <- matrix(c(other, green, blue, red, other), nrow = 5, byrow = TRUE)
  coordinate <- 4
  reverse <- TRUE
  expect_equal(check_calibration_line(line, coordinate, reverse), FALSE)
  line <- matrix(c(other, rep(green,2), rep(blue,2), rep(red,2), other), nrow = 8, byrow = TRUE)
  coordinate <- 7
  expect_equal(check_calibration_line(line, coordinate, reverse), FALSE)
})
