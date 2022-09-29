test_that("Properly shifts a point inside the image", {
  coordinate <- 100
  shift_before <- 25
  shift_after <- 25
  screen_dimension <- 1000
  expect_equal(shift_image_by_dimension(coordinate, shift_before, shift_after, screen_dimension), 75)
})

test_that("Properly excludes a point before the image", {
  coordinate <- 10
  shift_before <- 25
  shift_after <- 25
  screen_dimension <- 1000
  expect_equal(shift_image_by_dimension(coordinate, shift_before, shift_after, screen_dimension), NA)
})

test_that("Properly excludes a point far after the image", {
  coordinate <- 1100
  shift_before <- 25
  shift_after <- 25
  screen_dimension <- 1000
  expect_equal(shift_image_by_dimension(coordinate, shift_before, shift_after, screen_dimension), NA)
})

test_that("Properly excludes a point right after the image, when there is a small shift before", {
  coordinate <- 985
  shift_before <- 25
  shift_after <- 25
  screen_dimension <- 1000
  expect_equal(shift_image_by_dimension(coordinate, shift_before, shift_after, screen_dimension), NA)
})
