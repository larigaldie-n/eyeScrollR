test_that("Properly shifts a point inside the image", {
  coordinate <- 100
  shift_before <- 25
  shift_after <- 25
  screen_dimension <- 1000

  # With outside values discarded
  outside_image_is_na <- TRUE
  expect_equal(shift_image_by_dimension(coordinate, shift_before, shift_after, screen_dimension, outside_image_is_na), 75)

  # With outside values kept
  outside_image_is_na <- FALSE
  expect_equal(shift_image_by_dimension(coordinate, shift_before, shift_after, screen_dimension, outside_image_is_na), 75)
})

test_that("Properly handles a point before the image", {
  coordinate <- 10
  shift_before <- 25
  shift_after <- 25
  screen_dimension <- 1000

  # Points discarded outside the image
  outside_image_is_na <- TRUE
  expect_equal(shift_image_by_dimension(coordinate, shift_before, shift_after, screen_dimensio, outside_image_is_na), NA)

  # Points kept outside the image
  outside_image_is_na <- FALSE
  expect_equal(shift_image_by_dimension(coordinate, shift_before, shift_after, screen_dimensio, outside_image_is_na), 10-25)
})

test_that("Properly handles a point after the image", {
  coordinate <- 1100
  shift_before <- 25
  shift_after <- 25
  screen_dimension <- 1000

  # Points discarded outside the image
  outside_image_is_na <- TRUE
  expect_equal(shift_image_by_dimension(coordinate, shift_before, shift_after, screen_dimension, outside_image_is_na), NA)

  # Points kept outside the image
  outside_image_is_na <- FALSE
  expect_equal(shift_image_by_dimension(coordinate, shift_before, shift_after, screen_dimensio, outside_image_is_na), 1100-25)
})

test_that("Properly handles a point right after the image, when there is a small shift before", {
  coordinate <- 985
  shift_before <- 25
  shift_after <- 25
  screen_dimension <- 1000

  # Points discarded outside the image
  outside_image_is_na <- TRUE
  expect_equal(shift_image_by_dimension(coordinate, shift_before, shift_after, screen_dimension, outside_image_is_na), NA)

  # Points kept outside the image
  outside_image_is_na <- FALSE
  expect_equal(shift_image_by_dimension(coordinate, shift_before, shift_after, screen_dimensio, outside_image_is_na), 985-25)
})

test_that("Properly handles an NA coordinate", {
  coordinate <- NA
  shift_before <- 25
  shift_after <- 25
  screen_dimension <- 1000

  # Points discarded outside the image
  outside_image_is_na <- TRUE
  expect_equal(shift_image_by_dimension(coordinate, shift_before, shift_after, screen_dimension, outside_image_is_na), NA)

  # Points kept outside the image
  outside_image_is_na <- FALSE
  expect_equal(shift_image_by_dimension(coordinate, shift_before, shift_after, screen_dimensio, outside_image_is_na), NA)
})
