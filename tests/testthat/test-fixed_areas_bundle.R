test_that("Properly returns error if no arguments or non-paired arguments", {
  expect_error(
    fixed_areas_bundle(),
    "You must pass screen-to-image mapping pairs of coordinate vectors to this function"
  )
  expect_error(
    fixed_areas_bundle(c(0, 0, 1, 2)),
    "You must pass screen-to-image mapping pairs of coordinate vectors to this function"
  )
})

test_that("Properly returns error if arguments are not vectors, or are not of length 4",
          {
            expect_error(
              fixed_areas_bundle(1, 2),
              "Every argument should be a coordinate vector in the form: c(top_left_x, top_left_y, bottom_right_x, bottom_right_y)",
              fixed = TRUE
            )
            expect_error(
              fixed_areas_bundle(c(0, 0, 1), c(5, 3, 6, 5)),
              "Every argument should be a coordinate vector in the form: c(top_left_x, top_left_y, bottom_right_x, bottom_right_y)",
              fixed = TRUE
            )
          })

test_that("Properly returns an array of fixed areas bundle", {
  expect_equal(fixed_areas_bundle(c(0, 0, 1, 2), c(0, 0, 3, 4)), array(c(c(0, 0, 1, 2), c(0, 0, 3, 4)), dim = c(4, 2, 1)))
  expect_equal(fixed_areas_bundle(
    c(0, 0, 1, 2),
    c(0, 0, 3, 4),
    c(5, 6, 7, 8),
    c(8, 5, 6, 8),
    c(10, 11, 12, 13),
    c(14, 15, 16, 17)
  ), array(c(
    c(0, 0, 1, 2),
    c(0, 0, 3, 4),
    c(5, 6, 7, 8),
    c(8, 5, 6, 8),
    c(10, 11, 12, 13),
    c(14, 15, 16, 17)
  ), dim = c(4, 2, 3)))
})
