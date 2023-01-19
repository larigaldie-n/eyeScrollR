test_that("Scroll gets corrected properly", {
  data_line <-
    list(
      Timestamp = 10,
      Data = "Id:M;X:356;Y:619;MouseEvent:WM_MOUSEWHEEL;ScrollDelta:-120;Button:0",
      Corrected.X = 1700,
      Corrected.Y = 150
    )
  event <- "WM_MOUSEWHEEL"
  scroll <- 200
  min_scroll <- 0
  max_scroll <- 1000
  scroll_pixels <- 100
  top_left_x <- 0
  top_left_y <- 0
  bottom_right_x <- 1920
  bottom_right_y <- 1080
  expect_equal(
    shift_scroll(
      event,
      data_line,
      scroll,
      min_scroll,
      max_scroll,
      scroll_pixels,
      top_left_x,
      top_left_y,
      bottom_right_x,
      bottom_right_y
    ),
    300
  )

  data_line <-
    list(
      Timestamp = 10,
      Data = "Id:M;X:356;Y:619;MouseEvent:WM_MOUSEWHEEL;ScrollDelta:120;Button:0",
      Corrected.X = 1700,
      Corrected.Y = 150
    )
  scroll_pixels <- 50
  expect_equal(
    shift_scroll(
      event,
      data_line,
      scroll,
      min_scroll,
      max_scroll,
      scroll_pixels,
      top_left_x,
      top_left_y,
      bottom_right_x,
      bottom_right_y
    ),
    150
  )
})

test_that("Scroll gets correctly stopped at extremas", {
  data_line <-
    list(
      Timestamp = 10,
      Data = "Id:M;X:356;Y:619;MouseEvent:WM_MOUSEWHEEL;ScrollDelta:-120;Button:0",
      Corrected.X = 1700,
      Corrected.Y = 150
    )
  event <- "WM_MOUSEWHEEL"
  scroll <- 950
  min_scroll <- 0
  max_scroll <- 1000
  scroll_pixels <- 100
  top_left_x <- 0
  top_left_y <- 0
  bottom_right_x <- 1920
  bottom_right_y <- 1080
  expect_equal(
    shift_scroll(
      event,
      data_line,
      scroll,
      min_scroll,
      max_scroll,
      scroll_pixels,
      top_left_x,
      top_left_y,
      bottom_right_x,
      bottom_right_y
    ),
    1000
  )

  data_line <-
    list(
      Timestamp = 10,
      Data = "Id:M;X:356;Y:619;MouseEvent:WM_MOUSEWHEEL;ScrollDelta:120;Button:0",
      Corrected.X = 1700,
      Corrected.Y = 150
    )
  scroll <- 50
  expect_equal(
    shift_scroll(
      event,
      data_line,
      scroll,
      min_scroll,
      max_scroll,
      scroll_pixels,
      top_left_x,
      top_left_y,
      bottom_right_x,
      bottom_right_y
    ),
    0
  )
})

test_that("Scroll does not get corrected when scroll is outside of the area", {
  data_line <-
    list(
      Timestamp = 10,
      Data = "Id:M;X:356;Y:619;MouseEvent:WM_MOUSEWHEEL;ScrollDelta:-120;Button:0",
      Corrected.X = 1700,
      Corrected.Y = 150
    )
  event <- "WM_MOUSEWHEEL"
  scroll <- 200
  min_scroll <- 0
  max_scroll <- 1000
  scroll_pixels <- 100
  top_left_x <- 357
  top_left_y <- 0
  bottom_right_x <- 1920
  bottom_right_y <- 1080
  expect_equal(
    shift_scroll(
      event,
      data_line,
      scroll,
      min_scroll,
      max_scroll,
      scroll_pixels,
      top_left_x,
      top_left_y,
      bottom_right_x,
      bottom_right_y
    ),
    200
  )

  top_left_x <- 0
  top_left_y <- 620
  expect_equal(
    shift_scroll(
      event,
      data_line,
      scroll,
      min_scroll,
      max_scroll,
      scroll_pixels,
      top_left_x,
      top_left_y,
      bottom_right_x,
      bottom_right_y
    ),
    200
  )

  top_left_y <- 0
  bottom_right_x <- 355
  expect_equal(
    shift_scroll(
      event,
      data_line,
      scroll,
      min_scroll,
      max_scroll,
      scroll_pixels,
      top_left_x,
      top_left_y,
      bottom_right_x,
      bottom_right_y
    ),
    200
  )

  bottom_right_x <- 1920
  bottom_right_y <- 618
  expect_equal(
    shift_scroll(
      event,
      data_line,
      scroll,
      min_scroll,
      max_scroll,
      scroll_pixels,
      top_left_x,
      top_left_y,
      bottom_right_x,
      bottom_right_y
    ),
    200
  )
})
