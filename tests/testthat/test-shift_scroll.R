test_that("Scroll gets corrected properly", {
  data_line <- list(Timestamp = 10, Data = "Id:M;X:356;Y:619;MouseEvent:WM_MOUSEWHEEL;ScrollDelta:-120;Button:0", Corrected.X = 1700, Corrected.Y = 150)
  event <- "WM_MOUSEWHEEL"
  scroll <- 200
  min_scroll <- 0
  max_scroll <- 1000
  scroll_pixels <- 100
  expect_equal(shift_scroll(event, data_line, scroll, min_scroll, max_scroll, scroll_pixels), 300)

  data_line <- list(Timestamp = 10, Data = "Id:M;X:356;Y:619;MouseEvent:WM_MOUSEWHEEL;ScrollDelta:120;Button:0", Corrected.X = 1700, Corrected.Y = 150)
  expect_equal(shift_scroll(event, data_line, scroll, min_scroll, max_scroll, scroll_pixels), 100)
})

test_that("Scroll gets correctly stopped at extremas", {
  data_line <- list(Timestamp = 10, Data = "Id:M;X:356;Y:619;MouseEvent:WM_MOUSEWHEEL;ScrollDelta:-120;Button:0", Corrected.X = 1700, Corrected.Y = 150)
  event <- "WM_MOUSEWHEEL"
  scroll <- 950
  min_scroll <- 0
  max_scroll <- 1000
  scroll_pixels <- 100
  expect_equal(shift_scroll(event, data_line, scroll, min_scroll, max_scroll, scroll_pixels), 1000)

  data_line <- list(Timestamp = 10, Data = "Id:M;X:356;Y:619;MouseEvent:WM_MOUSEWHEEL;ScrollDelta:120;Button:0", Corrected.X = 1700, Corrected.Y = 150)
  scroll <- 50
  expect_equal(shift_scroll(event, data_line, scroll, min_scroll, max_scroll, scroll_pixels), 0)
})
