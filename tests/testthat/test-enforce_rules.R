test_that("Rules are correctly enforced", {
  flags <- c(TRUE, FALSE, TRUE)
  anchors <- list(array(c(c(0,0,1902,95), c(0,0,1902,95), c(1607,96,1902,905), c(1607,96,1902,905)), dim = c(4,2,2)), array(c(c(0,0,1902,63), c(0,32,1902,95), c(1607,63,1902,900), c(1607,63,1902,900)), dim = c(4,2,2)), array(c(c(1607,906,1920,1080), c(1607,6481,1920,6655)), dim = c(4,2,1)))
  # inside static anchor
  data_line <- list(timestamp = 10, event = "mouseclickorwhatever", corrected_x = 5, corrected_y = 10)
  expect_equal(enforce_rules(flags, anchors, data_line), 10)
  data_line <- list(timestamp = 10, event = "mouseclickorwhatever", corrected_x = 1700, corrected_y = 150)
  expect_equal(enforce_rules(flags, anchors, data_line), 150)

  # inside anchor that moves
  data_line <- list(timestamp = 10, event = "mouseclickorwhatever", corrected_x = 1700, corrected_y = 1000)
  expect_equal(enforce_rules(flags, anchors, data_line), 6575)

  # outside any anchor
  data_line <- list(timestamp = 10, event = "mouseclickorwhatever", corrected_x = 1500, corrected_y = 300)
  expect_equal(enforce_rules(flags, anchors, data_line), NA)
})
