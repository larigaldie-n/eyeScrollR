test_that("Rules are correctly enforced", {
  flags <- c(TRUE, FALSE, TRUE)
  fixed_areas <- list(array(c(c(0,0,1902,95), c(0,0,1902,95), c(1607,96,1902,905), c(1607,96,1902,905)), dim = c(4,2,2)), array(c(c(0,0,1902,63), c(0,32,1902,95), c(1607,63,1902,900), c(1607,63,1902,900)), dim = c(4,2,2)), array(c(c(1607,906,1920,1080), c(1607,6481,1920,6655)), dim = c(4,2,1)))
  # inside fixed area
  data_line <- list(Timestamp = 10, Data = "mouseclickorwhatever", Corrected.X = 5, Corrected.Y = 10)
  expect_equal(enforce_rules(flags, fixed_areas, data_line), 10)
  data_line <- list(Timestamp = 10, Data = "mouseclickorwhatever", Corrected.X = 1700, Corrected.Y = 150)
  expect_equal(enforce_rules(flags, fixed_areas, data_line), 150)

  # inside fixed area that "moves"
  data_line <- list(Timestamp = 10, Data = "mouseclickorwhatever", Corrected.X = 1700, Corrected.Y = 1000)
  expect_equal(enforce_rules(flags, fixed_areas, data_line), 6575)

  # outside any fixed area
  data_line <- list(Timestamp = 10, Data = "mouseclickorwhatever", Corrected.X = 1500, Corrected.Y = 300)
  expect_equal(enforce_rules(flags, fixed_areas, data_line), NA)
})
