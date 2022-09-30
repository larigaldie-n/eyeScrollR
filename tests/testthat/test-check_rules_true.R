test_that("Properly modifies the rule flags", {
  scroll <- 20
  rules <- list(rule_before_scrolling, rule_after_scrolling, rule_true)
  flags <- c(TRUE, TRUE, TRUE)
  anchors <- list(array(c(c(0,0,1902,95), c(0,0,1902,95), c(1607,96,1902,905), c(1607,96,1902,905)), dim = c(4,2,2)), array(c(c(0,0,1902,63), c(0,32,1902,95), c(1607,63,1902,900), c(1607,63,1902,900)), dim = c(4,2,2)), array(c(c(1607,906,1920,1080), c(1607,6481,1920,6655)), dim = c(4,2,1)))
  data_line <- list(timestamp = 10, event = "mouseclickorwhatever", corrected_x = 5, corrected_y = 10)
  expect_equal(check_rules_true(rules, data_line, flags, anchors, scroll), c(TRUE, FALSE, TRUE))

  scroll <- 40
  expect_equal(check_rules_true(rules, data_line, flags, anchors, scroll), c(FALSE, TRUE, TRUE))
})
