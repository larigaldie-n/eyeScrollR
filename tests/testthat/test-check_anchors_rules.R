test_that("Properly handles the functions list", {
  test_anchors <- list(array(c(c(0,0,1902,95), c(0,0,1902,95), c(1607,96,1902,905), c(1607,96,1902,905)), dim = c(4,2,2)), array(c(c(0,0,1902,63), c(0,32,1902,95), c(1607,63,1902,900), c(1607,63,1902,900)), dim = c(4,2,2)), array(c(c(1607,906,1920,1080), c(1607,6481,1920,6655)), dim = c(4,2,1)))
  test_rules <- list()
  expect_equal(check_anchors_rules(test_anchors, test_rules), list(rule_true, rule_true, rule_true))

  test_rules <- list(rule_before_scrolling, rule_after_scrolling)
  expect_equal(check_anchors_rules(test_anchors, test_rules), list(rule_before_scrolling, rule_after_scrolling, rule_true))

  test_rules <- list(rule_before_scrolling)
  expect_equal(check_anchors_rules(test_anchors, test_rules), list(rule_before_scrolling, rule_true, rule_true))
})

test_that("Properly handles common errors", {
  test_anchors <- list(array(c(c(0,0,1902,95), c(0,0,1902,95), c(1607,96,1902,905), c(1607,96,1902,905)), dim = c(4,2,2)), array(c(c(0,0,1902,63), c(0,32,1902,95), c(1607,63,1902,900), c(1607,63,1902,900)), dim = c(4,2,2)), array(c(c(1607,906,1920,1080), c(1607,6481,1920,6655)), dim = c(4,2,1)))
  test_rules <- c(1,2,3)
  expect_error(check_anchors_rules(test_anchors, test_rules), "rules must be an empty list or a list of functions")

  test_rules <- list(rule_true, rule_true, 63)
  expect_error(check_anchors_rules(test_anchors, test_rules), "rules must be an empty list or a list of functions")

  test_rules <- list(rule_before_scrolling, rule_after_scrolling, rule_true, rule_true)
  expect_error(check_anchors_rules(test_anchors, test_rules), "There can't be more rules than anchor arrays")

  test_anchors <- c()
  expect_error(check_anchors_rules(test_anchors, test_rules), "anchors must be an empty list or a list of arrays")

  test_anchors <- list(array(c(c(0,0,1902,95), c(0,0,1902,95), c(1607,96,1902,905), c(1607,96,1902,905)), dim = c(4,2,2)), 5, array(c(c(1607,906,1920,1080), c(1607,6481,1920,6655)), dim = c(4,2,1)))
  expect_error(check_anchors_rules(test_anchors, test_rules), "anchors must be an empty list or a list of arrays")

  test_anchors <- list(array(c(c(0,0,1902,95), c(0,0,1902,95), c(1607,96,1902,905), c(1607,96,1902,905)), dim = c(5,2,2)), array(c(c(1607,906,1920,1080), c(1607,6481,1920,6655)), dim = c(4,2,1)))
  expect_error(check_anchors_rules(test_anchors, test_rules), "anchor arrays must be 4x2 matrices of coordinates")

  test_anchors <- list(array(c(c(0,0,1902,96), c(0,0,1902,95), c(1607,96,1902,905), c(1607,96,1902,905)), dim = c(4,2,2)), array(c(c(1607,906,1920,1080), c(1607,6481,1920,6655)), dim = c(4,2,1)))
  expect_error(check_anchors_rules(test_anchors, test_rules), "origin (screen) and destination (image) anchor rectangles must be the same size", fixed=TRUE)
})
