test_that("Day 1 Part 1 Sample", {
  expect_equal(day1(system.file("day-1", "sample.txt", package = "ranswers")), 514579)
})
test_that("Day 1 Part 2 Sample", {
  expect_equal(day1p2(system.file("day-1", "sample.txt", package = "ranswers")), 241861950)
})
