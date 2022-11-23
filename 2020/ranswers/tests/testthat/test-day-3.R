test_that("Day 3 P1 sample", {
  expect_equal(day3p1(system.file("day-3", "sample.txt", package="ranswers"),3,1), 7)
})
test_that("Day 3 P2 sample", {
  expect_equal(day3p2(system.file("day-3", "sample.txt", package="ranswers")), 336)
})
