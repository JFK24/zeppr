test_that("growing_degree_days() works", {
  expect_equal(0.5, growing_degree_days(9, 12, t.ceiling=30, t.base=10, use.floor=FALSE))
  expect_equal(1, growing_degree_days(9, 12, t.ceiling=30, t.base=10, use.floor=TRUE))
  expect_equal(2.5, growing_degree_days(4, 20, t.ceiling=15, t.base=10, use.floor=TRUE))
  expect_equal(c(0.0, 1.0, 2.5), growing_degree_days(c(7, 8, 10), c(12, 14, 15), t.ceiling=30, t.base=10, use.floor=FALSE))
})

test_that("mutate_cumsum_gdd() works", {
  data <- data.frame(
    Date=as.Date(c("2022-01-01", "2022-01-02", "2022-01-02")),
    Tmin=c(4, 6, 11),
    Tmax=c(12, 14, 20))
  expect_equal(c(3.0, 18.5, 18.5), mutate_cumsum_gdd(data, Date, Tmin, Tmax)$cumsum_gdd)
  expect_equal(c(3.5, 19.0, 19.0), mutate_cumsum_gdd(data, Date, Tmin, Tmax, use.floor=TRUE)$cumsum_gdd)
})

