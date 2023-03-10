test_that("day_temperature_index", {
  expect_equal(day_temperature_index(t.min=12, t.max=32, index="hot"), TRUE)
  expect_equal(day_temperature_index(t.min=c(12, 15), t.max=c(15, 30), index="heating"), c(TRUE, FALSE))
})

test_that("mutate_day_temperature_index", {
  data <- data.frame(
    Date=as.Date(c("2022-01-01", "2022-01-02", "2022-01-02")),
    Tmin=c(4, 6, 11),
    Tmax=c(12, 14, 20))
  expect_equal(mutate_day_temperature_index(data, Tmin, Tmax)$heating, c(TRUE, TRUE, FALSE))
  expect_equal(ncol(mutate_day_temperature_index(data, Tmin, Tmax, indices=c("frost", "heating"))), 5)
  expect_equal(mutate_day_temperature_index(data, Tmin, Tmax, german.output=T)$Heiztag, c(TRUE, TRUE, FALSE))
})

