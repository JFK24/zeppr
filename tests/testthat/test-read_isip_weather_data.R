test_that("read_isip_hourly_weather_data() works", {
  file.name <- "20221215_isip_hourly_weather_data_export.xlsx"
  path <- system.file("extdata", file.name, package = "zeppr")
  # Test hourly data:
  hourly.table <- read_isip_hourly_weather_data(path)
  expect_equal(c(4320, 10), dim(hourly.table))
  expect_equal(20.97, round(max(hourly.table$temperature), 2))
  expect_equal(-4.34, round(min(hourly.table$temperature), 2))
  # Test daily data:
  daily.table <- read_isip_hourly_weather_data(path, returns.daily.data=TRUE)
  expect_equal(c(180, 10), dim(daily.table))
  expect_equal(11.84, round(max(daily.table$temperature), 2))
  expect_equal(-1.46, round(min(daily.table$temperature), 2))
})
