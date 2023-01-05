test_that("test of read_isip_hourly_weather_data()", {
  file.name <- "20221215_isip_hourly_weather_data_export.xlsx"
  path <- system.file("extdata", file.name, package = "zeppr")
  expect_true(file.exists(path))
  # Test hourly data:
  hourly.table <- read_isip_hourly_weather_data(path)
  expect_s3_class(hourly.table$date, "POSIXct")
  expect_equal(c(4320, 10), dim(hourly.table))
  expect_equal(20.97, round(max(hourly.table$Tavg), 2))
  expect_equal(-4.34, round(min(hourly.table$Tavg), 2))
  # Test daily data:
  daily.table <- read_isip_hourly_weather_data(path, returns.daily.data=TRUE)
  expect_s3_class(daily.table$date, "Date")
  expect_equal(c(180, 10), dim(daily.table))
  expect_equal(11.84, round(max(daily.table$Tavg), 2))
  expect_equal(-1.46, round(min(daily.table$Tavg), 2))
})

test_that("test of mutate_isip_weather_with_cumsum_gdd()",{
  file <- "20221215_isip_hourly_weather_data_export.xlsx"
  path <- system.file("extdata", file, package = "zeppr")
  expect_true(file.exists(path))
  # Test hourly data:
  hourly.table <- read_isip_hourly_weather_data(path)[1:3,]
  expect_equal(
    c(0.2, 0.39, 0.56),
    round(mutate_isip_weather_with_cumsum_gdd(hourly.table)$cumsum_gdd, 2))
  expect_equal(
    c(0.564, 0.564, 0.564),
    round(mutate_isip_weather_with_cumsum_gdd(hourly.table, max.per.day=TRUE)$cumsum_gdd, 3))
  # Test daily data:
  daily.table <- read_isip_hourly_weather_data(path, returns.daily.data=TRUE)[1:3,]
  expect_equal(
    c(4.48, 7.64, 13.89),
    round(mutate_isip_weather_with_cumsum_gdd(daily.table, daily.data=TRUE)$cumsum_gdd, 2))
  expect_equal(
    c(4.55, 8.48, 14.73),
    round(mutate_isip_weather_with_cumsum_gdd(daily.table, daily.data=TRUE, use.floor=TRUE)$cumsum_gdd, 2))
})
