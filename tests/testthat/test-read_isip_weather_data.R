test_that("test of read_isip_weather_data()", {
  file.name <- "20221215_isip_hourly_weather_data_export.xlsx"
  path <- system.file("extdata", file.name, package = "zeppr")
  expect_true(file.exists(path))
  # Test hourly data:
  hourly.table <- read_isip_weather_data(path)
  expect_s3_class(hourly.table$date, "POSIXct")
  expect_equal(c(4320, 10), dim(hourly.table))
  expect_equal(20.97, round(max(hourly.table$Tavg), 2))
  expect_equal(-4.34, round(min(hourly.table$Tavg), 2))
  # Test hourly data converted to daily data:
  daily.table <- read_isip_weather_data(path, hourly.to.daily=TRUE)
  expect_s3_class(daily.table$date, "Date")
  expect_equal(c(180, 10), dim(daily.table))
  expect_equal(11.84, round(max(daily.table$Tavg), 2))
  expect_equal(-1.46, round(min(daily.table$Tavg), 2))
  # Test daily data
  file.name.2 <- "20230330_isip_daily_weather_data_export.xlsx"
  path.2 <- system.file("extdata", file.name.2, package = "zeppr")
  expect_true(file.exists(path.2))
  daily.table <- read_isip_weather_data(path, read.daily.data=TRUE)
  expect_s3_class(daily.table$date, "Date")
  expect_equal(c(4320, 10), dim(daily.table))
  expect_equal(20.97, round(max(daily.table$Tavg), 2))
  expect_equal(-4.34, round(min(daily.table$Tavg), 2))
  })

test_that("test of mutate_isip_weather_with_cumsum_gdd()",{
  file <- "20221215_isip_hourly_weather_data_export.xlsx"
  path <- system.file("extdata", file, package = "zeppr")
  expect_true(file.exists(path))
  # Test hourly data:
  hourly.table <- read_isip_weather_data(path)[1:3,]
  expect_equal(
    c(0.2, 0.39, 0.56),
    round(mutate_isip_weather_with_cumsum_gdd(hourly.table)$cumsum_gdd, 2))
  # expect_equal(
  #   c(0.564, 0.564, 0.564),
  #   round(mutate_isip_weather_with_cumsum_gdd(hourly.table, max.per.day=TRUE)$cumsum_gdd, 3))
  # Test hourly data converted to daily data:
  daily.table <- read_isip_weather_data(path, hourly.to.daily=TRUE)[1:3,]
  expect_equal(
    c(4.48, 7.64, 13.89),
    round(mutate_isip_weather_with_cumsum_gdd(daily.table, daily.data=TRUE)$cumsum_gdd, 2))
  expect_equal(
    c(4.56, 8.48, 14.74),
    round(mutate_isip_weather_with_cumsum_gdd(daily.table, daily.data=TRUE, use.floor=TRUE)$cumsum_gdd, 2))

  # Test daily data
  file.name.2 <- "20230330_isip_daily_weather_data_export.xlsx"
  path.2 <- system.file("extdata", file.name.2, package = "zeppr")
  daily.table.2 <- read_isip_weather_data(path.2, read.daily=TRUE)[1:3,]
  expect_equal(
    c(4.37, 7.21, 13.39),
    round(mutate_isip_weather_with_cumsum_gdd(daily.table.2, daily.data=TRUE, show.warnings=FALSE)$cumsum_gdd, 2))
  })
