test_that("test of dwd_category_code()", {
  expect_equal(dwd_category_code("air_temperature"), "TU")
  expect_equal(dwd_category_code("dew_point"), "TD")
})

test_that("test of read_dwd_stations_info_file()", {
  file.name <- "TU_Stundenwerte_Beschreibung_Stationen.txt"
  path <- system.file("extdata", file.name, package = "zeppr")
  expect_true(file.exists(path))
  data.table <- read_dwd_stations_info_file(path)
  expect_s3_class(data.table$start_date, "Date")
  expect_s3_class(data.table$end_date, "Date")
  expect_equal(c(660, 8), dim(data.table))
  expect_equal(246, median(data.table$altitude))
  expect_equal(50.6528, median(data.table$latitude))
  expect_equal(10.0233, median(data.table$longitude))
  expect_equal(660, length(unique(data.table$station_name)))
  expect_equal(16, length(unique(data.table$bundesland)))
})
