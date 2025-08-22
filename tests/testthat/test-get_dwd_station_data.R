test_that("test of read_dwd_station_data_file()", {
  file.name <- "stundenwerte_TU_00044_akt.zip"
  path <- system.file("extdata", file.name, package = "zeppr")
  expect_true(file.exists(path))
  data.table <- read_dwd_station_data_file(path)
  expect_s3_class(data.table$timestamp, "POSIXct")
  expect_equal(c(13200, 4), dim(data.table))
  expect_equal(9.8, median(as.numeric(data.table$Lufttemperatur), na.rm=TRUE))
  expect_equal(86, median(as.numeric(data.table$relative_Feuchte), na.rm=TRUE))
})
