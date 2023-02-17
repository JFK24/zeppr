test_that("test dist_on_earth()", {
  expect_equal(3935.752, dist_on_earth(40.712778, 34.052222, 74.005833, 118.243611))
  expect_equal(2443.8604, dist_on_earth(40.712778, 34.052222, 74.005833, 118.243611, output_unit="miles"))
})

test_that("test closer_dwd_station()", {
  file.name.1 <- "TU_Stundenwerte_Beschreibung_Stationen.txt"
  path.1 <- system.file("extdata", file.name.1, package = "zeppr")
  my.stations <- read_dwd_stations_info_file(path.1)
  expect_equal("00399", closer_dwd_station(52.518611, 13.4083330, stations_table=my.stations))
  expect_equal("Berlin-Alexanderplatz", closer_dwd_station(52.518611, 13.4083330, stations_table=my.stations, return_string = "name"))
})
