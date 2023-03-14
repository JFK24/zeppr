test_that("day_temp_index", {
  expect_equal(day_temp_index(t.min=12, t.max=32, index="tropical"), TRUE)
  expect_equal(day_temp_index(t.min=c(12, 15), t.max=c(15, 30), index="heating"), c(TRUE, FALSE))
})

test_that("past_day_temp_index", {
  data <- data.frame(
    Date=as.Date(c("2022-01-10", "2022-01-11", "2022-01-12", "2022-01-13", "2022-01-14")),
    Tmin=c(4, 6, 11, 15, 20),
    Tmax=c(12, 14, 20, 17, 25))
  expect_equal(
    past_day_temp_index(data$Date, data$Tmin, data$Tmax, n.days=1, index="heating"),
    c(1, 1, 0, 0, 0)
               )
  expect_equal(
    past_day_temp_index(data$Date, data$Tmin, data$Tmax, n.days=1, index="vegetation"),
    c(1, 1, 1, 1, 1)
  )
  expect_equal(
    past_day_temp_index(data$Date, data$Tmin, data$Tmax, n.days=1, index="heating", from.month=1, from.month.day=11),
    c(0, 1, 0, 0, 0)
  )
  expect_equal(
    past_day_temp_index(data$Date, data$Tmin, data$Tmax, n.days=1, index="vegetation", from.date="2022-01-12"),
    c(0, 0, 1, 1, 1)
  )
})

test_that("mutate_past_day_temp_indices", {
  data <- data.frame(
      Date=as.Date( c("2022-01-10", "2022-01-11", "2022-01-12",
                    "2022-01-13", "2022-01-14", "2022-01-15")),
      Tmin=c(-10, -1, 1, 15, 20, 6),
      Tmax=c(-1, 5, 5, 17, 25, 10))

  expect_equal(
    mutate_past_day_temp_indices(data, Date, Tmin, Tmax, n.days=1)$ice_last_1d,
    c(1, 0, 0, 0, 0, 0)
  )
  expect_equal(
    mutate_past_day_temp_indices(data, Date, Tmin, Tmax, n.days=3)$frost_last_3d,
    c(NA, NA, 2, 1, 0, 0)
  )
  expect_equal(
    mutate_past_day_temp_indices(data, Date, Tmin, Tmax, n.days=3, longest_period=TRUE)$heating_last_3d_lp,
    c(NA, NA, 3, 2, 1, 1)
  )
  expect_equal(
    mutate_past_day_temp_indices(data, Date, Tmin, Tmax, from.month=1, from.month.day=11)$summer_1_11,
    c(0, 0, 0, 0, 1, 0)
  )
  expect_equal(
    mutate_past_day_temp_indices(data, Date, Tmin, Tmax, from.date="2022-01-12")$`vegetation_2022-01-12`,
    c(0, 0, 0, 1, 1, 1)
  )
  expect_equal(
    mutate_past_day_temp_indices(data, Date, Tmin, Tmax, german.output=TRUE)$Eistag_letzte_1T,
    c(1, 0, 0, 0, 0, 0)
  )
  expect_equal(
    mutate_past_day_temp_indices(data, Date, Tmin, Tmax, no.suffix=TRUE, indices=c("frost", "heating"))$heating,
    c(1, 1, 1, 0, 0, 1)
  )
})
