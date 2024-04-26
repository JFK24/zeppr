test_that("time_sliders", {

  # variables
  times <- as.Date(as.Date("2021-12-25"):as.Date("2022-01-05"))
  values <- c(10, 5, 8, 12, 6, 14, 11, 4, 3, 1, 9, 15)
  expr="sum(., na.rm=TRUE)"
  # expr="dplyr::first(.)"
  # expr="dplyr::last(.)"
  # expr="mean(., na.rm=TRUE)"

  # window_type == yday
  res_yday <- data.frame(
    dates=times,
    yday=lubridate::yday(times),
    values=values,
    res_t=time_slider(values, times, expr, "yday", start_yday=362, complete=T),
    res_f=time_slider(values, times, expr, "yday", start_yday=362, complete=F)
  )
  expect_equal(c(10, 15, 23, 12, 18, 32, 43, 47, 50, 51, 60, 75), res_yday$res_f)
  expect_equal(c(NA, NA, NA, 12, 18, 32, 43, 47, 50, 51, 60, 75), res_yday$res_t)

  # window_type == fixed
  my.date.1 <- "2021-12-27"
  my.date.2 <- "2022-01-03"
  res_fixed <- data.frame(
    dates=times,
    yday=lubridate::yday(times),
    values=values,
    res_t_1=time_slider(values, times, expr, "fixed", fixed_date=my.date.1, complete=T),
    res_f_1=time_slider(values, times, expr, "fixed", fixed_date=my.date.1, complete=F),
    res_t_2=time_slider(values, times, expr, "fixed", fixed_date=my.date.2, complete=T),
    res_f_2=time_slider(values, times, expr, "fixed", fixed_date=my.date.2, complete=F)
  )
  expect_equal(c(NA, NA,   8, 20, 26, 40, 51, 55, 58, 59, 68, 83), res_fixed$res_t_1)
  expect_equal(c(NA, NA,   8, 20, 26, 40, 51, 55, 58, 59, 68, 83), res_fixed$res_f_1)
  expect_equal(c(NA, NA, NA, NA, NA, NA, NA, NA, NA,   1, 10, 25), res_fixed$res_t_2)
  expect_equal(c(NA, NA, NA, NA, NA, NA, NA, NA, NA,   1, 10, 25), res_fixed$res_f_2)

  # window_type == start_values
  my.month <- 12
  my.day <- 27
  res_start_values <- data.frame(
    dates=times,
    yday=lubridate::yday(times),
    values=values,
    res_t=time_slider(values, times, expr, "start_values", start_month=my.month, start_day=my.day, complete=T),
    res_f=time_slider(values, times, expr, "start_values", start_month=my.month, start_day=my.day, complete=F)
  )
  expect_equal(c(NA, NA, 8, 20, 26, 40, 51, 55, 58, 59, 68, 83), res_start_values$res_t)
  expect_equal(c(10, 15, 8, 20, 26, 40, 51, 55, 58, 59, 68, 83), res_start_values$res_f)

  # window_type == before
  my.before_val_1 <- 3
  my.before_unit_1="days"
  my.before_val_2 <- 1
  my.before_unit_2="weeks"
  res_before <- data.frame(
    dates=times,
    yday=lubridate::yday(times),
    values=values,
    res_t_1=time_slider(values, times, expr, "before", before_val=my.before_val_1, before_unit=my.before_unit_1, complete=T),
    res_f_1=time_slider(values, times, expr, "before", before_val=my.before_val_1, before_unit=my.before_unit_1, complete=F),
    res_t_2=time_slider(values, times, expr, "before", before_val=my.before_val_2, before_unit=my.before_unit_2, complete=T),
    res_f_2=time_slider(values, times, expr, "before", before_val=my.before_val_2, before_unit=my.before_unit_2, complete=F)
  )
  expect_equal(c(NA, NA, NA, 35, 31, 40, 43, 35, 32, 19, 17, 28), res_before$res_t_1)
  expect_equal(c(10, 15, 23, 35, 31, 40, 43, 35, 32, 19, 17, 28), res_before$res_f_1)
  expect_equal(c(NA, NA, NA, NA, NA, NA, NA, 70, 63, 59, 60, 63), res_before$res_t_2)
  expect_equal(c(10, 15, 23, 35, 41, 55, 66, 70, 63, 59, 60, 63), res_before$res_f_2)
})

test_that("index or count leading values", {
  x <- c(1, NA, 7, 8, 4, 1, 2, NA, 4)
  y <- c(1,3,5,6,7,9,2,4,8)
  res.1 <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  res.2 <- c(TRUE, NA, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  res.3 <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE)
  res.4 <- c(TRUE, NA, FALSE, FALSE, FALSE, FALSE, TRUE, NA, FALSE)
  expect_equal(res.1, index_leading_values_if(x=x, operator="<", ref.value=8))
  expect_equal(res.2, index_leading_values_if(x=x, operator="<", ref.value=8, keep.na.values=TRUE))
  expect_equal(2, n_leading_values_if(x=x, operator="<", ref.value=8, keep.na.values=TRUE))
  expect_equal(3, n_leading_values_if(x=x, operator="<", ref.value=8, keep.na.values=FALSE))
  expect_equal(res.3, index_leading_values_if(x=x, operator="<=", ref.value=2, order.by=y, keep.na.values=FALSE))
  expect_equal(res.4, index_leading_values_if(x=x, operator="<=", ref.value=2, order.by=y, keep.na.values=TRUE))
})
test_that("index or count trailing values", {
  x <- c(1, NA, 7, 8, 4, 1, 2, NA, 4)
  res.1 <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
  res.2 <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, NA, TRUE)
  expect_equal(res.1, index_trailing_values_if(x=x, operator="==", ref.value=4))
  expect_equal(res.2, index_trailing_values_if(x=x, operator=">=", ref.value=2, keep.na.values=TRUE))
  expect_equal(1, n_trailing_values_if(x=x, operator="==", ref.value=4, keep.na.values=TRUE))
})
test_that("index first value", {
  x <- c(1, NA, 7, 8, 4, 1, 2, NA, 4)
  res.1 <- c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  res.2 <- c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  res.3 <- c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  expect_equal(res.1, index_first_value_if(x=x, operator=">", ref.value=6, complete.to.end=FALSE))
  expect_equal(res.2, index_first_value_if(x=x, operator=">", ref.value=6, complete.to.end=TRUE))
  expect_equal(res.3, index_first_value_if(x=x, operator="is.na"))
})
