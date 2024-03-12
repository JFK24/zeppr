test_that("normalized values", {
  data <- data.frame(
    date=as.Date(c("2022-01-01", "2022-01-05", "2022-01-06", "2022-01-11")),
    counts=c(12, 14, 20, 12))
  expect_equal(
    sum(normalize_trap_data(data, date, counts, default.duration=7, use.mean.days=TRUE, values.to="norm_val", ndays.to=NA, max.duration=NA)$norm_val), # 29.9
    29.9
  )
  expect_equal(
    sum(normalize_trap_data(data, date, counts, default.duration=7, use.mean.days=FALSE, values.to="norm_val", ndays.to=NA, max.duration=NA)$norm_val),
    27.614286
  )
  expect_equal(
    sum(normalize_trap_data(data, date, counts, default.duration=7, use.mean.days=FALSE, values.to="norm_val", ndays.to=NA, max.duration=2)$norm_val),
    34.714286
  )
})
