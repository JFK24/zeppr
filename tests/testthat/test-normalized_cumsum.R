test_that("Normalized cumulative sum works", {
  expect_equal(c(1,3,6,10,15)/sum(c(1,2,3,4,5)), normalized_cumsum(c(1,2,3,4,5), normalize=T))
  expect_equal(c(1,3,6,10,15), normalized_cumsum(c(1,2,3,4,5), normalize=F))
  expect_equal(c(1,3,6,10,10,15), normalized_cumsum(c(1,2,3,4, NA, 5), na.replace=0, normalize=F))
  expect_equal(c(1,3,6,10,NA,NA), normalized_cumsum(c(1,2,3,4, NA, 5), normalize=F))
  expect_equal(c(1,3,6,10,NA,NA)/sum(c(1,2,3,4, NA, 5), na.rm=T), normalized_cumsum(c(1,2,3,4, NA, 5), normalize=T))
})
