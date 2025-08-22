test_that("get_jki_gridid", {
  gridid <- get_jki_gridid(x=13.211756, y=52.292031) # expecting: 37875803
  expect_equal(gridid, "37875803")
  gridid <- get_jki_gridid(x=12.4558, y=51.124) # expecting: 37415671
  expect_equal(gridid, "37415671")
})
