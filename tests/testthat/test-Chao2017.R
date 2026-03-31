test_that("checking Chao2017", {

  #check Chao2017 SA(0.01s) for mag 4 at distance 15.8489 km
  vals <- Chao2017(4, 15.8489, 0.01, 0, 180, 1, 14.17, 0.352866, 1, 0)
  sa0.01 <- as.numeric(exp(unlist(vals)["lnY"])/exp(6.89))

  expect_length(vals, 16)
  expect_equal(sa0.01, 0.041, tolerance = 0.001, scale = 1)

})
