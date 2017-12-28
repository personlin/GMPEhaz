# library(testthat)
# library(GMPEhaz)

context('function Chao2017')

test_that("checking Chao2017", {

  #check LL08 PGA for mag 6 at distance 20 and depth 10
  vals <- Chao2017(4, 15.8489, 0.01, 0, 180, 1, 14.17, 0.352866, 1, 0)
  sa0.01 <- as.numeric(exp(unlist(vals)["lnY"])/exp(6.89))

  expect_that(length(vals), equals(16))
  expect_equal(sa0.01, 0.041, tolerance = 0.001, scale = 1)
  #expect_equivalent(pga.ll08, 0.09467912)

})
