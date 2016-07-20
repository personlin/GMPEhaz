context('function LL08Rock')

test_that("checking LL08Rock", {

  #check LL08 PGA for mag 6 at distance 20 and depth 10
  vals <- LL08Rock(6, 20, 10, 0, 0)
  pga.ll08 <- as.numeric(exp(unlist(vals)["lnY"])/exp(6.89))

  expect_that(length(vals), equals(9))
  expect_equal(pga.ll08, 0.09467912, tolerance = 0.0001, scale = 1)
  #expect_equivalent(pga.ll08, 0.09467912)

})
