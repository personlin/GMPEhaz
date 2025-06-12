context('utility functions')

test_that("get.GMPE.prd returns period range", {
  prd <- get.GMPE.prd("ASK14")
  expect_true(is.data.frame(prd))
  expect_true(all(c("period.min", "period.max") %in% names(prd)))
})

test_that("atten2 simple call includes Rrup and lnY", {
  vals <- atten2("LL08Rock", 0, Mag=c(6,7), Rrup=c(20,20), depth=10, ftype=0, plot=FALSE)
  expect_true(is.data.frame(vals))
  expect_true(all(c("Rrup", "lnY") %in% names(vals)))
})
