context("byTileAOP")
library(neonUtilities)

test_that("Test that unequal length easting and northing vectors return error", {
  expect_error(byTileAOP(dpID="DP3.30015.001", site="WREF", year=2017,
                         easting=571000, northing=c(5079000,5080000)),
               "Easting and northing vector lengths do not match, and/or contain null values. Cannot identify paired coordinates.")
})
