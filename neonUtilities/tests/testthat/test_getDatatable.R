context("getDatatable")
library(neonUtilities)

test_that("Test that soil collection call returns data", {
  expect_gte(nrow(getDatatable(dpid="DP1.10086.001", data_table_name="sls_soilCoreCollection", 
                          sample_location_list=c("CPER","TALL"), sample_date_min="2014-01-01", 
                          sample_date_max="2014-04-01")),
              1)
})

test_that("Test that bad soil collection call returns no data", {
  expect_equal(nrow(getDatatable(dpid="DP1.10086.001", data_table_name="sls_soilCoreCollection", 
                               sample_location_list=c("CPER","TALL"), sample_date_min="2010-01-01", 
                               sample_date_max="2011-06-01")),
             0)
})
