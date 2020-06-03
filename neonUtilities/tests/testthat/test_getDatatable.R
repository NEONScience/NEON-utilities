context("getDatatable")
library(neonUtilities)

test_that("Test that attempt to download IS data returns error", {
  expect_error(getDatatable(dpid="DP1.20033.001", data_table_name="NSW_15_minute", 
                          sample_location_list=c("ARIK","POSE"), sample_date_min="2014-01-01", 
                          sample_date_max="2014-04-01"),
              "This function is only configured for OS data tables published by site and date. See table_types for more information.")
})
