library(neonUtilities)

tmp <- tempfile()

test_that("canopy foliar stacks correctly", {
  expect_known_value(stackByTable(system.file("testdata", "NEON_chem-phys-foliar.zip", 
                                   package="neonUtilities")), tmp)
})
