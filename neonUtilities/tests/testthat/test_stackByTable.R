context("Table stacking")
library(neonUtilities)

files_match <- function(object, output, expected) {
  object
  comp <- compare(output$uid,
                  expected$uid)
  expect(
    comp$equal,
    sprintf("Output files do not match previous version files.\n%s", comp$message)
  )
}

f <- stackByTable(system.file("extdata", "NEON_gp.zip", 
                              package="neonUtilities"), savepath="envt")
test_that("Test that correct files are created for microbe group abundances", {
  files_match({},
              f$mga_soilGroupAbundances,
              read.csv(system.file("extdata", "expected/gp/mga_soilGroupAbundances.csv", 
                                   package="neonUtilities")))
})

