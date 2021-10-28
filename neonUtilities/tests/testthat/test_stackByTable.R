context("Table stacking")
library(neonUtilities)

files_match <- function(object, output, expected) {
  object
  comp <- compare(tools::md5sum(output)[[1]],
                  tools::md5sum(expected)[[1]])
  expect(
    comp$equal,
    sprintf("Output files do not match previous version files.\n%s", comp$message)
  )
}

f <- file.path(tempdir(), "output_gp")
test_that("Test that correct files are created for microbe group abundances", {
  files_match(stackByTable(system.file("extdata", "NEON_gp.zip", 
                                   package="neonUtilities"), savepath=f),
              paste(f, "stackedFiles/mga_soilGroupAbundances.csv", sep="/"),
              system.file("extdata", "expected/gp/mga_soilGroupAbundances.csv", package="neonUtilities"))
})

