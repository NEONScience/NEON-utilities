context("Table stacking")
library(neonUtilities)

makes_files <- function(object, output, expected) {
  object
  comp <- compare(list.files(output),
                  list.files(expected))
    expect(
      comp$equal,
      sprintf("List of stacked files does not match.\n%s", comp$message)
    )
}

test_that("Test that correct files are created for microbe group abundances", {
  makes_files(stackByTable(system.file("extdata", "NEON_gp.zip", 
                                   package="neonUtilities")),
              system.file("extdata", "NEON_gp/stackedFiles", package="neonUtilities"),
              system.file("extdata", "expected/gp", package="neonUtilities"))
})

