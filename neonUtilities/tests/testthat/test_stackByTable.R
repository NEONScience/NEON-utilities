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

f <- stackByTable(system.file("extdata", "NEON_litterfall.zip", 
                              package="neonUtilities"), savepath="envt")
test_that("Test that correct files are created for litterfall", {
  files_match({},
              f$ltr_litterLignin,
              read.csv(system.file("extdata", "expected/litter/ltr_litterLignin.csv", 
                                   package="neonUtilities")))
})

