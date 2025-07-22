context("Table stacking")
library(neonUtilities)


f <- stackByTable(system.file("extdata", "NEON_litterfall.zip", 
                              package="neonUtilities"), savepath="envt")
fe <- read.csv(system.file("extdata", "expected/litter/ltr_litterLignin.csv", 
                     package="neonUtilities"))
test_that("Test that correct files are created for litterfall", {
  expect_true(all(f$ltr_litterLignin$uid %in% fe$uid &
                    all(fe$uid %in% f$ltr_litterLignin$uid)))
})

