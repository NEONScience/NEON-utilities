makeEddyFixture <- function(filepath) {
  h5file <- hdf5r::H5File$new(filepath, mode="w")
  on.exit(h5file$close_all())
  hdf5r::h5attr(h5file, "version") <- "test-version"
  paths <- c("TEST/dp04/data/fluxCo2/nsae",
             "TEST/dp04/data/fluxCo2/rtioMoleDryCo2Vali",
             "TEST/dp04/data/foot/grid",
             "TEST/dp03/data/fluxCo2/nsae",
             "TEST/dp02/data/soni/000_010_30m/tempSoni",
             "TEST/dp01/data/soni/000_010_30m/tempSoni",
             "TEST/dp01/data/soni/000_010_30m/pres",
             "TEST/dp01/data/co2Stor/000_010_30m/pres")
  values <- data.frame(timeBgn=c("2024-01-01T00:00:00.000Z",
                                 "2024-01-01T00:30:00.000Z"),
                       timeEnd=c("2024-01-01T00:30:00.000Z",
                                 "2024-01-01T01:00:00.000Z"),
                       mean=c(1, 2))
  for(path in paths) {
    parts <- strsplit(dirname(path), "/", fixed=TRUE)[[1]]
    groupPath <- ""
    for(part in parts) {
      groupPath <- paste0(groupPath, "/", part)
      if(!h5file$exists(groupPath)) {
        h5file$create_group(groupPath)
      }
    }
    h5file[[path]] <- values
    hdf5r::h5attr(h5file[[path]], "unit") <- c("NA", "NA", "umol")
  }
  hdf5r::h5attr(h5file[["TEST"]], "siteName") <- "Test site"
  hdf5r::h5attr(h5file[[paths[2]]], "validation") <- "test-validation"
  h5file[["objDesc"]] <- data.frame(name="mean", description="Mean value")
}

test_that("stackEddy reads data and metadata with hdf5r", {
  skip_if_not_installed("hdf5r")
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive=TRUE))
  for(packageType in c("basic", "expanded")) {
    date <- if(packageType=="basic") "2024-01" else "2024-01-01"
    filepath <- file.path(directory, paste0("NEON.D01.TEST.DP4.00200.001.",
                                           packageType, ".", date, ".h5"))
    makeEddyFixture(filepath)
    result <- suppressMessages(stackEddy(filepath, metadata=TRUE,
                                         runLocal=TRUE, progress=FALSE))
    expect_equal(result$TEST$data.fluxCo2.nsae.mean, c(1, 2))
    expect_equal(result$TEST$timeBgn,
                 as.POSIXct(c("2024-01-01 00:00:00", "2024-01-01 00:30:00"),
                            tz="GMT"))
    expect_false(any(grepl("grid", names(result$TEST))))
    expect_equal(result$variables$units, rep(c("NA", "NA", "umol"), 2))
    expect_equal(result$objDesc,
                 data.frame(name="mean", description="Mean value"))
    expect_equal(result$siteAttributes$siteName, "Test site")
    expect_equal(result$siteAttributes$date, date)
    expect_equal(result$codeAttributes$version, "test-version")
    expect_equal(result$validationAttributes$validation, "test-validation")
    expect_false("issueLog" %in% names(result))

    for(level in c("dp01", "dp02", "dp03")) {
      selected <- stackEddy(filepath, level=level, var="tempSoni", avg=30,
                             runLocal=TRUE, progress=FALSE)
      column <- if(level=="dp03") "data.fluxCo2.nsae.mean" else
        "data.soni.tempSoni.mean"
      expect_equal(selected$TEST[[column]], c(1, 2))
      expect_false("siteAttributes" %in% names(selected))
      if(level!="dp03") {
        expect_equal(selected$TEST$verticalPosition, c("010", "010"))
        expect_equal(selected$TEST$horizontalPosition, c("000", "000"))
      }
    }
    selected <- stackEddy(filepath, level="dp01", var=c("pres", "co2Stor"),
                           avg=30, runLocal=TRUE, progress=FALSE)
    expect_equal(selected$TEST$data.co2Stor.pres.mean, c(1, 2))
    expect_false(any(grepl("soni", names(selected$TEST))))
    expect_error(stackEddy(filepath, level="dp01", var="tempSoni", avg=1,
                            runLocal=TRUE, progress=FALSE),
                 "No data met requested criteria")
    expect_error(stackEddy(filepath, level="dp01", var="missing", avg=30,
                            runLocal=TRUE, progress=FALSE),
                 "No data found for variables")
    h5file <- hdf5r::H5File$new(filepath, mode="r+")
    h5file$link_delete("objDesc")
    h5file$close_all()
    withoutDescription <- stackEddy(filepath, runLocal=TRUE, progress=FALSE)
    expect_true(is.na(withoutDescription$objDesc))
  }
})