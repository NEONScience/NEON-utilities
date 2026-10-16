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

test_that("getVarsEddy lists dataset metadata with hdf5r", {
  skip_if_not_installed("hdf5r")
  filepath <- tempfile(fileext=".h5")
  on.exit(unlink(filepath))
  makeEddyFixture(filepath)
  h5file <- hdf5r::H5File$new(filepath, mode="r+")
  h5file[["TEST/dp01/data/soni/000_010_30m/matrix"]] <- matrix(1:6, nrow=2)
  h5file$close_all()

  result <- suppressWarnings(getVarsEddy(filepath))
  expect_named(result, c("site", "level", "category", "system", "hor",
                         "ver", "tmi", "subsys", "name", "otype", "dclass",
                         "dim", "oth"))
  expect_equal(nrow(result), 10L)
  expect_true(all(result$otype == "H5I_DATASET"))
  pressure <- result[result$name == "pres",]
  expect_equal(pressure$site, rep("TEST", 2))
  expect_equal(pressure$level, rep("dp01", 2))
  expect_equal(pressure$category, rep("data", 2))
  expect_equal(pressure$system, c("co2Stor", "soni"))
  expect_equal(pressure$hor, rep("000", 2))
  expect_equal(pressure$ver, rep("010", 2))
  expect_equal(pressure$tmi, rep("30m", 2))
  expect_equal(pressure$dclass, rep("COMPOUND", 2))
  expect_equal(pressure$dim, rep("2", 2))
  expect_equal(result$dim[result$name == "matrix"], "2 x 3")
  expect_equal(result$dclass[result$name == "matrix"], "INTEGER")
  expect_equal(result$site[result$name == "objDesc"], "")
  expect_true(all(is.na(result$oth)))

  h5file <- hdf5r::H5File$new(filepath, mode="r+")
  expect_equal(h5file$get_obj_count(), 1)
  h5file$close_all()
  expect_error(getVarsEddy(paste0(filepath, ".missing")), "could not be read")
  writeLines("not an HDF5 file", filepath)
  expect_error(getVarsEddy(filepath), "could not be read")
})

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