makeFootprintFixture <- function(filepath, resolution=2, longitude=-105) {
  h5file <- hdf5r::H5File$new(filepath, mode="w")
  on.exit(h5file$close_all())
  site <- h5file$create_group("TEST")
  hdf5r::h5attr(site, "LatTow") <- 40
  hdf5r::h5attr(site, "LonTow") <- longitude
  hdf5r::h5attr(site, "ZoneUtm") <- "13N"
  foot <- site$create_group("dp04")$create_group("data")$create_group("foot")
  foot[["stat"]] <- data.frame(distReso=rep(resolution, 2))
  grid <- foot$create_group("grid")
  grid[["halfHour1"]] <- matrix(as.numeric(1:6), nrow=2)
  grid[["halfHour2"]] <- matrix(as.numeric(7:12), nrow=2)
  hdf5r::h5attr(grid[["halfHour1"]], "units") <- "test-units"
}

test_that("footRaster reads grids and georeferencing with hdf5r", {
  skip_if_not_installed("hdf5r")
  skip_if_not_installed("terra")
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive=TRUE))
  files <- file.path(directory, c("NEON.D01.TEST.2024-01-01.h5",
                                 "NEON.D01.TEST.2024-01-02.h5"))
  for(filepath in files) {
    makeFootprintFixture(filepath)
  }

  result <- footRaster(files, progress=FALSE)
  expect_s4_class(result, "SpatRaster")
  expect_equal(terra::nlyr(result), 5)
  expect_equal(unname(as.matrix(result[[2]], wide=TRUE)),
               t(matrix(as.numeric(1:6), nrow=2)))
  expect_equal(unname(as.matrix(result[[3]], wide=TRUE)),
               t(matrix(as.numeric(7:12), nrow=2)))
  expect_equal(unname(as.matrix(result[[1]], wide=TRUE)),
               t(matrix(as.numeric(4:9), nrow=2)))
  expect_equal(names(result)[1], "TEST.summary")
  tower <- terra::project(terra::vect(cbind(-105, 40),
                                     crs="+proj=longlat +datum=WGS84"),
                          "EPSG:32613")
  expect_equal(as.vector(terra::ext(result)),
               as.vector(terra::ext(tower)) + c(-301, 301, -301, 301))
  expect_match(terra::crs(result), "32613")
  for(filepath in files) {
    h5file <- hdf5r::H5File$new(filepath, mode="r+")
    expect_equal(h5file$get_obj_count(), 1)
    h5file$close_all()
  }

  makeFootprintFixture(files[2], resolution=3)
  expect_message(footRaster(files, progress=FALSE), "Resolution attribute is inconsistent")
  makeFootprintFixture(files[2], longitude=-104)
  expect_message(footRaster(files, progress=FALSE), "Resolution attribute is inconsistent")
})

test_that("footRaster closes HDF5 files when footprint data are absent", {
  skip_if_not_installed("hdf5r")
  skip_if_not_installed("terra")
  filepath <- tempfile(fileext=".h5")
  on.exit(unlink(filepath))
  makeFootprintFixture(filepath)
  h5file <- hdf5r::H5File$new(filepath, mode="r+")
  h5file$link_delete("TEST/dp04/data/foot/grid")
  h5file$close_all()

  expect_error(footRaster(filepath, progress=FALSE), "No footprint data available")
  h5file <- hdf5r::H5File$new(filepath, mode="r+")
  expect_equal(h5file$get_obj_count(), 1)
  h5file$close_all()
  expect_error(footRaster(paste0(filepath, ".missing.h5"), progress=FALSE),
               "could not be read")
})