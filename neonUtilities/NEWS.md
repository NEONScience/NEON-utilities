# version 2.4.3

Released 2024-12-04

## Changes

* stackByTable() and loadByProduct() updated to handle new files associated with Bathymetric and morphological maps (DP4.00132.001) and the three revised microbe community data products (DP1.10081.002, DP1.20086.002, and DP1.20141.002)


# version 2.4.2

Released 2024-04-30

## Bug fixes

* stackEddy() had been erroring out any time it encountered a file that was incompatible with the input criteria (e.g., a variable requested in the var= parameter was not present in an h5 file). Now it skips the file and moves on to the next.
* Download functions (zipsByProduct(), loadByProduct(), byFileAOP(), byTileAOP()) include a re-attempt routine if the first download attempt fails. include.provisional= was not handled correctly in the re-attempt code and resulted in the download erroring out. This is now fixed.
* Downloads by averaging interval (timeIndex=) in zipsByProduct() and loadByProduct() were not retrieving science review flag files. This is now fixed.


# version 2.4.1

Released 2024-01-09

## Enhancements

* New option to run stackEddy() only on already-downloaded data, without pinging the NEON API. Using this option blocks retrieval of the issue log and citation files; its use is not recommended unless you need to run code offline or otherwise need to avoid API calls. Toggle this option using the input `runLocal=`, which defaults to FALSE.

## Bug fixes

* Data tables in Breeding landbird point counts (DP1.10003.001), Bathymetric and morphological maps (DP4.00132.001), and Stream morphology map (DP4.00131.001) have changed publication paradigm over time (e.g., from publication only in the site and month when collection occurred, to publication in all months with data for the relevant site). stackByTable() and loadByProduct() have been updated to accommodate both current data and past releases.
* Surface-atmosphere exchange (Eddy covariance bundle, DP4.00200.001) metadata have updated the formatting of UTM zone reporting; footRaster() has been updated to handle both current and past formatting. 


# version 2.4.0

Released 2023-10-17

## Changes

* Data download functions now default to not returning provisional data. This applies to zipsByProduct(), loadByProduct(), byFileAOP(), and byTileAOP(). To download provisional data, add the new parameter `include.provisional=TRUE` to the function call.
* Data stacking has been updated to handle the new formatting of the Reaeration (DP1.20190.001) and Salt-based discharge (DP1.20193.001) data products. The conductivity data tables in these two products have been converted to separate files per sampling event. This update enables stackByTable() and loadByProduct() to work with both the previous and current formats.


# version 2.3.0

Released 2023-07-05

## Enhancements

* Data download and stacking functions now include recommended citation(s) in BibTeX format. Applies to stackByTable(), loadByProduct(), stackEddy(), byFileAOP(), and byTileAOP(). Note that citation retrieval will generally work on data downloaded from the NEON data portal or using neonUtilities functions, but if you use alternative download methods and/or modify the contents of downloaded data packages, the stacking functions may not be able to identify data release information and may not provide citations.
* New parameter metadata= in stackEddy() to retrieve site, code, and CO2 validation data from H5 file attributes. When used with basic package data, the attributes are valid for the first day of each month accessed. To get attributes for each day of data, use with the expanded package.
* Performance improvements to stackEddy(), will mostly be noticeable when stacking large amounts of data.
* Conversion of dates to POSIXct in stacking functions can optionally use the fasttime package, toggle on and off with useFasttime=. Applies to stackByTable(), readTableNEON(), loadByProduct(), and stackEddy(). Default is not to use fasttime, since it can introduce small inconsistencies in time stamps at the millisecond level. But note that for most NEON data products, precision at the millisecond level is unlikely to be scientifically meaningful.

## Bug fixes

* When no data are available for a particular day and sensor in the eddy covariance system, a single empty for the day is reported in the H5 file. In some cases, stackEddy() was mishandling these records and creating duplicate records. This is now resolved.

## Other changes

* byTileAOP() and footRaster() now use the terra package for spatial data handling, replacing the raster package, which will be obsolete as of Oct 2023.


-----------------------

# 2023-06-30: created NEWS file
