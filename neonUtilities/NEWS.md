# version 2.3.0

Released 2023-07-XX

## Enhancements

* Data download and stacking functions now include recommended citation(s) in BibTeX format. Applies to stackByTable(), loadByProduct(), stackEddy(), byFileAOP(), and byTileAOP().
* New parameter metadata= in stackEddy() to retrieve site, code, and CO2 validation data from H5 file attributes. When used with basic package data, the attributes are valid for the first day of each month accessed. To get attributes for each day of data, use with the expanded package.
* Performance improvements to stackEddy(), will mostly be noticeable when stacking large amounts of data.
* Conversion of dates to POSIXct in stacking functions can optionally use the fasttime package, toggle on and off with useFasttime=. Applies to stackByTable(), readTableNEON(), loadByProduct(), and stackEddy(). Default is not to use fasttime, since it can introduce small inconsistencies in time stamps at the millisecond level. But note that for most NEON data products, precision at the millisecond level is unlikely to be scientifically meaningful.

## Other changes

* byTileAOP() and footRaster() now use the terra package for spatial data handling, replacing the raster package, which will be obsolete as of Oct 2023.


# 2023-06-30: created NEWS file