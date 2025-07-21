NEON-utilities/neonUtilities
================

<!-- ****** Description ****** -->
Description
-----

The `neonUtilities` R package provides utilities for discovering, downloading, and working with NEON data files. NEON data files can be downloaded from the NEON Data Portal (http://data.neonscience.org) or API (http://data.neonscience.org/data-api). Provisional NEON data files from instrumented and observation systems are delivered by NEON within zip files organized by site and year-month. Provisional NEON data files from the airborne observation platform (AOP) are organized by site and year.

`neonUtilities` is available [on CRAN](https://CRAN.R-project.org/package=neonUtilities) and most users will want to install it from there. If you want to use the current development version, you can install from GitHub, but be warned that the version here may not be stable.

A cheat sheet for the `neonUtilities` package is available [here](https://www.neonscience.org/sites/default/files/cheat-sheet-neonUtilities.pdf).

See [NEON Data Tutorials](https://www.neonscience.org/resources/learning-hub/tutorials) for more information, particularly the [Get Started](https://www.neonscience.org/resources/learning-hub/tutorials/get-started-neon-data-series-data-tutorials) tutorial series.

This package is actively maintained - please post any issues [here](https://github.com/NEONScience/NEON-utilities/issues) and tag @chrlaney and/or @cklunch.

To get citation details for citing the `neonUtilities` package in a publication, run `citation("neonUtilities")` in R.

<!-- ****** Usage ****** -->
Usage
-----

### Starting out
Install the package into your local environment using the following code:

```
install.packages('neonUtilities')
library(neonUtilities)
```

### Primary functions

* `zipsByProduct()` Download NEON observational (OS) and instrumentation (IS) data by data product.
* `byFileAOP()` Download NEON remote sensing data by site, year, and data product.
* `byTileAOP()` Download NEON remote sensing data by data product, year, and UTM coordinates.
* `stackByTable()` NEON OS and IS data are provided in separate files for each site and month of collection (and location within site, for IS). This function merges these data files cleanly for each data type. Works with data downloaded either via the NEON Data Portal or `zipsByProduct()`.
* `readTableNEON()` Read NEON tabular data into R, detecting variable types via the `variables` file. Currently works for OS and IS data, but not eddy covariance data.
* `loadByProduct()` Combines the actions of `zipsByProduct()`, `stackByTable()`, and `readTableNEON()` for OS and IS data. In one step, downloads, merges, and loads data into R.
* `stackEddy()` Extracts NEON eddy covariance data from the provided HDF files and merges the resulting tabular data, similar to the IS data handling in `stackByTable()`.
* `footRaster()` Makes a raster of the flux footprint data provided in the expanded data package for eddy covaraince.

#### Short examples

`stackByTable()` unzips monthly packages, finds the CSV data files, and joins them by table (e.g., 2DWSD_2min, 2DWSD_30min for 2D Wind Speed and Direction). For data products from instrumented systems that have multiple sensors placed at various heights (or depths) and/or horizontal positions away from the supporting tower, this function will create 2 columns in addition to the existing columns, one for horizontalPosition and the other for verticalPosition. This function will only work for data products that organize data in CSV files. Other data file types, such as HDF5 files from the eddy covariance system and remote sensing airborne observing platform (AOP) are not supported.


```
stackByTable(filepath = "testdata/NEON_size-dust-particulate.zip") # modify filepath to your directory
```

To load data directly into the current R environment, instead of saving the stacked files to the filepath, use the option `savepath='envt'`. When using this option, assign the output of the function to a variable name. The output object will be a named list of data tables.

```
dust <- stackByTable(filepath = "testdata/NEON_size-dust-particulate.zip", savepath="envt")
```

`getPackage()` can be used to pull a single zip file (all the data for a single data product by site by month combination) using the NEON API.

```
# Plant phenology observations from the Jornada LTER site, May 2017
getPackage(dpID = "DP1.10055.001", site_code = "JORN", year_month = "2017-05", package = "basic")
```

`zipsByProduct()` pulls data from the NEON API in the correct format to be stacked by `stackByTable()`. Depending on the data product and data volume, pulling data from the API with `zipsByProduct()` can take a very long time.

```
{
# Herbaceous clip harvest data, from all sites and months for which it is currently available
zipsByProduct(dpID="DP1.10023.001", site="all", package="basic", check.size=T)
stackByTable(paste0(getwd(), "/filesToStack10023"))
}
```

`loadByProduct()` performs the actions of both `zipsByProduct()` and `stackByTable()` and loads the resulting data into the current R environment. The object output by `loadByProduct()` is a named list of tables.

```
bird <- loadByProduct(dpID="DP1.10003.001", site="all", package="expanded")
names(bird)

# To get each table in the list as an independent object, outside of the list:
list2env(bird, .GlobalEnv)
```

Both `zipsByProduct()` and `loadByProduct()` can also subset by sites and date range:

```
wq <- loadByProduct(dpID="DP1.20288.001", site=c("ARIK","POSE"), startdate="2018-04", enddate="2018-08")
```

`byFileAOP()` pulls data from the NEON API, specifically for remote sensing (AOP) data. This function preserves the file directory hierarchy that AOP files are typically stored in, making it easier to navigate a large number of downloaded files.

```
# Lidar slant rangeform data from Santa Rita Experimental Range, 2017 flight
byFileAOP(dpID = "DP3.30001.001", site = "SRER", year = "2017", check.size = T)
```

`byTileAOP()` pulls AOP data from the NEON API, for tiles matching the coordinates specified in the function call. A buffer can also be included in the function call, to download tiles within a certain distance of the coordinates (e.g., if the coordinates are the plot centroids of NEON TOS plots, use buffer=20). `byTileAOP()` will only work on the Level 3, mosaicked AOP products.

```
# Vegetation indices from San Joaquin Experimental Range, 2017
# easting and northing must be matched vectors of UTM coordinates
byTileAOP(dpID="DP3.30026.001", site="SJER", year="2017", easting=easting, northing=northing, buffer=20)
```

`transformFileToGeoCSV()` takes any single NEON csv data file plus its respective variables file, and generates a new CSV with GeoCSV headers. This makes the data similar in format to data provided by organizations such as UNAVCO, and is good for embedding in a repeating script.

### Getting help with this package

For a tutorial explaining how to use the `neonUtilities` package in more detail, including additional input options, view the [*Use the neonUtilities Package to Access NEON Data* tutorial](https://www.neonscience.org/resources/learning-hub/tutorials/neondatastackr).


### Known issues
* On Windows, file paths are limited to 260 characters. In some cases, NEON data file names plus local directories will exceed this length; this is most likely when working with lab quality assurance files, which include the name of the lab in the file name. If this happens, you will see an error saying "cannot open file". Usually, you can get around this by using zipsByProduct() -> stackByTable() -> readTableNEON() to download data and load it to R, taking care to download the files to a short file directory. Reportedly, R v4.3.0+ avoids this problem; we are still testing to evaluate this.
* On slow networks, data download can sometimes time out, particularly for the largest files, usually the remote sensing data. If your downloads take a long time and ultimately fail, try increasing the timeout in your R environment: `options(timeout=300)`. The `timeout` value is in seconds, and defaults to 60.
* Some software security systems prevent R from downloading data from the internet. This is fairly unusual, but if you're unable to download any data using neonUtilities, and increasing the timeout doesn't help, check your security settings.


<!-- ****** Acknowledgements ****** -->
Credits & Acknowledgements
--------------------------

<!-- Acknowledgements text -->
The National Ecological Observatory Network is a project solely funded by the National Science Foundation and managed under cooperative agreement by Battelle. Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation.

<!-- ****** License ****** -->
License
-------
 GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

<!-- ****** Disclaimer ****** -->
Disclaimer
----------
*Information and documents contained within this repository are available as-is. Codes or documents, or their use, may not be supported or maintained under any program or service and may not be compatible with data currently available from the NEON Data Portal.*

<!-- ****** Change Log ****** -->
Change Log
----------

#### 2025-07-21 v3.0.0
Enhancements:
* stacking functions stackByTable() and loadByProduct() now use the arrow package to stack files, increasing stacking speed
* loadByProduct() has a new input, cloud.mode=, to be used for faster data access when transferring data to a cloud environment
* progress bars are now suppressible in download and stacking functions (zipsByProduct(), stackByTable(), loadByProduct(), stackEddy(), byFileAOP(), byTileAOP())
* new function tokenDate() provides the expiration date for a NEON API token
* functions that use API tokens check for expiration and inform the user
* new function getHorVer() provides the available horizontal and vertical indices for a given sensor data product and site
* new function datasetQuery() returns an arrow dataset for a given data product, sites, date range, etc (see documentation). Datasets can be queried via dplyr syntax without downloading.
* new function byEventSIM() enables downloading site management data by event type, rather than by site or date


#### 2024-12-05 v2.4.3
* stackByTable() updated to work with data product revisions and updates affecting DP4.00132.001, DP1.20046.001, DP1.10081.002, DP1.20086.002, and DP1.20141.002


#### 2024-04-30 v2.4.2
Bug fixes:
* stackEddy() skips files that don't match variables requested, rather than erroring out
* Download re-attempts in all download functions now handle include.provisional correctly
* Downloads by averaging interval (timeIndex) for sensor data now include science review flag files


#### 2024-01-09 v2.4.1
Enhancements:
* stackEddy() can be run without API calls, using input parameter runLocal=TRUE

Bug fixes:
* stackByTable() can now handle changes to data table publication paradigm over time. As of this date this affects only DP1.10003.001, DP4.00131.001, and DP4.00132.001
* footRaster() has been updated to handle both current and past formatting of UTM zone in DP4.00200.001 metadata


#### 2023-10-17 v2.4.0
Changes:
* new parameter in download functions: include.provisional defaults to FALSE, provisional data are not included in download unless set to TRUE
* conductivity files in reaeration and salt-based discharge data products have been re-formatted; stacking functions updated to handle these


#### 2023-06-30 v2.3.0
Enhancements:
* option in stackEddy() to retrieve site, code, and CO2 validation data from H5 file attributes
* recommended data citation added to output of download and stacking functions
* performance improvements to stackEddy()
* option to use the fasttime package for conversion of dates to POSIXct in stacking functions

Other changes:
* update to byTileAOP() and footRaster() to use terra package for spatial data handling; replaces raster package


#### 2023-03-09 v2.2.1
* Science Review Flag table added to stackEddy() outputs
* updates to sensor position file handling to match new variable names


#### 2022-11-15 v2.2.0
Enhancements:
* verbose output from `byFileAOP()` and `byTileAOP()` removed
* updated lists of co-located terrestrial sites used for aquatic site meteorology, and co-located sites flown together by AOP
* `getTaxonTable()` is deprecated; moved to `neonOS` package as `getTaxonList()`
* new Science Review Flag table handling added

Bug fixes:
* `footRaster()` now allows a vector of .h5 files as input; handling updated to match `stackEddy()`
* `byFileAOP()` and `byTileAOP()` now correctly handle the rare occurrence of multiple months of data per year
* improved error messaging for all functions when API does not respond


#### 2022-04-12 v2.1.4
Enhancements:
* `getNeonDOI()` function added; finds DOI for a given data product and release
* `zipsByURI()` can now handle R object inputs

Bug fixes:
* column class matching fixed for lab tables
* empty string in API token value handled


#### 2021-12-09 v2.1.3
Enhancements:
* `stackByTable()`, `stackEddy()`, `byTileAOP()`, and `byFileAOP()` include issue log in outputs
* `getIssueLog()` is also available as an independent function

Bug fixes:
* improved error messaging when no data are found
* new error messaging if only metadata files are found


#### 2021-09-01 v2.1.2
Bug fixes:
* readme file handling in `stackByTable()` was failing on new data products; fixed.
* clean up of package dependencies


#### 2021-07-25 v2.1.1
Enhancements:
* more options in `stackEddy()` variable inputs; see function documentation

Bug fixes:
* `readr` package dependency removed to resolve incompatibility with readr 2.0.0
* rare Windows bug for special characters fixed
* `footRaster()` orientation correction in 2.1.0 did not work; corrected again
* `stackEddy()` time stamp handling improved for profile sensors


#### 2021-05-19 v2.1.0
Enhancements:
* download by release in `zipsByProduct()` and `loadByProduct()`
* per sample files, found in microbe community composition and field spectra, are stacked in `stackByTable()`
* `stackFromStore()` updated to work with `neonstore` v0.4.3

Bug fixes:
* `stackEddy()` handles scenarios with only one instance of a given variable
* `footRaster()` coordinate conversion for Alaska and Hawaii fixed
* `footRaster()` orientation corrected
* leading digit in 2D wind data tables is translated, with an alert, in `loadByProduct()`
* warning message in `stackByTable()` when file paths are longer than 260 characters in Windows


#### 2021-01-25 v2.0.1
Bug fixes:
* Release tag assignment in `stackByTable()` resolved in portal data downloads
* Identification of most recent sensor positions files and lab files in `stackByTable()` resolved
* Improved handling of failed downloads in download functions
* Empty filler records for single days with no data removed in `stackEddy()`


#### 2021-01-25 v2.0.0
Major version update, corresponding to the first Release of static, DOI-citable NEON data. Older versions of neonUtilities may not work correctly with Released data. For more information, see [Releases web page](https://www.neonscience.org/data-samples/data-management/data-revisions-releases).

Major changes:
* `stackByTable()` and `stackEddy()` updated to work with new zip folder structure
* `zipsByProduct()` updated to use `packages` API endpoint instead of pre-packaged zip files

Enhancements:
* `stackByTable()` adds release tag to stacked data when possible
* `getDatatable()` is deprecated and functionality is moved to `loadByProduct()`. Note download by table is not recommended for new users; familiarity with the data is a prerequisite.

Bug fixes:
* `stackFromStore()` now includes the full range of input options for sensor and SAE data
* if empty files are downloaded, `stackByTable()` skips them instead of failing. Note this is rare, the result of error in NEON publication systems.
* coordinate conversion for BLAN locations in `byTileAOP()` updated to use latest versions of spatial packages


#### 2021-01-06 v1.3.9
Bug fixes:
* fix bug in handling of .gz files in `stackEddy()`
* fix pubdate filtering in `stackFromStore()`
* regularize fail behavior when API is unavailable


#### 2020-11-09 v1.3.8
Bug fixes:
* handles .gz files in `stackEddy()`
* `footRaster()` update to work with latest rgdal

Enhancements:
* new `stackFromStore()` function to stack files from a local archive
* aquatic site requests can download meteorological data from nearby terrestrial sites
* `stackEddy()` accepts a vector of filepaths to .h5 files


#### 2020-09-24 v1.3.7
Bug fixes:
* removed gdata package dependency

Enhancements:
* input `avg` to `zipsByProduct()` changed to `timeIndex` for clarity


#### 2020-07-26 v1.3.6
Bug fixes:
* unzipped file deletion no longer crashes on large numbers of files

Enhancements:
* compatible with new format of sensor positions files
* zipsByURI() works with fastq files
* lab-specific tables are stacked as well as site-specific tables


#### 2020-05-29 v1.3.5
----------
Bug fixes:
* `loadByProduct()` automatic deletion of temporary files restored
* when API rate limit is reached, `zipsByProduct()` pauses to reset


#### 2020-04-29 v1.3.4
----------
Bug fixes:
* `byFileAOP()` and `byTileAOP()` escape from infinite loops if availability is in error
* water quality (DP1.20288.001) error handling fixed
* field spectra (DP1.30012.001) handling enabled - AOP functions error cleanly, OS/IS functions proceed
* `stackEddy()` works on expanded package data again
* `byTileAOP()` prevents conversion of coordinates to unreadable formats
* `byTileAOP()` correctly handles sites that cross UTM zones
* `byFileAOP()` and `byTileAOP()` handle download for sites that are included in other sites' flight boxes
* `loadByProduct()` handles missing sensor_positions and readme files
* encoding specified in `readTableNEON()`

Enhancements:
* download functions include option to use NEON API token
* `footRaster()` scales raster of flux footprint to geographical coordinates
* `zipsByProduct()` regenerates URLs if they expire


#### 2020-01-07 v1.3.3
----------
Bug fixes:
* readme file retention now works with `avg` input in `zipsByProduct()`
* fields added by `stackByTable()` now appear in variables file
* regularized package citation

Enhancements:
* variables, validation, readme, and sensor_positions file names now include data product number


#### 2019-12-02 v1.3.2
----------
Enhancements:
* `stackByTable()` optionally uses parallel processing
* `stackByTable()` preserves readme and sensor_positions files
* `stackByTable()` appends a new column containing the publication time stamp
* `stackByTable()` no longer requires `folder` input
* `loadByProduct()` detects data types (numeric, character, date) based on variables file


#### 2019-08-02 v1.3.1
-----------
Bug fixes:
* moved `rhdf5` from Imports to Suggests for smoother installation
* fixed expected encoding in `stackByTable()` to UTF-8


#### 2019-07-05 v1.3.0
-----------
Enhancements:
* added `stackEddy()` to extract and merge data from flux data HDF5 files (DP4.00200.001)


#### 2019-05-21 v1.2.2
-----------
Bug fixes:
* working progress bar in `byFileAOP()` and `byTileAOP()`

Enhancements:
* initial version of `zipsByURI()` to download via URLs within data


#### 2019-03-04 v1.2.1
-----------
Bug fixes:
* fixed bug in `stackByTable()` that deleted existing files when using `savepath` argument
* fixed bug in `getDatatable()` that created incorrect urls

Enhancements:
* enabled date and site subsetting in `zipsByProduct()`


#### 2019-01-24 v1.2.0
-----------
Bug fixes:
* fixed bug in `stackByTable()` that merged tables with overlapping names
* fixed bug in `zipsByProduct()` `avg=` option that only worked for a subset of data products
* fixed bug in `stackByTable()` that failed unzipping if savepath=filepath

Enhancements:
* added `getDatatable()`
* added option to `stackByTable()` to load files into the R environment
* added progress bar to `zipsByProduct()`
* added `loadByProduct()` to download and load files in one step


#### 2018-11-13 v1.0.1
------------
* patch to change testing to use temporary directory for test files
* fixed bug in stackByTable() that deleted URLs for ECS files


#### 2018-11-03 v1.0.0
------------
* version for initial CRAN release!
* added `byTileAOP()` to download only AOP tiles corresponding to certain coordinates

#### 2018-05-23 v0.1.1 'pine'
--------------
* dpID no longer required as an input to ```stackByTable()```
* added ```zipsByProduct()``` option to download only one averaging interval (e.g. only 30-minute files) for speedier download and stacking
* added ```zipsByProduct()``` input option to specify file path to save to

#### 2018-04-05 v0.1.0
-----------------
* neonUtilities created from the last version of neonDataStackR

Change Log For Deprecated neonDataStackR Package
----------

#### 2018-04-05 v0.1.6 'brazil'
-----------------
* Added function to convert individual NEON csv files to GeoCSV format
* Loading csv files and stacking files now use the ```data.table fread()``` and ```rbind()``` functions, respectively. This speeds up the stacking process.
* Added better messaging for progress of data unzipping and stacking
* Formally added ```byFileAOP()``` for downloading large amounts of AOP data using the API; no longer fails if download duration exceeds 24 hours.
* Added warning messages if ```stackByTable()``` is used for remote sensing (AOP), eddy covariance, or digital hemipsheric photos.
* NOTE: dpID now must be included in call to ```stackByTable()```

##### 2018-01-24 v0.1.5 'filbert'
-----------------
* This release adds an option to delete the unzipped, unstacked files after stacking.
* Also fixes a bug that incorrectly merged tables in the case where one table name was a substring of another table name.

##### 2017-12-14 v0.1.4 'walnut'
-----------------
* 'Site-all' data tables were not being properly stacked. This package includes a new function to identify 'site-all' data tables and stack only one per site (site-all tables are exact replicates in each monthly package for a given site).
* This release also formally includes the function zipsByProduct(), which allows the user to pull data using the NEON API and stack the tables in one step.

##### 2017-09-21 v0.1.3 'pecan'
-----------------
This has two main bug fixes:
* Stops stacking or overwriting multiple copies of a lab file that is the same from one monthly zip file to the next. The code now reads from table_types.rda to figure out whether a table is of type site-date, lab-all, lab-current, site-all. This file needs to be updated every time that a new or revised data product is available.
* Can properly read new and reprocessed OS data products that use the new file naming convention.

##### 2017-07-21 v0.1.2
-----------------
* fixed bug where code was attempting to reorder columns for republished OS data products when it should only be reordering columns for IS data.

##### 2017-07-02 v0.1.1
-----------------
* removed supporting functions from public view
