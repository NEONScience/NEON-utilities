NEON-utilities/neonUtilities
================

<!-- ****** Description ****** -->
Description
-----

The `neonUtilities` R package provides utilities for discovering, downloading, and working with NEON data files. NEON data files can be downloaded from the NEON Data Portal (http://data.neonscience.org) or API (http://data.neonscience.org/data-api). Provisional NEON data files from instrumented and observation systems are delivered by NEON within zip files organized by site and year-month. Provisional NEON data files from the airborne observation platform (AOP) are organized by site and year.

`neonUtilities` is available [on CRAN](https://CRAN.R-project.org/package=neonUtilities) and most users will want to install it from there. If you want to use the current development version, you can install from GitHub, but be warned that the version here may not be stable.

This package was developed on top of the deprecated `neonDataStackR` package; change logs from that package are included below.

This package is under development - please post any issues [here](https://github.com/NEONScience/NEON-utilities/issues) and tag @chrlaney and/or @cklunch.

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
stackByTable(paste0(getwd(), "/filesToStack10023"), folder=T)
}
```

`loadByProduct()` performs the actions of both `zipsByProduct()` and `stackByTable()` and loads the resulting data into the current R environment. The object output by `loadByProduct()` is a named list of tables.

```
bird <- loadByProduct(dpID="DP1.10003.001", site="all", package="expanded")
names(bird)
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

`transformFileToGeoCSV()` takes any single NEON csv data file plus its respective variables file, and generates a new CSV with [GeoCSV](http://geows.ds.iris.edu/documents/GeoCSV.pdf) headers. This makes the data similar in format to data provided by organizations such as UNAVCO, and is good for embedding in a repeating script.

### Getting help with this package

For a tutorial explaining how to use the `neonUtilities` package in more detail, including additional input options, view the [*Use the neonUtilities Package to Access NEON Data* tutorial](http://www.neonscience.org/neonDataStackR).

### Known issues
* `zipsByProduct()` and `byFileAOP()` use the `download.file()` function, wrapped by the `downloader` package, and we've found in testing that `download.file()` can be finicky. Using R version > 3.4 seems to help, and if you're on Windows, using Windows 10. Feel free to contact us if you run into problems!
* The file cleanup option in `stackByTable()` deletes the unstacked files after stacking, but it doesn't work correctly when used in combination with filtering by averaging interval (`avg=X` option in `zipsByProduct()`). This will be fixed in a future release.

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
