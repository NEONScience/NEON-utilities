### Description
Provisional NEON data files from instrumented and observation systems are delivered by NEON within zip files organized by site and 
year-month. `neonDataStackR` is an R package for unzipping and joining the data files within a single downloaded zip package (in a single step using `stackByTable()` - all other functions support this one). Data will be grouped into new files by table name (e.g., 2DWSD_2min, 2DWSD_30min for 2D Wind Speed and Direction). For data products from instrumented systems that have multiple sensors placed at various heights (or depths) and/or horizontal positions away from the supporting tower, neonDataStackR will create 2 columns in addition to the existing columns, one for horizontalPosition and the other for verticalPosition.

This package will only work for data products that organize data in CSV files. Other data file types, such as HDF5 files from the 
eddy covariance and airborne observing systems are not supported by this package.

### How to Use
This provides one basic function, `stackByTable()`. 

``` 
library(devtools)
install_github("NEONScience/NEON-utilities/neonDataStackR", dependencies=TRUE)
library (neonDataStackR)
stackByTable(dpID="DP1.10017.001","testdata/NEON_size-dust-particulate.zip")
```

A feeder function, `zipsByProduct()`, can be used to pull data from the NEON API in the correct format to be stacked by `stackByTable()`.

```
{
zipsByProduct(dpID="DP1.10023.001", site="all", package="basic", check.size=T)
stackByTable(paste0(getwd(), "/filesToStack10023"), folder=T)
}
```

Warning: depending on the data product and data volume, pulling data from the API with `zipsByProduct()` can take a very long time.

For a tutorial explaining how to use the `neonDataStackR` package in more detail, view the [*Use the neonDataStackR Package to Access NEON Data* tutorial](http://www.neonscience.org/neonDataStackR).

### Known issues that prevent the use of this package with certain data products:
* Remote sensing data (AOP) are not provided in tabular form, therefore can't be stacked.
* `zipsByProduct()` uses the `download.file()` function, wrapped by the `downloader` package, and we've found in testing that `download.file()` can be finicky. Using R version > 3.4 seems to help, and if you're on Windows, using Windows 10. Feel free to contact us if you run into problems!

This package is under development - please post any issues [here](https://github.com/NEONScience/NEON-utilities/issues) and tag @chrlaney and/or @cklunch.

### Credits & Acknowledgements
The National Ecological Observatory Network is a project solely funded by the National Science Foundation and managed under cooperative agreement by Battelle. Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation.

### License
GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

### Disclaimer
Information and documents contained within this repository are available as-is. Codes or documents, or their use, may not be supported or maintained under any program or service and may not be compatible with data currently available from the NEON Data Portal.

### Change Log
#### 2018-04-05 v0.1.6 'brazil'
-----------------
* Added function to convert individual NEON csv files to GeoCSV format
* Added better messaging for progress of data unzipping and stacking
* Formally added byFileAOP() for downloading large amounts of AOP data using the API; no longer fails if download duration exceeds 24 hours.
* Added warning messages if stackByTable() is used for remote sensing (AOP), eddy covariance, or digital hemipsheric photos.
* NOTE: dpID now must be included in call to stackByTable()

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

