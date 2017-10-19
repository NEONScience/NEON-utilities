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
stackByTable("testdata/NEON_size-dust-particulate.zip")
```

A feeder function, `zipsByProduct()`, can be used to pull data from the NEON API in the correct format to be stacked by `stackByTable()`.

```
{
zipsByProduct(dpID="DP1.10023.001", site="all", package="basic", check.size=T)
stackByTable(paste0(getwd(), "/filesToStack10023"), folder=T)
}
```

Warning: depending on the data product and data volume, pulling data from the API with `zipsByProduct()` can take a very long time.


### Known issues that prevent the use of this package with certain data products:
* Data files in Stream discharge field collection, DP1.20048, need final line endings. Warnings will be generated but the program will
still make stacked files. This will be resolved when this data product is re-published in the near future.
* Instrumentation data (IS) are currently undergoing re-publication into NEON's cloud storage system. Data products that haven't been re-published yet are unlikely to be downloadable by `zipsByProduct()`.
* Remote sensing data (AOP) are not provided in tabular form, therefore can't be stacked.

This package is under development - please post any issues [here](https://github.com/NEONScience/NEON-utilities/issues) and tag @chrlaney.

### Credits & Acknowledgements
The National Ecological Observatory Network is a project solely funded by the National Science Foundation and managed under cooperative agreement by Battelle. Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation.

### License
GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

### Disclaimer
Information and documents contained within this repository are available as-is. Codes or documents, or their use, may not be supported or maintained under any program or service and may not be compatible with data currently available from the NEON Data Portal.

### Change Log
##### 2017-09-21 v0.1.3 pecan
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

