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

### Known issues that prevent the use of this package with certain data products:
* The downloaded variables file for Soil physical properties (Megapit), DP1.00096, is a tab-delimited file saved as a .csv. Variables 
can't be read in correctly. The files will be unzipped but stacked files will not be generated. This will be resolved when this data product
is re-published in the near future.
* Data files in Stream discharge field collection, DP1.20048, need final line endings. Warnings will be generated but the program will
still make stacked files. This will be resolved when this data product is re-published in the near future.

This package is under development - please post any issues [here](https://github.com/NEONScience/NEON-utilities/issues) and tag @chrlaney.

### Credits & Acknowledgements
The National Ecological Observatory Network is a project solely funded by the National Science Foundation and managed under cooperative agreement by Battelle. Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation.

### License
GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

### Disclaimer
Information and documents contained within this repository are available as-is. Codes or documents, or their use, may not be supported or maintained under any program or service and may not be compatible with data currently available from the NEON Data Portal.

### Change Log

##### 2017-07-21 v0.1.2
-----------------
* fixed bug where code was attempting to reorder columns for republished OS data products when it should only be reordering columns for IS data.

##### 2017-07-02 v0.1.1
-----------------
* removed supporting functions from public view

