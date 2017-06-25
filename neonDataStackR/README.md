### Description
Provisional NEON data files from instrumented and observation systems are delivered by NEON within zip files organized by site and 
year-month. _neonDataStackR_ is an R package for unzipping and joining the data files within a single downloaded zip package. Data will be grouped into new files by table name (e.g., 2DWSD_2min, 2DWSD_30min for 2D Wind Speed and Direction). For data products from instrumented systems that have multiple sensors placed at various heights (or depths) and/or horizontal positions away from the supporting tower, neonDataStackR will create 2 columns in addition to the existing columns, one for horizontalPosition and the other for verticalPosition.

This package will only work for data products that organize data in CSV files. Other data file types, such as HDF5 files from the 
eddy covariance and airborne observing systems are not supported by this package.

### Known issues that prevent the use of this package with certain data products:
* The downloaded variables file for Soil physical properties (Megapit), DP1.00096, is a tab-delimited file saved as a .csv. Variables 
can't be read in correctly. The files will be unzipped but stacked files will not be generated. This will be resolved when this data product
is re-published in the near future.
* Data files in Stream discharge field collection, DP1.20048, need final line endings. Warnings will be generated but the program will
still make stacked files. This will be resolved when this data product is re-published in the near future.

This package is under development - please post any issues [here](https://github.com/NEONScience/NEON-utilities/issues) and tag @chrlaney.
