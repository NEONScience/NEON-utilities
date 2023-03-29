Helper files for neonUtilities functionality. Additional documentation in R package manual.

added_fields
This table contains variables file metadata for the additional fields that can be added to tabular data during stacking by stackByTable(). The relevant fields are appended to the variables file as needed during the stacking process.

chem_bundles
other_bundles
release_2021
Several data products are "bundled" in that, e.g., litter chemical properties can only be downloaded as part of the litterfall data product. The chem_bundles and other_bundles tables contain the mapping, so that the download function can redirect to the bundled product if a non-independent product is requested.
Some of these bundles were created after the 2021 release. The non-independent data tables are no longer accessible via the database query that populates the table_types table, so the release_2021 table retains them and is appended to table_types every time table_types is regenerated. This enables neonUtilities to continue stacking data from RELEASE-2021 correctly.

shared_aquatic
shared_flights
Some sites are paired for some purposes. For aquatic sites that don't have a meteorological station, shared_aquatic provides the mapping to the nearby terrestrial site to be used for specific data products. The download functions redirect to the nearby terrestrial site if the aquatic site and one of the specified products are requested.
Some flight boxes cover more than one site. If AOP data are requested for a site that is covered by another site's flight box, shared_flights provides the mapping and redirects the download to the designated site.

table_types
There are a few different options for data publication from the NEON pipeline, and they have different implications for data stacking. Data that are published based on the date and site of record in the data (site-date) are straightforward, all available data are stacked. For site-all data tables, all available data are published in every data package for a given site, regardless of date. This is done when the data are not specific to a date, for example the trap establishment data for litter traps. In this case data stacking is performed on the most recently published data for each site. Data that are specific to a lab, such as method detection limits, are published in every data package that contains relevant data from the lab in question. In this case data stacking is performed on the most recently published data for each lab. The table_types table contains an index of all NEON tabular data tables and their types. If a table is not found on this list, the findTablesByFormat() function attempts to infer the type based on the format of the file name, but the table_types table is the definitive source.
