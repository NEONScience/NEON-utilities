###################################
########### Disclaimer ############
This is the most recent readme publication based on all site-date combinations used during stackByTable.
Information specific to the query, including sites and dates, has been removed. The remaining content reflects general metadata for the data product.
All files used during stacking are listed at the bottom of this document, which includes the data publication dates.
##################################

This data package been produced by and downloaded from the National Ecological Observatory Network, managed cooperatively by Battelle. These data are provided under the terms of the NEON data policy at http://data.neonscience.org/data-policy. 

DATA PRODUCT INFORMATION
------------------------

ID: NEON.DOM.SITE.DP1.10109.001

Name: Soil microbe group abundances

Description: Counts and relative abundances of marker genes from total archaea, bacteria, and fungi observed by qPCR in soil microbial communities

NEON Science Team Supplier: TOS

Abstract: This data product contains the quality-controlled laboratory data and metadata for NEON's soil bacterial, archaeal, and fungal group abundances analysis, which are derived from soil microbial sampling. Group abundances are quantified via qPCR on frozen, field-collected soils. For additional details, see protocol [NEON.DOC.014048](http://data.neonscience.org/api/v0/documents/NEON.DOC.014048vJ): TOS Protocol and Procedure for Soil Biogeochemical and Microbial Sampling; and science design [NEON.DOC.000908vA](http://data.neonscience.org/api/v0/documents/NEON.DOC.000908vA): TOS Science Design for Terrestrial Microbial Diversity.

Brief Design Description: Three predetermined, randomly assigned locations are selected for each sampling event within each of 10 plots distributed throughout a site; sampling locations within a plot are not re-sampled. Soil sampling occurs once (sites with short growing seasons) to three times a year (sites with longer growing seasons), will all sites sampling during the historic peak in vegetation greenness. Soil samples are collected to a maximum depth of 30 cm, with organic and mineral soils sampled separately. Subsamples of homogenized soil (rocks, roots and organic debris removed) from each of the 3 sampling locations are stored in sterile containers, frozen on dry ice in the field and shipped to an analytical facility for DNA extraction, sample preparation and qPCR analysis using primer sets targeting the small subunit of the ribosomal RNA gene.

Brief Study Area Description: These data are collected at all NEON terrestrial sites.

Keywords: soil, archaea, microbes, fungi, bacteria, microbe abundances, group abundances, quantitative polymerase chain reaction (qPCR)


DATA PACKAGE CONTENTS
---------------------

This data product contains up to 5 data tables:

mga_soilGroupAbundances - Laboratory results of gene copy number data in soil samples
mga_soilLabSummary - Summary data on laboratory methods for qPCR results in soil samples
mga_soilBatchResults - Batch-level results of analysis of gene copy number in soil samples
mga_batchResults - Batch-level results of analysis of gene copy number in soil samples
mga_labSummary - Summary data on laboratory methods for qPCR results in soil samples
If data are unavailable for the particular sites and dates queried, some tables may be absent.
Basic download package definition: The basic package includes the group abundance data.

Expanded download package definition: The expanded package includes two additional tables that provide batch level metadata and summary information about the lab methodology.

FILE NAMING CONVENTIONS
-----------------------

NEON data files are named using a series of component abbreviations separated by periods. File naming conventions for NEON data files differ between NEON science teams. A file will have the same name whether it is accessed via the data portal or the API.

NEON observational systems (OS) data files: NEON.DOM.SITE.DPL.PRNUM.REV.DESC.YYYY-MM.PKGTYPE.GENTIME.csv

The definitions of component abbreviations are below. See NEON.DOC.002651: NEON Data Product Numbering Convention, located at http://data.neonscience.org/documents for more information.

General conventions, used for all data products:
   NEON: denotes the organizational origin of the data product and identifies the product as operational; data collected as part of a special data collection exercise are designated by a separate, unique alphanumeric code created by the PI.

   DOM: a three-character alphanumeric code, referring to the domain of data acquisition (D01 - D20).

   SITE: a four-character alphanumeric code, referring to the site of data acquisition; all sites are designated by a standardized four-character alphabetic code.

   DPL: a three-character alphanumeric code, referring to data product processing level;

   PRNUM: a five-character numeric code, referring to the data product number (see the Data Product Catalog at http://data.neonscience.org/data-product-catalog).

   REV: a three-digit designation, referring to the revision number of the data product. The REV value is incremented by 1 each time a major change is made in instrumentation, data collection protocol, or data processing such that data from the preceding revision is not directly comparable to the new.

   HOR: a three-character designation, referring to measurement locations within one horizontal plane. For example, if five surface measurements were taken, one at each of the five soil array plots, the number in the HOR field would range from 001-005. 

   VER: a three-character designation, referring to measurement locations within one vertical plane. For example, if eight air temperature measurements are collected, one at each tower vertical level, the number in the VER field would range from 010-080. If five soil temperature measurements are collected below the soil surface, the number in the VER field would range from 501-505. 

   TMI: a three-character designation, referring to the temporal representation, averaging period, or coverage of the data product (e.g., minute, hour, month, year, sub-hourly, day, lunar month, single instance, seasonal, annual, multi-annual). 000 = native resolution, 001 = native resolution (variable or regular) or 1 minute, 002 = 2 minute, 005 = 5 minute, 015 = 15 minute, 030 = 30 minute, 060 = 60 minutes or 1 hour, 100 = approximately once per minute at stream sites and once every 5-10 minutes at buoy sites (lakes/rivers), 101-103 = native resolution of replicate sensor 1, 2, and 3 respectively, 999 = Sensor conducts measurements at varied interval depending on air mass, 01D = 1 day, 01M = 1 month, 01Y = 1 year.

   DESC: an abbreviated description of the data file or table.

   YYYY-MM: the year and month of the data in the file.

   PKGTYPE: the type of data package downloaded. Options are 'basic', representing the basic download package, or 'expanded',representing the expanded download package (see more information below).

   GENTIME: the date-time stamp when the file was generated, in UTC. The format of the date-time stamp is YYYYMMDDTHHmmSSZ.

Time stamp conventions:
   YYYY: Year
   YY: Year, last two digits only
   MM: Month: 01-12
   DD: Day: 01-31
   T: Indicator that the time stamp is beginning
   HH: Hours: 00-23
   mm: Minutes: 00-59
   SS: Seconds: 00-59
   Z: Universal Time Coordinated (Universal Coordinated Time), or UTC

ADDITIONAL INFORMATION
----------------------

Data products that are a source of this data product:

Data products that are derived from this data product:

Other related data products (by sensor, protocol, or variable measured):
NEON.DOM.SITE.DP1.10078.001, Soil chemical properties (Distributed periodic)
NEON.DOM.SITE.DP1.10100.001, Soil stable isotopes (Distributed periodic)
NEON.DOM.SITE.DP1.10104.001, Soil microbe biomass
NEON.DOM.SITE.DP1.10108.001, Soil microbe marker gene sequences
NEON.DOM.SITE.DP1.20277.001, Benthic microbe group abundances
NEON.DOM.SITE.DP1.20278.001, Surface water microbe group abundances
NEON.DOM.SITE.DP1.10107.001, Soil microbe metagenome sequences
NEON.DOM.SITE.DP1.10081.001, Soil microbe community composition
NEON.DOM.SITE.DP1.10080.001, Soil inorganic nitrogen pools and transformations
NEON.DOM.SITE.DP1.10086.001, Soil physical properties (Distributed periodic)

Obfuscation of Personnel Information: At times it is important to know which data were collected by particular observers. In order to protect privacy of NEON technicians while also providing a way to consistently identify different observers, we obfuscate each NEON personnel name by internally linking it to a unique string identifier (e.g., Jane Doe=ByrziN0LguMJHnInl2NM/trZeA5h+c0) and publishing only the identifier.

CHANGE LOG
----------

ADDITIONAL REMARKS
------------------

Queries for this data product will return data from mga_soilGroupAbundances for all dates within the specified date range, as well as data from all dates for the mga_batchResults and mga_labSummary (if the expanded package is selected). Note that both soil and aquatic samples may be analyzed in the same batch and using the same methods. As such, batch and lab summary tables may include data for soil and aquatic samples. A given mga_soilGroupAbundances.dnaSampleID is expected to generate one record per targetTaxonGroup. Duplicate samples and/or missing data may exist where protocol and/or data entry aberrations have occurred; users should check data carefully for anomalies before joining tables.

NEON DATA POLICY AND CITATION GUIDELINES
----------------------------------------

Please visit http://data.neonscience.org/data-policy for more information about NEON's data policy and citation guidelines.

DATA QUALITY AND VERSIONING
---------------------------

The data contained in this file are considered provisional. Updates to the data, QA/QC and/or processing algorithms over time will occur on an as-needed basis.  Please check back to this site for updates tracked in change logs.  Query reproducibility on provisional data cannot be guaranteed. 
 
Starting in 2020 or earlier, NEON will begin to offer static versions of each data product, annotated with a globally unique identifier. Versioned IS and OS data will be produced by reprocessing each IS and OS data product from the beginning of the data collection period to approximately 12-18 months prior to the reprocessing date (to allow for calibration checks, return of external lab data, etc.). The reprocessing step will use the most recent QA/QC methods and processing algorithms. Versioned AOP data will be produced by reprocessing the entire AOP archive as advances in algorithms and processing technology are incorporated. This will typically occur in the northern winter months, between flight season peaks, and will be on the order of every 3 to 5 years in frequency.

POST STACKING README DOCUMENTATION
----------------------------------

Each row contains the readme filename used during stackByTable

NEON.D04.LAJA.DP1.10109.001.readme.20180705T154613Z.txt
NEON.D06.UKFS.DP1.10109.001.readme.20180705T163900Z.txt
NEON.D09.NOGP.DP1.10109.001.readme.20180705T173721Z.txt
