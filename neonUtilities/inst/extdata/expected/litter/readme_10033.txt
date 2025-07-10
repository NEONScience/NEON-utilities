###################################
########### Disclaimer ############
This is the most recent readme publication based on all site-date combinations used during stackByTable.
Information specific to the query, including sites and dates, has been removed. The remaining content reflects general metadata for the data product.
##################################

This data package was produced by and downloaded from the National Ecological Observatory Network (NEON). NEON is
funded by the National Science Foundation (Awards 0653461, 0752017, 1029808, 1138160, 1246537, 1638695, 1638696,
1724433) and managed cooperatively by Battelle. These data are provided under the terms of the NEON data policy at
https://www.neonscience.org/data-policy.
DATA PRODUCT INFORMATION
------------------------
ID: NEON.DOM.SITE.DP1.10033.001
Name: Litterfall and fine woody debris production and chemistry
Description: Dry weight of litterfall and fine woody debris collected from elevated and ground traps, sorted to functional group, as well as periodic measurements of litter chemistry and stable isotopes.
NEON Science Team Supplier: Terrestrial Observation System
Abstract: This data product contains the quality-controlled, native sampling resolution data from NEON's Litterfall and fine woody debris sampling. Litter is defined as material that is dropped from the forest canopy and has a butt end diameter <2cm and a length <50 cm; this material is collected in elevated 0.5m2 PVC traps. Fine woody debris is defined as material that is dropped from the forest canopy and has a butt end diameter <2cm and a length >50 cm; this material is collected in ground traps as longer material is not reliably collected by the elevated traps. Following field collection, each sample is sorted by functional group, dried and weighed. After sorting litter by functional group, material from elevated traps from select bouts is sent for chemical analysis, with excess material archived in the NEON Biorepository and available upon request. For additional details, see the user guides, protocols, and science design listed in the Documentation section below. Products resulting from this sampling include mass of litterfall and fine woody debris by functional group as well as litter chemistry and stable isotope ratios. Summary tables of external lab precision and accuracy are included in the expanded package.
Latency:
The expected time from data and/or sample collection in the field to data publication is as follows, for each of the data tables (in days) in the downloaded data package. See the Data Product User Guides for more information.
bgc\_CNiso\_externalSummary: 30
lig_externalSummary: 30
ltr_chemistrySubsampling: 60
ltr_fielddata: 60
ltr_litterCarbonNitrogen: 270
ltr_litterLignin: 180
ltr_massdata: 60
ltr_pertrap: 30
Brief Design Description: Each qualifying tower plot in forested ecosystems has 0-2 elevated traps and 1-2 ground traps deployed. Ground traps are sampled annually. Sampling interval of elevated litter traps is variable by dominant overstory vegetation. Deciduous forests are sampled once in the spring then multiple times during fall senescence; evergreen and coniferous forests are sampled year round at monthly intervals. Traps are consistent with those used by the Smithsonian Center for Tropical Forest Science (CTFS). Mass data for each collection event are measured separately for functional groups: Leaves, Needles, Twigs/branches, Woody material, Seeds, Flowers (which includes other non-woody reproductive structures), and Other. Any material that cannot be sorted due to time or resource constraints is categorized as Mixed (unsorted). Once every five years, material from elevated traps from a single collection event are analyzed for carbon and nitrogen concentrations and stable isotopes as well as lignin concentrations. Prior to 2022, only leaf and needle materials were analyzed. Starting in 2022, all functional group material is measured for chemistry, though the non-leaf and needle groups use a pooled site-level sample.
Brief Study Area Description: These data are collected at NEON terrestrial sites with overstory vegetation.
Keywords: biodiversity, biomass, carbon cycle, d13C, d15N, detritus, fine woody debris, gross primary productivity (GPP), leaves, litter, litterfall, ltr, net primary productivity (NPP), nitrogen cycle, plant productivity, production, senescence, turnover, vegetation
Domain: D05
DATA PACKAGE CONTENTS
---------------------
This folder contains the following documentation files:
This data product contains up to 9 data tables:
- Term descriptions, data types, and units: NEON.D05.STEI.DP1.10033.001.variables.20241118T123447Z.csv
ltr_vegetationCover - Cover of qualifying vegetation from AOP survey
ltr_massdata - Dry mass of litter and fine woody debris components per trap per bout
ltr_pertrap - Record of trap establishment, contains date, trap type and location
ltr_chemistrySubsampling - Identifiers for subsamples created for chemical analyses or archive
ltr_litterLignin - External lab analysis of lignin concentrations in litter
ltr_fielddata - Field collection details and sample tracking
ltr_litterCarbonNitrogen - External lab analysis of carbon and nitrogen concentrations in litter
bgc_CNiso_externalSummary - Long-term uncertainty values for analysis of carbon and nitrogen concentrations and stable isotopes
lig_externalSummary - Long-term uncertainty values for lignin analysis in plant tissue
If data are unavailable for the particular sites and dates queried, some tables may be absent.
Basic download package definition: The basic data package includes the primary measurements.
Expanded download package definition: The expanded data package contains all the basic package data, plus additional tables containing analytical laboratory precision and accuracy.
FILE NAMING CONVENTIONS
-----------------------
NEON data files are named using a series of component abbreviations separated by periods. File naming conventions
for NEON data files differ between NEON science teams. A file will have the same name whether it is accessed via
NEON's data portal or API. Please visit https://www.neonscience.org/data-formats-conventions for a full description
of the naming conventions.
ISSUE LOG
----------
This log provides a list of issues identified during data collection or processing, prior to publication
of this data package. For a more recent log, please visit this data product's detail page at
https://data.neonscience.org/data-products/DP1.10033.001.
Issue Date: 2024-11-08
Issue: Elevated trapSize incorrect at MLBS: Elevated traps at all plots at MLBS were constructed incorrectly causing the trapSize to be non-standard, 0.64 m versus the standard size of 0.5 m. In addition, the elevated trap located at MLBS_073_872 was fixed to the correct size on 11/09/2022, so data collected before this date only have the non-standard trapSize of 0.64. Data in releases prior to 2025 indicate the incorrect trap size for elevated traps at MLBS.
    Date Range: 2017-08-23 to 2024-12-31
    Location(s) Affected: MLBS
Resolution Date: 
Resolution: All data records have been corrected in the ‘ltr_perTrap’ data table to reflect the non-standard trapSize of 0.64 m. MLBS_073_872 had additional corrections for records collected after 11/09/2022 to indicate the trapSize of 0.5 m. During the first collection bout of 2025 at MLBS, all existing elevated traps will be re-established to new locations, with new trapIDs and with the correct trap dimensions.  Edits will be present in provisional data and in the RELEASE-2025 and onward.
Issue Date: 2024-09-18
Issue: Chemistry data errors: NEON discovered errors in 21 litterfall chemistry samples. Most of the issues were in the CNratio field while a few samples had carbonPercent errors.
    Date Range: 2018-04-27 to 2020-10-07
    Location(s) Affected: GUAN, RMNP
Resolution Date: 2024-12-31
Resolution: Working with the analytical lab, these data errors were corrected and updated values will be available starting in RELEASE-2025. Note that the old, erroneous values will be present in earlier releases. Contact us for information on exactly what values changed for which samples.
Issue Date: 2024-01-02
Issue: No 'ltr_vegetationCover' table: Unable to scale mass from targeted trap placement to plot or site levels prior to 2024.
    Date Range: 2013-01-01 to 2024-01-01
    Location(s) Affected: BART, HARV, BLAN, SCBI, SERC, JERC, OSBS, GUAN, STEI, TREE, UNDE, KONZ, UKFS, GRSM, MLBS, ORNL, LENO, DELA, TALL, RMNP, CLBJ, YELL, NIWO, SRER, ABBY, WREF, SJER, SOAP, TEAK, BONA, DEJU, HEAL, PUUM
Resolution Date: 2024-01-15
Resolution: Initially, data for the Litterfall and fine woody debris production and chemistry data product (DP1.10033.001) were published without data on total cover of qualifying vegetation (woody individual > 2M height). For sites with targeted trap placement, this resulted in an inability to scale litter production to plot or site levels. RELEASE-2024 and releases and provisional data thereafter include a new table 'ltr_vegetationCover' that provides cover of qualifying woody vegetation at the scale of the subplot for all deployed traps at all sites (both targeted and random trap placement strategies).
Issue Date: 2024-09-18
Issue: Analytical lab switch: NEON litter chemistry analyses are conducted by external laboratories. After working with the same lab for many years to analyze carbon and nitrogen concentrations and stable isotope, results of a competitive process resulted in selection of a new lab to conduct these analyses starting in 2024. In order to assess the possible data implications of this lab switch, a set of samples was analyzed by both labs.
    Date Range: 2016-10-10 to 2023-12-05
    Location(s) Affected: All
Resolution Date: 2024-01-01
Resolution: While there were significant differences in observed chemistry values between the two labs according to paired t-tests, the differences were subtle and did not exceed the long-term analytical uncertainty except for delta 15N. For this analyte, values were higher (on the order of 0.5 per mill) when analyzed by the new lab compared to the previous lab; users should interpret the data in this context. For a more detailed look at the lab comparison results, see https://data.neonscience.org/api/v0/documents/CNiso_Lab_Change_2024_Results
Issue Date: 2023-09-25
Issue: Lab summary correction for C-N data: The table `bgc_CNiso_externalSummary` reports the precision and accuracy of carbon and nitrogen concentration and stable isotope measurements of plant and soil samples, based on analysis of secondary reference materials treated as unknowns. The University of Wyoming Stable Isotope Facility had been reporting their summary statistics since 2015 but had mis-interpreted the definitions of some reported uncertainty fields.
    Date Range: 2015-03-16 to 2023-08-18
    Location(s) Affected: All terrestrial sites
Resolution Date: 2023-12-31
Resolution: The lab re-generated all summary statistics according to the formulas and definitions expected by NEON and uploaded these corrected values to the NEON data portal. This updated version of the `bgc_CNiso_externalSummary` table is available with download of provisional data and will appear in RELEASE-2024 and onward.
Issue Date: 2023-11-09
Issue: SubplotId change: The Base plot subplot naming convention was ambiguous with respect to location and scale, and not well differentiated from the identifiers of the points that spatially define Base plots.
    Date Range: 2012-01-01 to 2023-12-31
    Location(s) Affected: All terrestrial sites
Resolution Date: 2023-12-31
Resolution: The subplot naming convention is updated across all data products with observations and samples that originate from Base plots. The format consists of the identity of the plot point in the southwest corner of the subplot, the scale or size of the subplot, and, for those subplots smaller than 100 m2, the corner of the 100 m2 subplot in which the smaller subplot is located. For example, ‘21_400’ refers to point 21 in the southwest corner and is 400 m2 (20 m x 20 m), and ‘31_1_1’ is a subplot with point 31 in the southwest corner, is 1 m2, and is in corner 1 of the 100 m2 subplot with point 31 in the southwest corner. See the associated Data Notification for further detail: https://www.neonscience.org/impact/observatory-blog/neon-terrestrial-observation-system-tos-base-plot-subplot-renaming.
Issue Date: 2023-04-12
Issue: Subplot changed from 23 to subplot 39 at LENO_069.
    Date Range: 2016-07-01 to 2023-04-01
    Location(s) Affected: LENO_069
Resolution Date: 2023-04-01
Resolution: Due to persistent flooding, it is no longer possible to sample subplot 23 in Tower plot LENO_069. All sampling in subplot 23 has been discontinued. Subplot 39 will be used for all sampling activities previously scheduled for subplot 23.
Issue Date: 2022-09-13
Issue: Severe flooding destroyed several roads into Yellowstone National Park in June 2022, making the YELL and BLDE sites inaccessible to NEON staff. Observational data collection was halted during this time. Canceled data collection events are indicated in data records via the samplingImpractical field.
    Date Range: 2022-06-12 to 2022-10-31
    Location(s) Affected: YELL
Resolution Date: 2022-10-31
Resolution: Normal operations resumed on October 31, 2022, when the National Park Service opened a newly constructed road from Gardiner, MT to Mammoth, WY with minimal restrictions. For more details about data impacts, see Data Notification https://www.neonscience.org/impact/observatory-blog/data-impacts-neons-yellowstone-sites-yell-blde-due-catastrophic-flooding-0
Issue Date: 2022-11-15
Issue: Functional groups, chemistry: The chemistry of litterfall material other than leaf and needle functional groups was not measured, even though in some sites these materials contribute a substantial amount to litter mass flux. Without knowing the chemistry of all material types, it is difficult to calculate mass-weighted fluxes of carbon, nitrogen, lignin, and stable isotopes without making significant assumptions.
    Date Range: 2016-01-01 to 2022-02-22
    Location(s) Affected: All terrestrial sites
Resolution Date: 2022-02-23
Resolution: Starting in the 2022 field season, all functional groups are analyzed for chemistry and stable isotopes. As before, leaf and needle materials are measured at the plot scale, whereas the other functional groups measure a site-level pooled sample combining material across plots. In both cases, material is from a single bout with high fluxes for the specific group. Chemistry measurements still occur once every five years per site.
Issue Date: 2020-11-19
Issue: Safety measures to protect personnel during the COVID-19 pandemic resulted in reduced or canceled sampling activities for extended periods at NEON sites. Data availability may be reduced during this time.
    Date Range: 2020-03-23 to 2021-12-31
    Location(s) Affected: All terrestrial sites
Resolution Date: 2021-12-31
Resolution: The primary impact of the pandemic on observational data was reduced data collection. Training procedures and data quality reviews were maintained throughout the pandemic, although some previously in-person training was conducted virtually.  Scheduled measurements and sampling that were not carried out due to COVID-19 or any other causes are indicated in data records via the samplingImpractical data field.
Issue Date: 2021-02-16
Issue: Limited trap collection: Severely limited elevated litter trap collection for the 2020 season. Because of extended litter collection periods due to sampling limitations during COVID restrictions, the incidence of litter trap disturbance by resident black bears was unusually high. Over 2 collection bouts (40 traps total), only 5 traps were successfully collected in both bouts. It is not recommended these data be used to calculate productivity for the 2020 sampling year at GRSM.
    Date Range: 2020-01-01 to 2021-01-01
    Location(s) Affected: GRSM
Resolution Date: 2021-02-16
Resolution: Litter collection frequency returned to more normal levels as of the 2021 field season.
Issue Date: 2020-12-01
Issue: Samples not re-dried: Due to a miscommunication, samples analyzed for carbon (C) and nitrogen (N) concentrations and stable isotopes were not re-dried prior to weighing and analysis at the external lab. While all NEON litterfall samples are dried at 65C in the domain labs, they are sometimes then stored in paper bags or coin envelopes for weeks to months before being ground, transferred to vials, and shipped. During this time they may accumulate moisture, especially in humid areas. Subsequent testing revealed that %C data are likely underestimated by 1.5-2.5% due to this lack of re-drying prior to analysis. As vegetation samples tend to have high %C (20% - 55%), this bias may have only minor impacts on many analyses, but is something for end users to keep in mind. For the other parameters (%N, C:N, d15N, d13C), testing suggests there were no detectable differences between re-dried samples and originals.
    Date Range: 2016-01-01 to 2020-08-15
    Location(s) Affected: All sites with litterfall chemistry measurements in this date range, with the exception of GUAN and PUUM whose tissues were re-dried for permitting/quarantine reasons.
Resolution Date: 2020-11-10
Resolution: Affected data have been flagged with dataQF = dryingProtocolError in the `ltr_litterCarbonNitrogen` table. For sample analysis dates starting in November 2020, all carbon-nitrogen samples are re-dried at 65C prior to analysis to drive out any residual moisture and improve data accuracy for % C. Samples collected in 2020 may have been analyzed before or after the change; check dataQF to determine which individual samples are affected.
Issue Date: 2020-10-02
Issue: Data products not bundled: Until October 2020, litter biomass, chemistry, and stable isotopes were published as separate data products.
    Date Range: 2016-01-01 to 2020-10-06
    Location(s) Affected: All terrestrial sites.
Resolution Date: 2020-10-06
Resolution: In October 2020, data tables for chemistry and isotopes were bundled with the sampling and biomass data tables in a single data product for improved usability. This applies to all existing and future data.
Issue Date: 2020-01-01
Issue: Sampling numbers reduced: Discontinued litterfall sampling as some plots.
    Date Range: 2016-01-01 to 2020-01-01
    Location(s) Affected: BART, HARV, SCBI, SERC, JERC, TREE, UNDE, GRSM, ORNL, TALL
Resolution Date: 2020-06-08
Resolution: In January 2020, litter traps were removed from a subset of plots at sites where data analyses indicated reliable site level estimates of litter production were possible with a reduced number of plots. Plots to continue sampling for litterfall were selected to maintain spatial balance across the Tower airshed. In June 2020, plot selection at these sites was revised to prioritize stratification across vegetation types.
ADDITIONAL INFORMATION
----------------------
Queries for this data product will return data from all dates for `ltr_pertrap` (which may be established many years before a litter collection event), whereas the other tables will be subset to data collected during the date range specified. The protocol dictates that each trap is established once (one expected record per `ltr_pertrap.trapID`).  A record from `ltr_pertrap` may have zero or more child records in `ltr_fielddata.trapID`, depending on the date range of the data downloaded; a given `ltr_fielddata.trapID` is expected to be sampled zero or one times per collectDate (local time). A record from `ltr_fielddata` may have zero (if no litter collected) or more child records in `ltr_massdata` depending on the functional groups contained in the trap and whether reweighing occurred for QA purposes. A record from `ltr_massdata` may be pooled with one or more other `ltr_massdata` records into zero (if not sent for chemistry analyses) or one child record in `ltr_chemistrySubsampling`. Chemistry subsamples may appear one or more times in `ltr_litterCarbonNitrogen` and `ltr_litterLignin`, depending on whether analytical replicates were conducted or if C and N were analyzed separately. Duplicates may exist where protocol and/or data entry aberrations have occurred; users should check carefully for anomalies before joining tables.
NEON DATA POLICY AND CITATION GUIDELINES
----------------------------------------
A citation statement is available in this data product's detail page at
https://data.neonscience.org/data-products/DP1.10033.001. Please visit https://www.neonscience.org/data-policy for
more information about NEON's data policy and citation guidelines.
DATA QUALITY AND VERSIONING
---------------------------
NEON data are initially published with a status of Provisional, in which updates to data and/or processing
algorithms will occur on an as-needed basis, and query reproducibility cannot be guaranteed. Once data are published
as part of a Data Release, they are no longer provisional, and are associated with a stable DOI.
To learn more about provisional versus released data, please visit
https://www.neonscience.org/data-revisions-releases.
