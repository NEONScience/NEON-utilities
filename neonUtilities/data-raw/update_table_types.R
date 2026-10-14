# update_table_types.R
##############################################################################################
#' @title Update table_types data frame in sysdata.rda

#' @author
#' Christine Laney \email{claney@battelleecology.org}

#' @description
#' Connect to the DPS database, pull in updated information about tables, and save locally.

#' @return A saved .rda file

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2018-10-05) converted to get tables from PDR via the data service
#   Christine Laney (2017-10-19)
##############################################################################################

update_table_types <- function(){
  
  options(stringsAsFactors=F)
  
  # get publication tables from PDR
  req <- httr::GET(Sys.getenv("PUB_TABLES"))
  rawj <- jsonlite::fromJSON(httr::content(req, as="text", encoding="UTF-8"), 
                             simplifyDataFrame=TRUE, flatten=TRUE)
  
  table_types <- rawj$pubTables[,c("dataProductId", "tableName",
                         "description", "tableType")]
  tmi <- apply(rawj$pubTables, 1, FUN=function(x) {
    tmx <- unique(x$pubFields$timeIndex)
    if(length(tmx)>1) {
      print(x$tableName)
      print(x$dataProductId)
    }
    return(tmx)
  })
  table_types <- cbind(table_types, tmi)
  colnames(table_types) <- c("productID", "tableName", "tableDesc", 
                             "tableType", "tableTMI")
  
  # need to keep table definitions for tables that only exist in RELEASE-2021
  table_types <- rbind(table_types, release_2021)
  
  rownames(table_types) <- 1:nrow(table_types)
  
  # term definitions for fields added by stackByTable
  added_fields <- data.frame(cbind(fieldName=c('domainID','siteID','horizontalPosition',
                                               'verticalPosition','publicationDate',
                                               'release'),
                                   description=c('Unique identifier of the NEON domain',
                                                 'NEON site code',
                                                 'Index of horizontal location at a NEON site',
                                                 'Index of vertical location at a NEON site',
                                                 'Date of data publication on the NEON data portal',
                                                 'Identifier for data release'),
                                   dataType=c(rep('string',4),'dateTime','string'),
                                   units=rep(NA,6),
                                   downloadPkg=rep('appended by stackByTable',6),
                                   pubFormat=rep(NA,6),
                                   primaryKey=rep('N',6),
                                   categoricalCodeName=rep('',6)))
  
  usethis::use_data(table_types, added_fields, science_review_variables, shared_flights, 
                    sensor_position_variables, sensor_position_legacy,
                    shared_aquatic, release_2021, chem_bundles, other_bundles, relevant_EPSG, 
                    cit_prov_template, frame_file_codes, frame_names_internal,
                    frame_file_variables_internal,
                    internal=TRUE, overwrite=TRUE)
  usethis::use_data(table_types, shared_flights, shared_aquatic, chem_bundles, other_bundles, 
                    internal=FALSE, overwrite=TRUE)
  
  # write tables to repo for use outside package
  data.table::fwrite(added_fields, paste(Sys.getenv('NU_REPO'), 'helper_files/added_fields.csv', sep='/'))
  data.table::fwrite(chem_bundles, paste(Sys.getenv('NU_REPO'), 'helper_files/chem_bundles.csv', sep='/'))
  data.table::fwrite(other_bundles, paste(Sys.getenv('NU_REPO'), 'helper_files/other_bundles.csv', sep='/'))
  data.table::fwrite(release_2021, paste(Sys.getenv('NU_REPO'), 'helper_files/release_2021.csv', sep='/'))
  data.table::fwrite(shared_aquatic, paste(Sys.getenv('NU_REPO'), 'helper_files/shared_aquatic.csv', sep='/'))
  data.table::fwrite(shared_flights, paste(Sys.getenv('NU_REPO'), 'helper_files/shared_flights.csv', sep='/'))
  data.table::fwrite(table_types, paste(Sys.getenv('NU_REPO'), 'helper_files/table_types.csv', sep='/'))
  data.table::fwrite(frame_file_codes, paste(Sys.getenv('NU_REPO'), 'helper_files/frame_file_codes.csv', sep='/'))
  data.table::fwrite(frame_names_internal, paste(Sys.getenv('NU_REPO'), 'helper_files/frame_names.csv', sep='/'))
  data.table::fwrite(frame_file_variables_internal, paste(Sys.getenv('NU_REPO'), 'helper_files/frame_file_variables.csv', sep='/'))
  
}

update_table_types()
