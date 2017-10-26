# update_table_types.R
##############################################################################################
#' @title Update table_types.rda

#' @author
#' Christine Laney \email{claney@battelleecology.org}

#' @description
#' Connect to the DPS database, pull in updated information about tables, and save locally.

#' @return A saved .rda file

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Christine Laney (2017-10-19)
##############################################################################################

update_table_types <- function(){
  library(RMySQL)
  dps_con <-  dbConnect(drv=RMySQL::MySQL(), dbname="dps_database", username="root",password="p@ssw0rd", host="10.206.27.43", root = 3306)
  table_types <- dbGetQuery(dps_con, "SELECT * from dpTable WHERE dpTable.tableType != 'ingest'")
  save(table_types, file = "data/table_types.rda")
}
