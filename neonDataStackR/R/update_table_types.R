# update_table_types.R
# Hidden from public
library(RMySQL)
dps_con <-  dbConnect(drv=RMySQL::MySQL(), dbname="dps_database", username="root",password="p@ssw0rd", host="10.206.27.43", root = 3306)
table_types <- dbGetQuery(dps_con, "SELECT * from dpTable WHERE dpTable.tableType != 'ingest'")
save(table_types, file = "data/table_types.rda")
