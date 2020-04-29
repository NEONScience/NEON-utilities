#' Publication table information
#'
#' A dataset containing publication table names, descriptions, type (site-date, site-all, lab-all, lab-current), and a time index
#'
#' @format A data frame with 5 variables. Number of rows changes frequently as more tables are added:
#' \describe{
#'   \item{productID}{data product ID}
#'   \item{tableName}{name of table}
#'   \item{tableDesc}{description of table}
#'   \item{tableType}{type of table (important for knowing which tables to stack, and how to stack)}
#'   \item{tableTMI}{a time index (e.g., 0 = native resolution, 1 = 1 minute, 30 = 30 minute averages or totals)}
#' }
#' @source NEON database
"table_types"
