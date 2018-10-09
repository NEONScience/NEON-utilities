#' Publication table information
#'
#' A dataset containing publication table names, descriptions, type (site-date, site-all, lab-all, lab-current), and a time index
#'
#' @format A data frame with 4 variables. Number of rows changes frequently as more tables are added:
#' \describe{
#'   \item{productID}{data product ID}
#'   \item{tableName}{name of table}
#'   \item{tableDesc}{description of table}
#'   \item{tableType}{type of table (important for knowing which tables to stack and which to not stack)}
#'   \item{tableTMI}{a time index (e.g., 1 = native resolution or 1 minute, 30 = 30 minute averages or totals)}
#' }
#' @source dps database
"table_types"
