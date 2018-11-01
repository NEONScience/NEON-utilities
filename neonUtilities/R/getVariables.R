##############################################################################################
#' @title Get correct data types

#' @author
#' Christine Laney \email{claney@battelleecology.org}

#' @description
#' Support way to force R to read assign correct data types to each column based on variables file
#'
#' @keywords internal
#' @param varFile A file that contains variable definitions
#' @return A data frame with fieldName and assigned column class, along with table if present

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Christine Laney (2017-09-28)
##############################################################################################

getVariables <- function(varFile){
  d <- utils::read.csv(varFile, header = T, stringsAsFactors = F)                     # read in a variables file
  d$colClass <- rep("numeric", nrow(d))                                       # make a new colClass column defaulting to numeric
  d$colClass[which(d$dataType %in% c("string", "date", "dateTime", "uri"))] <- "character"   # change to character if string or dateTime
  if("table" %in% names(d)){                                                   # OS variables files have table, IS do not
    return(d[, c("table", "fieldName", "colClass")])
  }
  return(d[, c("fieldName","colClass")])
}
