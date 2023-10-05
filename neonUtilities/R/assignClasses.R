##############################################################################################
#' @title Assign correct column classes

#' @author
#' Christine Laney \email{claney@battelleecology.org}

#' @description
#' Use the variables file to assign classes to each column in each data file
#'
#' @keywords internal
#' @param dt A data frame
#' @param inVars The variables expected in the df
#' @return A data frame with corrected column classes

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   2017-09-28 (Christine Laney): Created original function
#   2018-04-13 (Christine Laney): stackDataFiles now generates a data.table rather than a data.frame;
#     convert back to data.frame for checking and updating column types, then convert back to data.table.
##############################################################################################

assignClasses <- function(dt, inVars){
  df <- as.data.frame(dt)
  for(i in 1:ncol(df)){
    if(names(df)[i] %in% inVars$fieldName){
      type <- inVars$colClass[which(inVars$fieldName==names(df)[i])[1]]
      if(type == "numeric")
        df[,i] <- as.numeric(df[,i])
      if(type == "character")
        df[,i] <- as.character(df[,i])
    }
  }
  dt <- data.table::as.data.table(df)
  return(dt)
}
