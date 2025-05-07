##############################################################################################
#' @title Returns the most recent files for those that do not need stacking

#' @author
#' Nathan Mietkiewicz \email{mietkiewicz@battelleecology.org}

#' @description
#' Given a list of files, this will order and return the file with the most recent publication date.

#' @param inList The list of files.
#' 
#' @keywords internal

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   2019-10-08 (Nathan Mietkiewicz): Original creation
#   2020-11-05 (Claire Lunch): Modify for compatibility with stackFromStore()

##############################################################################################
getRecentPublication <- function(inList) {
  
  # if file dates are available, use file dates
  file_dates <-  lapply(basename(inList), function(x) {
    regmatches(x, regexpr("[0-9]{8}T[0-9]{6}Z", x))
  })
  # if dates can't be found, move to dates in file path - for release 2021, these are all identical
  if(length(unlist(file_dates))==0) {
    
    path_dates <-  lapply(inList, function(x) {
      regmatches(x, regexpr("[0-9]{8}T[0-9]{6}Z", x))
    })
    # if dates can't be found, use "max" of input file names. This should be very rare.
    if(length(unlist(path_dates))==0) {
      file_dates <- inList
    } else {
      file_dates <- path_dates
    }
    
  }
  
  outList <- inList[grep(max(unlist(file_dates)), inList)][1]
  return(list(outList, ifelse(nchar(file_dates[[1]])==16, max(unlist(file_dates)),
                              "undetermined")))
}