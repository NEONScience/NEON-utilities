##############################################################################################
#' @title Returns the most recent files for those that do not need stacking

#' @author
#' Nathan Mietkiewicz \email{mietkiewicz@battelleecology.org}

#' @description
#' Given a list of files, this will order and return the file with the most recent publication date.

#' @param inList The list of files.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   2019-10-08 (Nathan Mietkiewicz): Original creation
#   2020-11-05 (Claire Lunch): Modify for compatibility with stackFromStore()

##############################################################################################
getRecentPublication <- function(inList) {
  path_dates <-  lapply(inList, function(x) {
    regmatches(x, regexpr("[0-9]{8}T[0-9]{6}Z", x))
  })
  # if dates can't be found, use "max" of input file names. This should be rare.
  if(length(unlist(path_dates))==0) {
    path_dates <- inList
  }
  outList <- inList[grep(max(unlist(path_dates)), inList)][1]
  return(list(outList, ifelse(nchar(path_dates[[1]])==16, max(unlist(path_dates)),
                              "undetermined")))
}