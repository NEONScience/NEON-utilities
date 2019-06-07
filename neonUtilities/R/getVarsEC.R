##############################################################################################
#' @title Extract list of eddy covariance variables from HDF5 files

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Extracts a list of variables contained in a set of HDF5 files. Specific to eddy covariance data product: DP4.00200.001
#'
#' @param filepath The folder containing the H5 files [character]

#' @return A data frame of the specified variables

#' @examples
#' \dontrun{
#' # read variables from a file in the working directory
#' getVarsEC(filepath=getwd())
#' }

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2019-06-07)
#     partially adapted from eddy4R.base::def.hdf5.extr() authored by David Durden
##############################################################################################

getVarsEC <- function(filepath) {
  
  listObj <- base::try(rhdf5::h5ls(filepath), silent=T)
    
  if(class(listObj)=="try-error") {
    stop(paste("\n", paste(filepath, files[1], sep="/"), " could not be read.", sep=""))
    }
    
  listDataObj <- listObj[listObj$otype == "H5I_DATASET",]
  
  listObjSpl <- tidyr::separate(listDataObj, col="group", 
                         into=c(NA, "site", "level", "category", "system", "horvertmi"), 
                         sep="/", fill="right")
  listObjSpl <- tidyr::separate(listObjSpl, col="horvertmi", 
                                into=c("hor", "ver", "tmi"), 
                                sep="_", fill="right") # this doesn't work right for some of the storage terms
  
  return(listDataObj)
  
}
