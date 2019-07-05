##############################################################################################
#' @title Extract list of eddy covariance tables from HDF5 files

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Extracts a list of table metadata from a single HDF5 file. Specific to eddy covariance data product: DP4.00200.001. Can inform inputs to stackEddy(); variables listed in 'name' are available inputs to the 'var' parameter in stackEddy().
#'
#' @param filepath The folder containing the H5 file [character]

#' @return A data frame of the metadata for each data table in the HDF5 file

#' @examples
#' \dontrun{
#' # read variables from a file in a hypothetical filepath
#' ec.vars <- getVarsEddy(filepath='/data/NEON.D19.BONA.DP4.00200.001.nsae.2017-12.basic.h5')
#' }

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2019-06-07)
#     partially adapted from eddy4R.base::def.hdf5.extr() authored by David Durden
##############################################################################################

getVarsEddy <- function(filepath) {
  
  listObj <- base::try(rhdf5::h5ls(filepath), silent=T)
    
  if(class(listObj)=="try-error") {
    stop(paste(filepath, " could not be read.", sep=""))
    }
    
  listDataObj <- listObj[listObj$otype == "H5I_DATASET",]
  
  listObjSpl <- tidyr::separate(listDataObj, col="group", 
                         into=c(NA, "site", "level", "category", "system", "horvertmi"), 
                         sep="/", fill="right")
  listObjSpl <- tidyr::separate(listObjSpl, col="horvertmi", 
                                into=c("hor", "ver", "tmi"), 
                                sep="_", fill="left")
  listObjSpl$oth[which(is.na(suppressWarnings(as.numeric(listObjSpl$ver))))] <- 
    listObjSpl$ver[which(is.na(suppressWarnings(as.numeric(listObjSpl$ver))))]
  listObjSpl$ver[which(is.na(suppressWarnings(as.numeric(listObjSpl$ver))))] <- NA
  
  return(listObjSpl)
  
}
