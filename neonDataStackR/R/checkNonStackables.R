##############################################################################################
#' @title Check for data that is not stackable

#' @author
#' Christine Laney \email{claney@battelleecology.org}

#' @description
#' Some data products deliver data files in formats that cannot easily be 'stacked', such as HDF5, TIF, LAZ.
#' This function checks whether the user is trying to stack incompatible data files.

#' @param dpID The data product ID
#' @param package Whether the data package is basic or expanded
#' @return Nothing

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Christine Laney (2018-03-06)
##############################################################################################

checkNonstackables <- function(dpID, package='basic'){
  if(substr(dpID, 5, 5) == "3"){
    stop("This is an AOP data product, files cannot be stacked. Use byFileAOP() instead.")
  }
  if(dpID == "DP4.00200.001"){
    stop("This eddy covariance data product is in HDF5 format and cannot be stacked.")
  }
  if(dpID == "DP1.10017.001" && package != 'basic'){
    warning("Digital hemispheric photos (in NEF format) cannot be stacked; only the CSV metadata files will be stacked.")
  }
  return()
}
