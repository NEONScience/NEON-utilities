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
  
  # first check for hdf5r package
  if(!requireNamespace("hdf5r", quietly=T)) {
    stop("Package hdf5r is required for this function to work. Install with install.packages('hdf5r').")
  }
  
  h5file <- base::try(hdf5r::H5File$new(filepath, mode="r"), silent=T)
  if(inherits(h5file, "try-error")) {
    stop(paste(filepath, " could not be read.", sep=""))
  }
  on.exit(h5file$close_all())

  listObj <- base::try(h5file$ls(recursive=TRUE), silent=T)
    
  if(inherits(listObj, "try-error")) {
    stop(paste(filepath, " could not be read.", sep=""))
    }
    
  listDataObj <- listObj[listObj$obj_type == "H5I_DATASET",]
  groups <- dirname(listDataObj$name)
  groups <- ifelse(groups == ".", "/", paste0("/", groups))
  listDataObj <- data.frame(group=groups,
                           name=basename(listDataObj$name),
                           otype=as.character(listDataObj$obj_type),
                           dclass=sub("^H5T_", "", listDataObj$dataset.type_class),
                           dim=as.character(listDataObj$dataset.dims),
                           row.names=which(listObj$obj_type == "H5I_DATASET") - 1L,
                           stringsAsFactors=FALSE)
  
  listObjSpl <- tidyr::separate(listDataObj, col="group", 
                         into=c(NA, "site", "level", "category", "system", 
                                "horvertmi", "subsys"), 
                         sep="/", fill="right")
  listObjSpl <- tidyr::separate(listObjSpl, col="horvertmi", 
                                into=c("hor", "ver", "tmi"), 
                                sep="_", fill="left")
  listObjSpl$oth[which(is.na(suppressWarnings(as.numeric(listObjSpl$ver))))] <- 
    listObjSpl$ver[which(is.na(suppressWarnings(as.numeric(listObjSpl$ver))))]
  listObjSpl$ver[which(is.na(suppressWarnings(as.numeric(listObjSpl$ver))))] <- NA
  
  return(listObjSpl)
  
}
