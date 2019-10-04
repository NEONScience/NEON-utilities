##############################################################################################
#' @title Extract eddy covariance footprint data from HDF5 format

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Create a raster of flux footprint data. Specific to expanded package of eddy covariance data product: DP4.00200.001
#'
#' @param filepath One of: a folder containing NEON EC H5 files, a zip file of DP4.00200.001 data downloaded from the NEON data portal, a folder of DP4.00200.001 data downloaded by the neonUtilities::zipsByProduct() function, or a single NEON EC H5 file. Filepath can only contain files for a single site. [character]

#' @details Given a filepath containing H5 files of expanded package DP4.00200.001 data, extracts flux footprint data and creates a raster.

#' @return A rasterStack object containing all the footprints in the input files, plus one layer (the first in the stack) containing the mean footprint.

#' @examples
#' \dontrun{
#' # To run the function on a zip file downloaded from the NEON data portal:
#' ftprnt <- footRaster(filepath="~/NEON_eddy-flux.zip")
#' }

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2019-07-04)
##############################################################################################

footRaster <- function(filepath) {
  
  # first check for raster package
  if(!requireNamespace("raster", quietly=T)) {
    stop("Package raster is required for this function to work. Install and re-try.")
  }
  
  # get list of files, unzipping if necessary
  if(substring(filepath, nchar(filepath)-3, nchar(filepath))==".zip") {
    outpath <- gsub(".zip", "", filepath)
    if(!dir.exists(outpath)) {
      dir.create(outpath)
    }
    utils::unzip(filepath, exdir=outpath)
    filepath <- outpath
  }
  
  # allow for a single H5 file
  if(substring(filepath, nchar(filepath)-2, nchar(filepath))==".h5") {
    files <- unlist(strsplit(filepath, split="/", 
                             fixed=T))[length(unlist(strsplit(filepath, 
                                                              split="/", fixed=T)))]
    filepath <- paste0(unlist(strsplit(filepath, split="/", 
                                fixed=T))[1:I(length(unlist(strsplit(filepath, 
                                                                   split="/", fixed=T)))-1)],
                       collapse="/")
  } else {
    files <- list.files(filepath, recursive=T)
  }
  
  # unzip files if necessary
  if(length(grep(".zip", files))==length(files)) {
    for(i in 1:length(files)) {
      utils::unzip(paste(filepath, files[i], sep="/"), exdir=filepath)
    }
    files <- list.files(filepath, recursive=T)
  }
  
  files <- files[grep(".h5", files)]
  
  # make empty, named list for the footprint grids
  gridList <- vector("list", length(files))
  names(gridList) <- substring(files, 1, nchar(files)-3)
  
  # set up progress bar
  writeLines(paste0("Extracting data"))
  pb <- utils::txtProgressBar(style=3)
  utils::setTxtProgressBar(pb, 0)

  # extract footprint data from each file
  for(i in 1:length(files)) {
    
    listObj <- base::try(rhdf5::h5ls(paste(filepath, files[i], sep="/")), silent=T)
    
    if(class(listObj)=="try-error") {
      stop(paste("\n", paste(filepath, files[i], sep="/"), " could not be read.", sep=""))
    }
    
    listDataObj <- listObj[listObj$otype == "H5I_DATASET",]
    listDataName <- base::paste(listDataObj$group, listDataObj$name, sep = "/")
    
    # filter by variable/level selections
    levelInd <- grep("dp04", listDataName)
    
    # get only the footprint grid data
    if(length(grep("foot/grid", listDataName))==0) {
      stop("No footprint data available.")
    } else {
      gridInd <- grep("foot/grid", listDataName)
    }
    
    ind <- intersect(levelInd, gridInd)
    
    # check that you haven't filtered to nothing
    if(length(ind)==0) {
      stop("No footprint data available.")
    }
    
    listDataName <- listDataName[ind]
    
    gridList[[i]] <- base::lapply(listDataName, rhdf5::h5read, 
                                 file=paste(filepath, files[i], sep="/"), read.attributes=T)
    base::names(gridList[[i]]) <- substring(listDataName, 2, nchar(listDataName))
    
    utils::setTxtProgressBar(pb, i/length(files))
    
  }
  close(pb)
 
  allGrids <- unlist(gridList, recursive=F)
  
  # check that data come from only one site
  site <- unique(substring(names(allGrids), 10, 13))
  if(length(site)>1) {
    stop(paste(filepath, " contains files from more than one site.", sep=""))
  }
  
  rasterList <- lapply(allGrids, raster::raster)
  masterRaster <- raster::stack(rasterList)
  summaryRaster <- raster::mean(masterRaster, na.rm=T)
  masterRaster <- raster::addLayer(summaryRaster, masterRaster)
  names(masterRaster)[1] <- paste(site, "summary")
  
  return(masterRaster)
  
}
